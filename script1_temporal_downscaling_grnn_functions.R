#################################################################################################
# GRNN: General Regression Neuronal Network 

# Ayala, A. I., Moras, S., and Pierson, D. C. (2020). 
# Simulations of future changes in thermal structure of Lake Erken: proof of concept for ISIMIP2b lake sector local simulation strategy, 
# Hydrol. Earth Syst. Sci., 24, 3311–3330, https://doi.org/10.5194/hess-24-3311-2020.

# Targets: hourly air temperature, relative humidity, wind speed, incoming short-wave radiation, precipitation
# Features: max daily , min daily and mean daily air temperature, 
#           mean daily relative humidity, wind speed and incoming short-wave radiation,
#           total daily precipitation, 
#           sin(hour), cos(hour), sin(doy), cos(doy),
#           dayligh flat (0: night, 1: day), time since sunrise, time until sunset, hour angle 
#################################################################################################

# Ana I. Ayala (isabel.ayala.zamora@ebc.uu.se)
# 2026-04-14

prepare_data <- function(dt, lat, lon, tz = "Etc/GMT-1") {
  setDT(dt)
  
  dt_h <- dt[, .(datetime, at, rh, swrin, ws, pr)]
  dt_h <- na.omit(dt_h, cols = c("at","rh","swrin","ws","pr")) # remove na
  dt_h[, `:=`(date = as.Date(datetime, tz = tz), # date, hour, doy
              hour = hour(datetime),
              doy  = yday(datetime))]
  dt_h <- dt_h[, if (.N == 24) .SD, by = date] # keep full days (24 values per day)
  dt_h[, `:=`(hour_sin = sin(2 * pi * hour / 24), # doy/ hour sin/cos transformations
              hour_cos = cos(2 * pi * hour / 24),
              doy_sin  = sin(2 * pi * doy / 365),
              doy_cos  = cos(2 * pi * doy / 365))]
  dt_d <- dt_h[, .(at_mean = mean(at), at_min  = min(at), at_max  = max(at), # daily summaries
                   rh_mean = mean(rh),
                   swrin_mean = mean(swrin),
                   ws_mean = mean(ws),
                   pr_total = sum(pr)),
               by = date]
  dt_h <- merge(dt_h, dt_d, by = "date", all.x = TRUE) # merge hourly and daily
  
  df_sun <- getSunlightTimes(date = unique(dt_h$date), lat = lat, lon = lon, tz = tz) # sunlight times
  dt_sun <- as.data.table(df_sun[, c("date","solarNoon","sunrise","sunset")])
  dt_h <- merge(dt_h, dt_sun, by = "date", all.x = TRUE) 
  # solar features
  dt_h[, `:=`(daylight_flag = ifelse(datetime >= sunrise & datetime <= sunset, 1, 0), # daylight flag: night 0, day 1
              time_since_sunrise = as.numeric(difftime(datetime, sunrise, units = "hours")), # time since sunrise
              time_until_sunset  = as.numeric(difftime(sunset, datetime, units = "hours")), # time until sunset
              hour_angle_deg = as.numeric(difftime(datetime, solarNoon, units = "hours")) * 15)] # hour angle (degree)
  dt_h[time_since_sunrise < 0, time_since_sunrise := 24 + time_since_sunrise] # adjust negative values
  dt_h[time_until_sunset < 0, time_until_sunset := 24 + time_until_sunset]
  
  dt_h[, hour_angle_rad := hour_angle_deg * pi / 180] # to rad
  
  return(dt_h)
}

# split
split_data <- function(dt, years_train, years_test, inputs, outputs) {
  list(x_train = dt[year %in% years_train, ..inputs],
       y_train = dt[year %in% years_train, ..outputs],
       dt_train = dt[year %in% years_train, datetime],
       x_test = dt[year %in% years_test, ..inputs],
       y_test = dt[year %in% years_test, ..outputs],
       dt_test = dt[year %in% years_test, datetime])
}

# fit scaler 
fit_scaler <- function(dt, scale_vars) {
  scaled <- scale(dt[, ..scale_vars])
  list(mean = attr(scaled, "scaled:center"),
       sd = attr(scaled, "scaled:scale"),
       vars = scale_vars)
}

# apply scaler
apply_scaler <- function(dt, scaler) {
  dt_scaled <- copy(dt)
  dt_scaled[, (scaler$vars) := Map(function(x, m, s) (x - m) / s, .SD, scaler$mean, scaler$sd), .SDcols = scaler$vars]
}

# apply descaler 
inverse_scaler <- function(dt, scaler) {
  dt_scaled <- copy(dt)
  dt_scaled[, (scaler$vars) := Map(function(x, m, s) x * s + m, .SD, scaler$mean, scaler$sd), .SDcols = scaler$vars]
}

# from matrix to data.table (numeric)
to_numeric_dt <- function(mat) {
  dt <- as.data.table(mat)
  dt[, (names(dt)) := lapply(.SD, as.numeric)]
  return(dt)
}

# GRNNs (General Regression Neural Networks) package
# https://github.com/cran/GRNNs
# Find best spread
# in findSpread(), when fun="euclidean" and scale="FALSE", the function findSpreadRdist() is called
# findSpreadRdist() has been modified to specify the number of spreads as an argument
findSpreadRdist_mod <- function(x,y,k,fun,scale=TRUE,spread_seq) {
  x<-as.data.frame(x)
  y<-as.data.frame(y)
  spread_all<-NULL
  rmse_all<-NULL
  cvr<-cvTools::cvFolds(nrow(x), K = k) # Generate the index of random k folds for the data
  subcvr<-cvr$subsets # Assign the index to subsvr
  for (spread in spread_seq) {
    predict<-NULL
    #print(spread)
    for (i in 1:k) {
      train.x <- x[subcvr[cvr$which != i],,drop=FALSE] # k-1 folds of physiognomic data used for training
      validation.x <- x[subcvr[cvr$which == i],,drop=FALSE] # 1 folds of physiognomic data used as validation data
      train.y <- y[subcvr[cvr$which != i],,drop=FALSE] # k-1 folds of meteorological data used for training
      validation.y <- y[subcvr[cvr$which == i],,drop=FALSE] # 1 folds of meteorological data used as validation data
      if (scale==TRUE){
        train.x<-scales::rescale(as.matrix(train.x), to=c(-1,1))
        validation.x<-scales::rescale(as.matrix(validation.x), to=c(-1,1))
      }
      w.input<-rdist::cdist(train.x,validation.x,metric = fun) # Calculating the distance between the trainning data and the validation data
      b.input<-w.input*sqrt(-log(.5))/spread
      a1<-exp(-b.input^2) # Use Gaussian kernel function to transform the input data
      weight_all<-colSums(a1)
      for (h in 1:ncol(a1)) {if (weight_all[h]==0) {weight_all[h]<-1}}
      pred_it<-(t(a1) %*% as.matrix(train.y))/weight_all # Calculating the prediction values
      row.names(pred_it) <- row.names(validation.y)
      predict<-rbind(predict,pred_it)
    }
    predict1<-predict[order(rownames(predict)),]
    y1<-y[order(rownames(y)),]
    res<-predict1-y1  # Calculating the errors between the prediction values and the validation values
    res<-as.data.frame(res)
    rmse<-sqrt(colSums(res^2)/nrow(res)) # root-mean-square error
    spread_all<-rbind(spread_all,spread)
    rmse_all<-rbind(rmse_all,rmse)
  }
  best.spread<-numeric(ncol(y))
  test<-cbind(as.matrix(spread_all),rmse_all) # Combine the spread and rmse_all values together
  for (m in 1:ncol(y)) {
    best_num<-test[which(test[,m+1]==min(test[,m+1])),1]
    best.spread[m]<-as.matrix(best_num[1]) # Find the optimal spread with minimum value of rmse
  }
  best.spread
}