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

# libraries
library(data.table)
library(lubridate)
library(suncalc)
library(GRNNs)

setwd("C:/Users/anaay970/Documents/Projects/2025_EcoTwin/WP3/Climate data downscaling/Final") # working directory

Sys.setenv(tz="Etc/GMT-1")

# source functions
source("script1_temporal_downscaling_grnn_functions.R") 
# prepare_data(dt, lat, lon, tz = "Etc/GMT-1")
# split_data(dt, years_train, years_test, inputs, outputs) 
# fit_scaler(dt, scale_vars)
# apply_scaler(dt, scaler) 
# inverse_scaler(dt, scaler)
# findSpreadRdist_mod(x,y,k,fun,scale=TRUE,spread_seq) 
source("script1_performance_and_figures.R")
# statistical_performance_metrics(obs, sim)
# scatter_fig(dt, var, cal_period, val_period)
# plot_fig(dt, var, period)

## pre-processing
# meteorological station: malma island in lake erken, sweden (lat=59.83917, lon=18.629333, tz=Etc/GMT-1)
load(file.path("df_erken_met_hourly.RData")) # hourly data: at [degreeC], rh [%], ws [m/s], pr [mm/h], swrin [W/m2], lwrin [W/m2] (1988-2025)
lat <- 59.83917 # degree
lon <- 18.629333 # degree
tz <- "Etc/GMT-1"

dt <- prepare_data(df_erken_met_hourly, lat, lon, tz) 
dt$year <- format(dt$date, "%Y") 
unique(dt$year)
# "1995" "1996" "1997" "1998" "1999" "2000" "2001" "2002" "2003" "2004" "2005" "2006" "2007" "2008" "2009" 
# "2010" "2011" "2012" "2013" "2014" "2015" "2016" "2017" "2018" "2019" "2020" "2021" "2022" "2023" "2024"
dt[, .(n_doy = uniqueN(doy)), by = year]
# 1995 103
# 1996 122
# 1997 164
# 1998 214
# 1999 229
# 2000 304
# 2001 278
# 2002 292
# 2003 236
# 2004 341
# 2005 323
# 2006 309
# 2007 320
# 2008 323
# 2009 329
# 2010 322
# 2011 300
# 2012 290
# 2013 291
# 2014 331
# 2015 317
# 2016 341
# 2017 323
# 2018 306
# 2019 302
# 2020 314
# 2021 324
# 2022 336
# 2023 345
# 2024 207

## temporal disaggregation of climate data: grnn
# training: 2016-2021 (6 years)
years_train <- seq(2016, 2021)
# testing: 2022-2023 (2 years)
years_test <- seq(2021, 2022)

# inputs
inputs <- c("at_mean", "at_min", "at_max", "ws_mean", "rh_mean", "swrin_mean", "pr_total", 
            "doy_sin", "doy_cos", "hour_sin", "hour_cos", 
            "daylight_flag", "time_since_sunrise", "time_until_sunset", "hour_angle_rad")
# outputs
outputs <- c("at", "ws", "rh", "swrin", "pr")

# what to scale (grnn is distance-based)
scale_vars_x <- c("at_mean", "at_min", "at_max", "ws_mean", "rh_mean", "swrin_mean", "pr_total", "time_since_sunrise", "time_until_sunset")
no_scale_vars_x <- c("doy_sin", "doy_cos", "hour_sin", "hour_cos", "daylight_flag", "hour_angle_rad") # cyclic/bounded
scale_vars_y <- c("at", "ws", "rh", "swrin", "pr")

# split
split <- split_data(dt, years_train, years_test, inputs, outputs)

# scaler 
scaler_x_train <- fit_scaler(split$x_train, scale_vars_x)
scaler_y_train <- fit_scaler(split$y_train, scale_vars_y)

# apply scaler
x_train_s <- apply_scaler(split$x_train, scaler_x_train)
x_test_s <- apply_scaler(split$x_test, scaler_x_train)
y_train_s <- apply_scaler(split$y_train, scaler_y_train)

# GRNNs (General Regression Neural Networks) package
# optimize spread with findSpreadRdist_mod() over seq(0.05, 2, 0.05)
start <- Sys.time()
best_spread <- findSpreadRdist_mod(x_train_s, y_train_s, 10, "euclidean", scale=FALSE, seq(0.05, 2, 0.05))
end <- Sys.time()
end - start # time difference of 6.179833 hours
# best spread: (0.2, 0.2, 0.25, 0.30, 0.5)
save(best_spread, x_train_s, y_train_s, scaler_x_train, scaler_y_train, file=file.path("script1", paste0("script1_grnn_train_bestspread_step1.RData")))

# optimize spread with findSpreadRdist_mod() over seq(0.15, 0.55, 0.01)
start <- Sys.time()
best_spread <- findSpreadRdist_mod(x_train_s, y_train_s, 10, "euclidean", scale=FALSE, seq(0.15, 0.55, 0.01))
end <- Sys.time()
end - start # time difference of 6.744034 hours
# best spread: (0.20, 0.22, 0.23, 0.28, 0.5)
save(best_spread, x_train_s, y_train_s, scaler_x_train, scaler_y_train, file=file.path("script1", paste0("script1_grnn_train_bestspread_step2.RData")))

# grnn: prediction on training dataset
start <- Sys.time()
y_train_s_pred <- GRNNs::grnn(x_train_s, x_train_s, y_train_s, fun="euclidean", best_spread, scale=FALSE)
end <- Sys.time()
end - start # time difference of 19.29821 mins
save(y_train_s_pred, best_spread, x_train_s, y_train_s, scaler_x_train, scaler_y_train, file=file.path("script1", paste0("script1_grnn_erken_train_pred.RData")))

# grnn: prediction on testing dataset
start <- Sys.time()
y_test_s_pred <- GRNNs::grnn(x_test_s,x_train_s,y_train_s,fun="euclidean",best_spread,scale=FALSE)
end <- Sys.time()
end - start # time difference of 6.565254 mins
save(y_test_s_pred, x_test_s, best_spread,x_train_s, y_train_s, scaler_x_train, scaler_y_train, file=file.path("script1", paste0("script1_grnn_erken_test_pred.RData")))

# inverse scaler (descale)
dt_y_train_s_pred <- to_numeric_dt(y_train_s_pred) # train
y_train_pred <- inverse_scaler(dt_y_train_s_pred, scaler_y_train)
dt_y_test_s_pred <- to_numeric_dt(y_test_s_pred) # test
y_test_pred <- inverse_scaler(dt_y_test_s_pred, scaler_y_train)

## post-processing
dir.create("script1_performance_and_figures_bis")

# training dataset
dt_train_pred <- data.table(datetime=split$dt_train, y_train_pred) 
dt_train_obs <- data.table(datetime=split$dt_train, split$y_train)
dt_train <- merge(dt_train_obs, dt_train_pred, by="datetime", suffixes = c("_obs", "_dis"), all.x=TRUE)
dt_train[, month := month(datetime)] # training dataset
dt_train[, season := fifelse(month %in% c(12, 1, 2), "winter", fifelse(month %in% c(3, 4, 5), "spring", fifelse(month %in% c(6, 7, 8), "summer", "autumn")))]
# testing dataset
dt_test_pred <- data.table(datetime=split$dt_test, y_test_pred) 
dt_test_obs <- data.table(datetime=split$dt_test, split$y_test)
dt_test <- merge(dt_test_obs, dt_test_pred, by="datetime", suffixes = c("_obs", "_dis"), all.x=TRUE)
dt_test[, month := month(datetime)] # testing dataset
dt_test[, season := fifelse(month %in% c(12, 1, 2), "winter", fifelse(month %in% c(3, 4, 5), "spring", fifelse(month %in% c(6, 7, 8), "summer", "autumn")))]

vars <- c("at", "rh", "swrin", "ws", "pr")
# performance metrics
l_pm <- lapply(vars, function(v) {
  obs_col <- paste0(v, "_obs")
  dis_col <- paste0(v, "_dis")
  list(variable = v,
       train = statistical_performance_metrics(dt_train[, get(obs_col)], dt_train[, get(dis_col)]),
       test = statistical_performance_metrics(dt_test[, get(obs_col)], dt_test[, get(dis_col)]))
  })
dt_pm <- rbindlist(lapply(l_pm, function(x) {
  data.table(var = x$variable, period = c("train", "test"), season = "all", rbind(x$train, x$test))}), fill = TRUE)

l_pm_season <- list()
for (v in vars) {
  obs_col <- paste0(v, "_obs")
  dis_col <- paste0(v, "_dis")
  for (s in unique(dt_train$season)) {
    dt_train_season <- dt_train[season == s]
    pm_train <- statistical_performance_metrics(dt_train_season[[obs_col]], dt_train_season[[dis_col]])
    dt_test_season <- dt_test[season == s]
    l_pm_season[[length(l_pm_season) + 1]] <- data.table(var = v, period = "train", season = s, pm_train)
    pm_test <- statistical_performance_metrics(dt_test_season[[obs_col]], dt_test_season[[dis_col]])
    l_pm_season[[length(l_pm_season) + 1]] <- data.table(var = v, period = "test", season = s, pm_test)
    }
}
dt_pm_season <- rbindlist(l_pm_season, fill = TRUE)
dt_pm_all <- rbindlist(list(dt_pm, dt_pm_season), fill = TRUE)
fwrite(dt_pm_all, file.path("script1_performance_and_figures", "script1_performance_metrics.csv"))
# figures
# scatter
for (var in vars) {
  filename <- file.path("script1_performance_and_figures", paste0("script1_plot_", var, ".png"))
  png(filename, width = 1000, height = 550, res = 120)
  scatter_fig(dt_train, dt_test, var)
  dev.off()
}
# plot
months_test <- seq(floor_date(min(dt_test$datetime, na.rm = TRUE), "month"), floor_date(max(dt_test$datetime, na.rm = TRUE), "month"), by = "month")
for (var in vars) {
  for (i in seq_along(months_test)) {
    start_date <- as.POSIXct(months_test[i])
    if (i < length(months_test)) {
      end_date <- months_test[i + 1] - days(1)
    } else {
      end_date <- tail(dt_test$datetime, 1)
    }
    filename <- file.path("script1_performance_and_figures", paste0("script1_plot_", var, "_", format(start_date, "%Y_%m"), ".png"))
    png(filename, width = 1600, height = 600, res = 120)
    plot_fig(dt_test, var, c(start_date, end_date))
    dev.off()
  }
}
