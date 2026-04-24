#################################################################################################
# The Teddy tool (an adaptation of MATLAB to R)

# Zabel, F. and Poschlod, B.: The Teddy tool v1.1 (2023). 
# Temporal disaggregation of daily climate model data for climate impact analysis, 
# Geosci. Model Dev., 16, 5383–5399, https://doi.org/10.5194/gmd-16-5383-2023.
# https://github.com/flozabel/Teddy

# Matching each model day with the most similar meteorological day from high-resolution reference 
# dataset within a user-defined day-of-year window and selecting the best match based on ranked 
# absolute errors across variables, while also accounting for precipitation state and inter-day 
# wet/dry persistence.Once the most similar reference day is identified, its observed hourly diurnal 
# cycles are extracted, normalized to remove absolute magnitude, and then rescaled to match the 
# daily mean or total values.
#################################################################################################

# Ana I. Ayala (isabel.ayala.zamora@ebc.uu.se)
# 2026-04-14

# libraries
library(data.table)
library(lubridate)
library(gotmtools)

## helper functions
# check if a given year is a leap year (returns TRUE/FALSE)
leap_year <- function(y) {
  (y %% 4 == 0 & y %% 100 != 0) | (y %% 400 == 0)
}
# generate a moving day-of-year window around a given doy, with wrap-around handling for year boundaries and leap years
doy_window_fun <- function(doy, year, window = 11) {
  days_in_year <- ifelse(leap_year(year), 366, 365)
  
  seq_days <- (doy - window):(doy + window)
  seq_days[seq_days < 1] <- seq_days[seq_days < 1] + days_in_year
  seq_days[seq_days > days_in_year] <- seq_days[seq_days > days_in_year] - days_in_year
  
  return(seq_days)
}
# create a 3-class precipitation flag string (previous, today, next)
# each value is binarized: 0 if < 1, otherwise 1
get_precip_class <- function(p_prev, p_today, p_next) {
  bin <- function(x) ifelse(x < 1, 0, 1)
  paste0(bin(p_prev), bin(p_today), bin(p_next))
}
# rank values while handling ties by assigning the minimum rank to tied values
rank_with_ties <- function(x) {
  rank(x, ties.method = "min", na.last = "keep")
}
# compute normalized hourly factors
# if precipitation: normalize by sum (distribution over total)
# if non-precipitation: normalize by mean (relative scaling)
get_hourly_factors <- function(hourly, is_precip = FALSE) {
  if (is_precip) {
    s <- sum(hourly)
    if (s == 0) return(rep(0, length(hourly)))
    return(hourly / s)
  } else {
    m <- mean(hourly)
    if (m == 0) return(rep(0, length(hourly)))
    return(hourly / m)
  }
}

## hourly meteorological data]
hourly_met_data <- function(dt) {
  dt <- as.data.table(dt) # convert to data.table 
  dt_h <- dt[, .(datetime, at, rh, swrin, ws, pr)] # extract at, rh, swrin, ws, pr
  setnames(dt_h, c("at","rh","swrin","ws","pr"),  c("temp","humidity","radiation","wind","precip")) # rename variables
  dt_h <- na.omit(dt_h, cols = c("temp","humidity","radiation","wind","precip")) # remove na
  dt_h[, date := as.Date(datetime, tz = "")] # extract date
  dt_h <- dt_h[, if (.N == 24) .SD, by = date] # keep only full days (24 obs/day)
  return(dt_h)
}

## daily summaries
daily_met_data_aggregation <- function(dt_h) {
  dt_h[, date := as.Date(datetime, tz="")]
  dt_d <- dt_h[, .(temp = mean(temp), 
                   precip = sum(precip), 
                   humidity = mean(humidity), 
                   radiation = mean(radiation),
                   wind = mean(wind), 
                   tmin = min(temp), 
                   tmax = max(temp)), 
               by = date]
  dt_d[, `:=`(doy = yday(date), year = year(date))]
  dt_d[, precip_prev := shift(precip, 1, fill = NA)]
  dt_d[, precip_next := shift(precip, -1, fill = NA)]
  dt_d[, seq_class := mapply(get_precip_class, precip_prev, precip, precip_next)]
  return(dt_d)
}

## find best matching day in reference dataset
find_best_day <- function(target_row, ref_daily, window = 11) {
  
  doy_seq <- doy_window_fun(target_row$doy, target_row$year, window)
  candidates <- ref_daily[doy %in% doy_seq]
  candidates <- candidates[seq_class == target_row$seq_class]
  
  if (nrow(candidates) == 0) return(NULL)
  candidates[, `:=`(e_temp = abs(temp - target_row$temp), 
                    e_prec = abs(precip - target_row$precip),
                    e_hum = abs(humidity - target_row$humidity),
                    e_rad = abs(radiation - target_row$radiation),
                    e_wind = abs(wind - target_row$wind))]
  
  candidates[, `:=`(r1 = rank_with_ties(e_temp),
                    r2 = rank_with_ties(e_prec), 
                    r3 = rank_with_ties(e_hum),
                    r4 = rank_with_ties(e_rad),
                    r5 = rank_with_ties(e_wind))]
  
  candidates[, rank_sum := r1 + r2 + r3 + r4 + r5]
  
  return(candidates[which.min(rank_sum)])
}

## scaling functions
scale_temperature <- function(hourly_temp, tmin, tmax, tmean_target) {
  t_norm <- (hourly_temp - min(hourly_temp)) / (max(hourly_temp) - min(hourly_temp))
  t_scaled <- tmin + t_norm * (tmax - tmin)
  correction <- tmean_target - mean(t_scaled)
  t_scaled <- t_scaled + correction
  return(t_scaled)
}

scale_humidity <- function(rh, target_mean) {
  rh <- pmin(rh, 100)
  correction <- target_mean - mean(rh)
  rh <- rh + correction
  rh <- pmax(pmin(rh, 100), 0)
  return(rh)
}

limit_radiation <- function(rad_hourly, rad_potential, target_mean) {
  rad_hourly <- pmin(rad_hourly, rad_potential)
  factor <- target_mean / mean(rad_hourly)
  rad_hourly <- rad_hourly * factor
  return(rad_hourly)
}

## precipitation fallback model
fit_precip_model <- function(hourly) {
  hourly[, date := as.Date(datetime, tz="")]
  daily <- hourly[, .(total = sum(precip), duration = sum(precip > 0)), by = date]
  return(lm(duration ~ total, data = daily))
}

generate_precip_event <- function(total_precip, model) {
  duration <- predict(model, data.frame(total = total_precip))
  duration <- max(1, round(duration))
  precip <- rep(0, 24)
  start <- sample(0:23, 1)
  idx <- start:min(start + duration - 1, 23)
  precip[idx] <- total_precip / length(idx)
  return(precip)
}

## compute rank
compute_rank <- function(candidates, target, use_precip = TRUE) {
  
  candidates[, `:=`(e_temp = abs(temp - target$temp),
                    e_hum  = abs(humidity - target$humidity),
                    e_rad  = abs(radiation - target$radiation),
                    e_wind = abs(wind - target$wind))]
  
  if (use_precip) {
    candidates[, e_prec := abs(precip - target$precip)]
  }
  
  candidates[, `:=`(r1 = rank_with_ties(e_temp),
                    r3 = rank_with_ties(e_hum),
                    r4 = rank_with_ties(e_rad),
                    r5 = rank_with_ties(e_wind))]
  
  if (use_precip) {
    candidates[, r2 := rank_with_ties(e_prec)]
    candidates[, rank_sum := r1 + r2 + r3 + r4 + r5]
  } else {
    candidates[, rank_sum := r1 + r3 + r4 + r5]
  }
  
  candidates
}

## hourly disaggregation pipeline
hourly_disaggregation_pipeline <- function(target_daily, ref_daily, ref_hourly, rad_pot = NULL) {
  
  precip_model <- fit_precip_model(ref_hourly)
  
  ref_daily[, `:=`(seq_class_2bwd = substr(seq_class, 1, 2),
                   seq_class_2fwd = substr(seq_class, 2, 3),
                   seq_class_central = substr(seq_class, 2, 2))]
  
  results <- vector("list", nrow(target_daily))
  
  for (i in 1:nrow(target_daily)) {
    
    target <- target_daily[i, ]
    best <- NULL
    
    # window 11 days, full 3-day sequence 
    best <- find_best_day(target, ref_daily, window = 11)
    
    # window 50 days, full 3-day sequence 
    if (is.null(best)) {
      best <- find_best_day(target, ref_daily, window = 50)
    }
    
    # window 50 days, 2-day sequence (bwd + fwd combined) 
    if (is.null(best)) {
      doy_window <- doy_window_fun(target$doy, target$year, 50)
      
      target_seq_class_2bwd <- substr(target$seq_class, 1, 2)
      target_seq_class_2fwd <- substr(target$seq_class, 2, 3)
      
      candidates <- ref_daily[
        doy %in% doy_window &
          (
            seq_class_2bwd == target_seq_class_2bwd |
              seq_class_2fwd == target_seq_class_2fwd
          )
      ]
      
      if (nrow(candidates) > 0) {
        candidates <- compute_rank(candidates, target, use_precip = TRUE)
        best <- candidates[which.min(rank_sum)]
      }
    }
    
    # window 50 days, central only 
    if (is.null(best)) {
      doy_window <- doy_window_fun(target$doy, target$year, 50)
      
      target_seq_class_central <- substr(target$seq_class, 2, 2)
      
      candidates <- ref_daily[
        doy %in% doy_window &
          seq_class_central == target_seq_class_central
      ]
      
      if (nrow(candidates) > 0) {
        candidates <- compute_rank(candidates, target, use_precip = TRUE)
        best <- candidates[which.min(rank_sum)]
      }
    }
    
    # fallback: ignore precipitation 
    if (is.null(best)) {
      doy_window <- doy_window_fun(target$doy, target$year, 50)
      
      candidates <- ref_daily[doy %in% doy_window]
      
      candidates <- compute_rank(candidates, target, use_precip = FALSE)
      best <- candidates[which.min(rank_sum)]
    }
    
    # extract hourly reference profile 
    ref_day_hours <- ref_hourly[date == best$date]
    
    time_index <- seq.POSIXt(from = as.POSIXct(paste(target$date, "00:00:00")), by = "hour", length.out = 24)
    
    f_temp <- get_hourly_factors(ref_day_hours$temp)
    f_hum  <- get_hourly_factors(ref_day_hours$humidity)
    f_rad  <- get_hourly_factors(ref_day_hours$radiation)
    f_wind <- get_hourly_factors(ref_day_hours$wind)
    
    temp_h <- target$temp * f_temp
    hum_h  <- target$humidity * f_hum
    rad_h  <- target$radiation * f_rad
    wind_h <- target$wind * f_wind
    
    temp_h <- scale_temperature(temp_h, target$tmin, target$tmax, target$temp)
    hum_h  <- scale_humidity(hum_h, target$humidity)
    
    rad_pot_h <- calc_swr(time = time_index, lat = lat, lon = lon, cloud = 0)
    
    if (!is.null(rad_pot)) {
      rad_h <- limit_radiation(rad_h, rad_pot_h, target$radiation)
    }
    
    # precipitation handling 
    if (target$precip == 0) {
      prec_h <- rep(0, 24)
    } else {
      if (sum(ref_day_hours$precip) > 0) {
        f_prec <- get_hourly_factors(ref_day_hours$precip, TRUE)
        prec_h <- target$precip * f_prec
      } else {
        prec_h <- generate_precip_event(target$precip, precip_model)
      }
    }
    
    results[[i]] <- data.table(datetime = time_index, temp = temp_h, precip = prec_h, humidity = hum_h, radiation = rad_h, wind = wind_h)
  }
  
  return(rbindlist(results))
}
