#################################################################################################
# MELODIST - MEteoroLOgical observation time series DISaggregation Tool

# Förster, K., Hanzer, F., Winter, B., Marke, T., and Strasser, U.(2016).
# An open-source MEteoroLOgical observation time series DISaggregation Tool (MELODIST v0.1.1), 
# Geosci. Model Dev., 9, 2315–2333, https://doi.org/10.5194/gmd-9-2315-2016.
# https://github.com/kristianfoerster/melodist

# Air temperature: method="sine_min_max", min_max_time="sun_loc_shif"
# Relative humidity: method="dewpoint_regression"
# Wind speed: method="cosine"
# Incoming short-wave radiation: method="pot_rad"
# Precipitation: method="cascade"
#################################################################################################

# Ana I. Ayala (isabel.ayala.zamora@ebc.uu.se)
# 2026-04-14

# libraries
library(data.table)
library(lubridate)
library(suncalc)
library(reticulate)

init_environment <- function(config) {
  use_python(config$python_path)
  Sys.setenv(tz = config$timezone)
}

## hourly meteorological data
# datetime: [yyyy-mm-dd hh:mm:ss]
# at: air temperature (at 2 m height) [degreeC]
# rh: relative humidity [%]
# swrin: incoming short-wave radiation [W/m2]
# ws: wind speed (at 10 m height) [m/s]
# pr: precipitation (total) [mm/h]
hourly_met_data <- function(dt) {
  dt <- as.data.table(dt) # convert to data.table 
  dt_h <- dt[, .(datetime, at, rh, swrin, ws, pr)] # extract at, rh, swrin, ws, pr
  dt_h <- na.omit(dt_h, cols = c("at","rh","swrin","ws","pr")) # remove na
  dt_h[, date := as.Date(datetime, tz = "")] # extract date
  dt_h <- dt_h[, if (.N == 24) .SD, by = date] # keep only full days (24 obs/day)
  return(dt_h)
}

## daily summaries
# max, min: air temperature
# mean: air temperature [degreeC], relative humidity [%], incoming short-wave radiation [W/m2], wind speed [m/s]
# sum: precipitation [mm/day]
daily_met_data_aggregation <- function(dt_h) {
    dt_d <- dt_h[, .(at_mean = mean(at), at_min = min(at), at_max = max(at),
                     rh_mean = mean(rh),
                     swrin_mean = mean(swrin),      
                     ws_mean = mean(ws),
                     pr_total = sum(pr)), 
                 by = date]
    return(dt_d)
}
  
## sunshine duration 
# length of time between sunrise and sunset
# daily sunshine duration (hour)
daily_sunshine_duration <- function(date, lat, lon, tz) { 
  sun <- getSunlightTimes(date = as.Date(date, tz = tz), lat = lat, lon = lon, tz = tz)
  return(data.table(date = as.Date(date), sunshine = as.numeric(difftime(sun$sunset, sun$sunrise, units = "hours"))))
}
# daily sunshine duration per hour (min)
daily_sunshine_duration_per_hour <- function(date, lat, lon, tz) { 
  
  result <- data.table::rbindlist(lapply(date, function(d) {
    
    sun <- getSunlightTimes(date = as.Date(d), lat = lat, lon = lon, keep = c("sunrise", "sunset"), tz = tz)
    sunrise <- sun$sunrise
    sunset  <- sun$sunset
    
    hours_seq <- seq(from = floor_date(sunrise, "day"),
                     to = floor_date(sunrise, "day") + hours(23),
                     by = "1 hour")
    
    sunshine <- sapply(hours_seq, function(h) {
      hour_start <- h
      hour_end <- h + hours(1)
      
      overlap_start <- max(hour_start, sunrise)
      overlap_end <- min(hour_end, sunset)
      
      if (overlap_end > overlap_start) {
        as.numeric(difftime(overlap_end, overlap_start, units = "mins"))
      } else {
        0
      }
    })
    
    results <- data.table(datetime = hours_seq, sunshine = sunshine)
  }))
  
  return(result)
}

## melodist format
# daily: temp [degreeK], hum [%], glob [W/m2], wind [m/s], precip [mm/day], ssd [h]
# hourly: temp [degreeK], tmin [degreeK], tmax [degreeK], hum [%], glob [W/m2], wind [mm/s], precip [mm/h], ssd [min]
melodist_format <- function(dt_h, dt_d, dt_ssd_h, dt_ssd_d) {
  
  dt_hourly <- merge(dt_h[, .(datetime, at, rh, swrin, ws, pr)], dt_ssd_h, by="datetime", all.x=TRUE)
  dt_daily  <- merge(dt_d, dt_ssd_d, by="date", all.x=TRUE)
  
  dt_hourly[, at := at + 273.15] # degreeC -> degreeK
  dt_daily[, c("at_mean","at_min","at_max") := .(at_mean+273.15, at_min+273.15, at_max+273.15)]
  
  setnames(dt_hourly, # rename columns
           c("datetime","at","rh","swrin","ws","pr","sunshine"),
           c("datetime","temp","hum","glob","wind","precip","ssd"))
  
  setnames(dt_daily, # rename columns
           c("date","at_mean","at_min","at_max","rh_mean","swrin_mean","ws_mean","pr_total","sunshine"),
           c("date","temp","tmin","tmax","hum","glob","wind","precip","ssd"))
  
  return(list(hourly = dt_hourly, daily = dt_daily))
}

## run melodist in python
run_melodist <- function(dt_hourly, dt_daily, lat, lon, config, cal_period) {
  
  assign("dt_hourly", dt_hourly, envir = .GlobalEnv)
  assign("dt_daily",  dt_daily,  envir = .GlobalEnv)
  assign("lat", lat, envir = .GlobalEnv)
  assign("lon", lon, envir = .GlobalEnv)
  assign("tz",  config$timezone_py, envir = .GlobalEnv)
  assign("cal", cal_period, envir = .GlobalEnv)
  
  # python
  py_run_string("
import pandas as pd
import melodist

latitude = r.lat
longitude = r.lon
timezone = r.tz
  
df_daily = pd.DataFrame(r.dt_daily) 
df_daily['date'] = pd.to_datetime(df_daily['date'])
df_daily.set_index('date', inplace=True)

station = melodist.Station(lon=longitude, lat=latitude, timezone=timezone, data_daily=df_daily,) # station object

calibration_period = slice(pd.to_datetime(r.cal[0]), pd.to_datetime(r.cal[1]))

df_hourly = pd.DataFrame(r.dt_hourly)
df_hourly['datetime'] = pd.to_datetime(df_hourly['datetime'])
df_hourly.set_index('datetime', inplace=True)

station.statistics = melodist.StationStatistics(df_hourly.loc[calibration_period]) # statistics
stats = station.statistics
stats.calc_temperature_stats()
stats.calc_humidity_stats()
stats.calc_wind_stats()
stats.calc_radiation_stats()
stats.calc_precipitation_stats()

station.disaggregate_temperature(method='sine_min_max', min_max_time='sun_loc_shift')
station.disaggregate_humidity(method='dewpoint_regression')
station.disaggregate_wind(method='cosine')
station.disaggregate_radiation(method='pot_rad')
station.disaggregate_precipitation(method='cascade')

df_hourly_dis = station.data_disagg.reset_index()
df_hourly_dis['index'] = df_hourly_dis['index'].astype(str)
")

# r  
dt_hourly_dis <- as.data.table(py$df_hourly_dis)
setnames(dt_hourly_dis, "index", "datetime")
    
return(dt_hourly_dis)  
}

## run temporal meteorological disaggregation pipeline
get_tz_offset <- function(tz) { # convert timezone string -> numeric offset
  as.numeric(format(Sys.time(), "%z", tz = tz)) / 100
}

run_pipeline <- function(df_hourly, lat, lon, config, cal_period, val_period) {
  
  dt_h <- hourly_met_data(df_hourly) # hourly data
  dt_d <- daily_met_data_aggregation(dt_h) # daily data
  
  dt_ssd_d <- daily_sunshine_duration(dt_d$date, lat, lon, config$timezone) # daily sunshine duration (hour)
  dt_ssd_h <- daily_sunshine_duration_per_hour(dt_d$date, lat, lon, config$timezone) # daily sunshine duration per hour (min)
  
  formatted <- melodist_format(dt_h, dt_d, dt_ssd_h, dt_ssd_d) # melodist format
  
  cfg <- config 
  cfg$timezone_py <- get_tz_offset(cfg$timezone) # time zone in the format needed for melodist
  
  df_h_dis <- run_melodist(dt_hourly = formatted$hourly, dt_daily = formatted$daily, # run melodist python module   
                           lat = lat, lon = lon, config = cfg, 
                           cal_period = cal_period)
 
  return(df_h_dis)
}