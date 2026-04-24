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

setwd("C:/Users/anaay970/Documents/Projects/2025_EcoTwin/WP3/Climate data downscaling/Final") # working directory

Sys.setenv(tz="Etc/GMT-1")

# source functions
source("script2_temporal_downscaling_teddytool_functions.R")
# leap_year <- function(y)
# doy_window_fun(doy, year, window = 11) 
# get_precip_class(p_prev, p_today, p_next)
# rank_with_ties(x)
# get_hourly_factors(hourly, is_precip = FALSE)
# hourly_met_data(dt)  
# daily_met_data_aggregation(dt_h)
# find_best_day(target_row, ref_daily, window = 11) 
# scale_temperature(hourly_temp, tmin, tmax, tmean_target) 
# scale_humidity(rh, target_mean)
# limit_radiation(rad_hourly, rad_potential, target_mean) 
# fit_precip_model(hourly)
# generate_precip_event(total_precip, model)
# compute_rank(candidates, target, use_precip = TRUE) {
# hourly_disaggregation_pipeline(target_daily, ref_daily, ref_hourly, rad_pot = NULL) {
source("script2_performance_and_figures.R")
# statistical_performance_metrics(obs, sim)
# scatter_fig(dt, var)
# plot_fig(dt, var, period)

## pre-processing
# meteorological station: malma island in lake erken, sweden (lat=59.83917, lon=18.629333, tz=UTC+1)
load(file.path("df_erken_met_hourly.RData")) # hourly data: at [degreeC], rh [%], ws [m/s], pr [mm/h], swrin [W/m2], lwrin [W/m2] (1988-2025)
lat <- 59.83917 # degree
lon <- 18.629333 # degree
tz <- "Etc/GMT-1"

dt_h <- hourly_met_data(df_erken_met_hourly) # hourly dataset
dt_h[, year := year(datetime)] # extract year
days_per_year <- dt_h[, .(n_days = uniqueN(date)), by = year] # number of full days available for each year
# year n_days
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
years <- sort(unique(dt_h$year)) # sorted list of all unique years present in the dataset
# 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 (80 % of all years)
# 2019 2020 2021 2022 2023 2024 (20 % of all years)
# reference years (80% of all years)
n_ref <- floor(0.8 * length(years))
# select 80% of the years to form the reference dataset
ref_years <- years[1:n_ref]
# the remaining 20% of years will form the target dataset
target_years <- years[(n_ref + 1):length(years)]
# subset the hourly data into reference and target sets
dt_ref_h <- dt_h[year %in% ref_years] # reference data set
dt_target_h <- dt_h[year %in% target_years] # target data set

# daily summaries
dt_ref_d <- daily_met_data_aggregation(dt_ref_h) # reference (1995-2018)
dt_target_d <- daily_met_data_aggregation(dt_target_h) # target (2019-2024)
# remove the first and last row from dt_target_d
dt_target_d <- dt_target_d[-c(1, .N)]

## temporal disaggregation of climate data: teddy tool pipeline
# hourly_disaggregation(dt_target_d, dt_ref_d, dt_ref_h)
# dt_target_d: daily aggregated data for the target period (to disaggregate)
# dt_ref_d: daily aggregated data for the reference period
# dt_ref_h: hourly data for the reference period (used to derive typical intra-day patterns)
dt_target_h_dis <- hourly_disaggregation_pipeline(dt_target_d, dt_ref_d, dt_ref_h)

## post-processing
dir.create("script2_performance_and_figures")

# merge obs and dis
dt_target_h_obs_dis <- merge(dt_target_h, dt_target_h_dis, by = "datetime", suffixes = c("_obs", "_dis"))
dt_target_h_obs_dis[, month := month(datetime)] 
dt_target_h_obs_dis[, season := fifelse(month %in% c(12, 1, 2), "winter", fifelse(month %in% c(3, 4, 5), "spring", fifelse(month %in% c(6, 7, 8), "summer", "autumn")))]

# performance and figures
vars <- c("temp", "humidity", "radiation", "wind", "precip")
# performance metrics
l_pm <- lapply(vars, function(v) {
  obs_col <- paste0(v, "_obs")
  dis_col <- paste0(v, "_dis")
  list(variable = v,
       target = statistical_performance_metrics(dt_target_h_obs_dis[, get(obs_col)], dt_target_h_obs_dis[, get(dis_col)]))
})
dt_pm <- rbindlist(lapply(l_pm, function(x) {
  data.table(var = x$variable, season = "all", x$target)}), fill = TRUE)

l_pm_season <- list()
for (v in vars) {
  obs_col <- paste0(v, "_obs")
  dis_col <- paste0(v, "_dis")
  for (s in unique(dt_target_h_obs_dis$season)) {
    dt_season <- dt_target_h_obs_dis[season == s]
    pm <- statistical_performance_metrics(dt_season[[obs_col]], dt_season[[dis_col]])
    l_pm_season[[length(l_pm_season) + 1]] <- data.table(var = v, season = s, pm)
  }
}
dt_pm_season <- rbindlist(l_pm_season, fill = TRUE)
dt_pm_all <- rbindlist(list(dt_pm, dt_pm_season), fill = TRUE)
fwrite(dt_pm_all, file.path("script2_performance_and_figures", "script2_performance_metrics.csv"))
# figures
# scatter
for (var in vars) {
  filename <- file.path("script2_performance_and_figures", paste0("script2_plot_", var, ".png"))
  png(filename, width = 550, height = 550, res = 120)
  scatter_fig(dt_target_h_obs_dis, var)
  dev.off()
}
# plot
months_target <- seq(floor_date(min(dt_target_h_obs_dis$datetime, na.rm = TRUE), "month"), floor_date(max(dt_target_h_obs_dis$datetime, na.rm = TRUE), "month"), by = "month")
for (var in vars) {
  for (i in seq_along(months_target)) {
    start_date <- as.POSIXct(months_target[i])
    if (i < length(months_target)) {
      end_date <- months_target[i + 1] - days(1)
    } else {
      end_date <- tail(dt_target_h_obs_dis$datetime, 1)
    }
    filename <- file.path("script2_performance_and_figures", paste0("script2_plot_", var, "_", format(start_date, "%Y_%m"), ".png"))
    png(filename, width = 1600, height = 600, res = 120)
    plot_fig(dt_target_h_obs_dis, var, c(start_date, end_date))
    dev.off()
  }
}