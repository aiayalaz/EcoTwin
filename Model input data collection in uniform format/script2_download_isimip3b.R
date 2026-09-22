##--- Download ISIMIP3b products ---##
#----------- Ana I. Ayala -----------#
#--------- Uppsala Univesity --------#
#------------ 2026-04-17 ------------# 

# libraries
library(ncdf4)
library(data.table)
library(lubridate)
library(curl)

# working directory
setwd("C:/2025_EcoTwin/WP3/Data in uniform format")

### climate data / isimip3b / daily
# date: yyyy-mm-dd
# air temperature: tas [degreeK] at 2m, daily average
# min air temperature: tasmin [degreeK] at 2m, daily min
# max air temperature: tasmax [degreeK] at 2m, daily max
# relative humidity: hurs [%] at 2m, daily average
# incoming short-wave radiation: rsds [W/m2] at surface, daily average
# incoming long-wave radiation: rlds [W/m2] at surface, daily average
# wind speed: sfcwind [m/s] at 10 m, daily average
# precipitation: pr [kg/m2/s], daily total
# air pressure: ps [Pa] at surface, daily average

## automated downloader
gcms <- c("gfdl-esm4", "ipsl-cm6a-lr", "mpi-esm1-2-hr", "mri-esm2-0", "ukesm1-0-ll")
scenarios <- c("historical", "ssp126", "ssp370", "ssp585")
# vars <- c("tas", "tasmin", "tasmax", "hurs", "rlds", "rsds", "pr", "ps", "sfcwind")
vars <- c("huss")
base_url <- "https://files.isimip.org/ISIMIP3b/InputData/climate/atmosphere/bias-adjusted/global/daily"
outdir <- "C:/isimip3b/climate_related_forcing/global"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
# periods
get_periods <- function(scenario) {
  if (scenario == "historical") {
    return(c("1850_1850","1851_1860","1861_1870","1871_1880","1881_1890",
             "1891_1900","1901_1910","1911_1920","1921_1930","1931_1940",
             "1941_1950","1951_1960","1961_1970","1971_1980","1981_1990",
             "1991_2000","2001_2010","2011_2014"))
  } else {
    return(c("2015_2020","2021_2030","2031_2040","2041_2050",
             "2051_2060","2061_2070","2071_2080","2081_2090","2091_2100"))
  }
}
# url list
build_urls <- function(gcm, scenario, var) {
  periods <- get_periods(scenario)
  paste0(base_url, "/", scenario, "/", toupper(gcm), "/", gcm, "_r1i1p1f1_w5e5_", # gfdl-esm4, ipsl-cm6a-lr, mpi-esm1-2-hr, mri-esm2-0
         scenario, "_", var, "_global_daily_", periods, ".nc")
  # paste0(base_url, "/", scenario, "/", toupper(gcm), "/", gcm, "_r1i1p1f2_w5e5_", # ukesm1-0-ll
  #        scenario, "_", var, "_global_daily_", periods, ".nc")
  
}
urls <- unlist(lapply(gcms, function(g) {
  unlist(lapply(scenarios, function(s) {
    unlist(lapply(vars, function(v) {
      build_urls(g, s, v)
      }))
  }))
}))
# download function
download_file <- function(url) {
  file <- file.path(outdir, basename(url))
  if (file.exists(file)) {
    cat("✓ Exists:", basename(url), "\n")
    return(TRUE)
  }
  cat("Downloading:", basename(url), "\n")
  ok <- tryCatch({
    curl_download(url, file)
    TRUE
  }, error = function(e) {
    cat("✗ Failed:", basename(url), "\n")
    FALSE
  })
  return(ok)
}
failed <- c()
for (url in urls) {
  ok <- download_file(url)
  if (!ok) {
    failed <- c(failed, url)
  }
}
# retry failed once
if (length(failed) > 0) {
  cat("\nRetrying failed downloads...\n")
  for (url in failed) {
    download_file(url)
  }
}
cat("\nDONE.\n")