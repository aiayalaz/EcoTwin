##--- Extracting ISIMIP3b grid cell data ---##
#--------------- Ana I. Ayala ---------------#
#------------ Uppsala University ------------#
#---------------- 2026-07-09 ----------------# 

# Description:
#   This script reads global ISIMIP3b NetCDF files (one file per 10-year period and meteorological variable),
# identify the lake grid cells for each study lake, extract full time series for all meteorological variables,
# concatenate all 10-year periods into a continuous time dimension, merge the extracted grid cells into a
# lake-specific NetCDF file and save the outputs organized by lake, scenario and gcm with variables
# stored as time x stations (lat, lon).
# each study lake. It reads latitude/longitude and meteorological variables
# from previously downloaded NetCDF files (script2 output), combines station
# data into a single NetCDF per lake, and saves results in lake-specific folders.

# libraries
library(ncdf4)

# working directory
setwd("C:/2025_EcoTwin/WP3/Data in uniform format")

extract_lake <- function(gcm, scenario, vars, lake_name, lake_cells, input_dir, output_dir){
  # first variable only used to define dimensions
  pattern <- paste0("^", gcm, ".*", scenario, ".*", vars[1], "_.*\\.nc$")
  first_file <- sort(list.files(input_dir, pattern = pattern, full.names = TRUE))[1]
  nc <- nc_open(first_file)
  lon <- ncvar_get(nc, "lon")
  lat <- ncvar_get(nc, "lat")
  lon_idx <- match(lake_cells$lon, lon)
  lat_idx <- match(lake_cells$lat, lat)
  lon_unique <- sort(unique(lon_idx))
  lat_unique <- sort(unique(lat_idx))
  lon_sub <- lon[lon_unique]
  lat_sub <- lat[lat_unique]
  nc_close(nc)
  
  # read all time vectors and convert to common origin
  files <- sort(list.files(input_dir, pattern = pattern, full.names = TRUE))
  time_all <- c()
  time_units <- NULL
  ref_origin <- NULL
  ref_unit <- NULL
  for(f in files){
    nc <- nc_open(f)
    t <- ncvar_get(nc, "time")
    units <- ncatt_get(nc, "time", "units")$value
    # first file defines the output reference
    if(is.null(time_units)){
      time_units <- units
      ref_origin <- sub(".*since ", "", units)
      ref_unit <- sub(" since.*", "", units)
      ref_origin <- as.POSIXct(ref_origin, tz = "UTC")
    }
    origin <- sub(".*since ", "", units)
    unit <- sub(" since.*", "", units)
    if(unit != ref_unit){
      stop("Different time units found: ", unit, " and ", ref_unit)
    }
    origin <- as.POSIXct(origin, tz = "UTC")
    offset <- as.numeric(difftime(origin, ref_origin, units = ref_unit))
    t <- t + offset
    time_all <- c(time_all, t)
    nc_close(nc)
  }
  
  # define output dimensions
  londim <- ncdim_def("lon", "degrees_east", lon_sub)
  latdim <- ncdim_def("lat", "degrees_north", lat_sub)
  timedim <- ncdim_def("time", time_units, time_all, unlim = TRUE)
  
  # define variables
  var_defs <- list()
  for(v in vars){
    file_v <- sort(list.files(input_dir, pattern = paste0("^", gcm, ".*_", scenario, ".*", v, "_.*\\.nc$"), full.names = TRUE))[1]
    nc <- nc_open(file_v)
    units <- ncatt_get(nc, v, "units")$value
    var_defs[[v]] <- ncvar_def(v, units, list(londim, latdim, timedim), missval = NA, prec = "float")
    nc_close(nc)
  }
  outfile <- file.path(output_dir, paste0(lake_name, "_", gcm, "_", scenario, ".nc"))
  ncout <- nc_create(outfile, var_defs)
  
  # write the unified time coordinate
  ncvar_put(ncout, "time", time_all)
  
  # create lake mask
  mask <- matrix(FALSE, length(lon_unique), length(lat_unique))
  for(i in seq_len(nrow(lake_cells))){
    x <- match(lon_idx[i], lon_unique)
    y <- match(lat_idx[i], lat_unique)
    mask[x, y] <- TRUE
  }
  
  # write variables
  for(v in vars){
    cat("Processing", v, "\n")
    files <- sort(list.files(input_dir, pattern = paste0("^", gcm, ".*_", scenario, ".*", v, "_.*\\.nc$"), full.names = TRUE))
    start_time <- 1
    for(f in files){
      nc <- nc_open(f)
      x <- ncvar_get(nc, v)
      sub <- x[lon_unique, lat_unique,,drop = FALSE]
      # mask land cells
      for(tt in seq_len(dim(sub)[3])){
        tmp <- sub[,,tt]
        tmp[!mask] <- NA
        sub[,,tt] <- tmp
      }
      nt <- dim(sub)[3]
      ncvar_put(ncout, v, sub, start = c(1, 1, start_time), count = c(-1, -1, nt))
      start_time <- start_time + nt
      nc_close(nc)
    }
  }
  nc_close(ncout)
  cat("Finished:", outfile, "\n")
}

# run extract_lake() for each lake, gcm and scenario
vars <- c("tas", "tasmin", "tasmax", "hurs", "huss", "rlds", "rsds", "pr", "ps", "sfcwind")
lake_names <- c("wylerbergmeer", "iseo", "mälaren", "balaton")
scenarios <- c("historical", "ssp126", "ssp370", "ssp585")
gcms <- c("gfdl-esm4", "ipsl-cm6a-lr", "mpi-esm1-2-hr", "mri-esm2-0", "ukesm1-0-ll")
for (lake_name in lake_names) {
  lake_folder <- file.path("script1_identified_isimip3b_grid_cells", lake_name)
  csv_file <- list.files(lake_folder, pattern = ".csv$", full.names = TRUE)
  lake_cells <- read.csv(csv_file)
  out_dir <- file.path("script3_extracted_isimip3b_grid_cells", lake_name)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  for(gcm in gcms){
    for(scenario in scenarios){
      cat("Processing:", lake_name, gcm, scenario, "\n")
      extract_lake(gcm=gcm, scenario=scenario, vars=vars, lake_name=lake_name, lake_cells=lake_cells,
                   input_dir="D:/isimip3b/climate_related_forcing/global",
                   output_dir=out_dir)
    }
  }
}