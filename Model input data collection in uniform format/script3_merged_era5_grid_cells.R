##--- Merging extracted ERA5 grid cell data ---##
#----------------- Ana I. Ayala ----------------#
#-------------- Uppsala University -------------#
#------------------ 2026-06-29 -----------------# 

# Description:
#   This script merges ERA5 hourly time-series data for grid cells representing
# each study lake. It reads latitude/longitude and meteorological variables
# from previously downloaded NetCDF files (script2 output), combines station
# data into a single NetCDF per lake, and saves results in lake-specific folders.

# libraries
library(ncdf4)

# working directory
setwd("C:/2025_EcoTwin/WP3/Data in uniform format")

# folder containing ERA5 point NetCDF files
era5_dir <- "script2_download_era5"

# variables 
vars <- c("u10","v10","d2m","t2m","sp","ssrd","strd","tcc","tp")

lake_folders <- list.dirs(era5_dir, recursive = FALSE)
for(lake_folder in lake_folders){
  lake_name <- basename(lake_folder)
  message("Processing ", lake_name)
  nc_files <- list.files(lake_folder, pattern="\\.nc$", full.names=TRUE)
  if(length(nc_files)==0){
    warning("No NetCDF files found.")
    next
  }
  
  # first file (template) 
  nc0 <- nc_open(nc_files[1])
  time <- ncvar_get(nc0,"valid_time")
  ntime <- length(time)
  nstation <- length(nc_files)
  latitude <- numeric(nstation)
  longitude <- numeric(nstation)
  data <- lapply(vars, function(x) matrix(NA_real_, nrow=ntime, ncol=nstation))
  names(data) <- vars
  
  # save metadata from first file
  units <- sapply(vars, function(v) ncatt_get(nc0,v,"units")$value)
  long_name <- sapply(vars, function(v) ncatt_get(nc0,v,"long_name")$value)
  standard_name <- sapply(vars, function(v) {
    tmp <- ncatt_get(nc0,v,"standard_name")
    if(tmp$hasatt) tmp$value else ""
  })
  
  # read all files
  for(i in seq_along(nc_files)){
    message("Reading ", basename(nc_files[i]))
    nc <- nc_open(nc_files[i])
    if(length(ncvar_get(nc,"valid_time")) != ntime)
      stop("Time dimension differs among files.")
    latitude[i]  <- ncvar_get(nc,"latitude")
    longitude[i] <- ncvar_get(nc,"longitude")
    for(v in vars){
      data[[v]][,i] <- ncvar_get(nc,v)
    }
    nc_close(nc)
  }
  
  # define dimensions
  dim_time <- ncdim_def("valid_time", units="seconds since 1970-01-01", vals=time, unlim=TRUE)
  dim_station <- ncdim_def("station", units="station", vals=1:nstation)
  
  # coordinate variables
  var_lat <- ncvar_def("latitude", "degrees_north", list(dim_station), missval=NA, prec="double")
  var_lon <- ncvar_def("longitude", "degrees_east", list(dim_station), missval=NA, prec="double")
  
  # variables
  var_list <- list(var_lat,var_lon)
  for(v in vars){
    var_list[[length(var_list)+1]] <-
      ncvar_def(name=v, units=units[v], dim=list(dim_time,dim_station), missval=NA, prec="float", compression=4)
  }
  
  # output file
  out_dir <- "script3_merged_era5_grid_cells"
  dir.create(file.path(out_dir, lake_name))
  outfile <- file.path(out_dir, lake_name, paste0("lake_",lake_name,"_era5.nc"))
  if(file.exists(outfile))
    file.remove(outfile)
  nc_out <- nc_create(outfile,var_list)
  # coordinates
  ncvar_put(nc_out,"latitude",latitude)
  ncvar_put(nc_out,"longitude",longitude)
  # variables
  for(v in vars){
    ncvar_put(nc_out,v,data[[v]])
    ncatt_put(nc_out,v,"long_name",long_name[v])
    if(nchar(standard_name[v])>0)
      ncatt_put(nc_out,v,"standard_name",standard_name[v])
  }
  # global attributes
  global_attrs <- c("Conventions", "institution", "history")
  for(att in global_attrs){
    tmp <- ncatt_get(nc0,0,att)
    if(tmp$hasatt)
      ncatt_put(nc_out,0,att,tmp$value)
  }
  ncatt_put(nc_out, 0, "title", paste("ERA5 meteorological data for",lake_name))
  ncatt_put(nc_out, 0, "source","Merged point NetCDF files")
  nc_close(nc_out)
  nc_close(nc0)
  message("Saved ", outfile)
}

    