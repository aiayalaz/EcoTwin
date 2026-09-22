##--- ERA5 single-level climate time series download ---##
#---------------------- Ana I. Ayala---------------------#
#------------------ Uppsala University ------------------#
#---------------------- 2026-06-25 ----------------------# 

# Description:
#   This script downloads ERA5 hourly time-series data for the grid cells
# previously identified as representing each study lake. The script reads 
# latitude and longitude coordinates of the selected ERA5 grid cells generated
# in script1_identified_era5_grid_cell.R, request the selected meteorological 
# variables from the Copernicus Climate Data Store (CDS) for the period 
# 1980-01-01 to 2026-05-31, and stores the downloaded NetCDF files in 
# lake-specific folders. 

# libraries
library(ecmwfr)

# working directory
setwd("C:/2025_EcoTwin/WP3/Data in uniform format")

# CDS API credentials
wf_set_key(key = "") # add your key

# variables to download from era5
variables <-  c("2m_dewpoint_temperature",
                "surface_pressure",
                "surface_solar_radiation_downwards",
                "surface_thermal_radiation_downwards",
                "2m_temperature",
                "total_precipitation",
                "10m_u_component_of_wind",
                "10m_v_component_of_wind",
                "total_cloud_cover")
# folder containing ERA5 grid-cell coordinates identified in script1_identified_era5_grid_cell.R
grid_dir <- "script1_identified_era5_grid_cells" # folder containing grid-cell coordinates from script1
# output folder for downloaded ERA5 data
out_dir <- "script2_download_era5"
# lake folders
lake_folders <- list.dirs(grid_dir, recursive = FALSE) 
# loop through lakes
for (lake_folder in lake_folders) {
  lake_name <- basename(lake_folder)
  message("Processing lake: ", lake_name)
  # locate ERA5 grid-cell coordinate file
  csv_file <- list.files(lake_folder, pattern = ".csv$", full.names = TRUE)
  # read ERA5 grid-cell coordinates
  grids <- read.csv(csv_file)
  # create output folder for lake
  lake_out <- file.path(out_dir, lake_name)
  dir.create(lake_out, recursive = TRUE)
  # loop through all ERA5 grid cells associated with the lake
  for (k in seq_len(nrow(grids))) {
    lat <- grids$lat[k] 
    lon <- grids$lon[k]
    message(sprintf("Downloading %s: lat=%.2f lon=%.2f", lake_name, lat, lon))
    # data request
    cds_request <- list(dataset_short_name = "reanalysis-era5-single-levels-timeseries", 
                        product_type = "reanalysis", 
                        variable = variables, 
                        location = list(longitude = lon, latitude = lat ), 
                        date = "1980-01-01/2026-05-31", 
                        format = "netcdf", 
                        target = sprintf("era5_lat%.2f_lon%.2f.nc", lat, lon))
    # submit request and download file
    wf_request(request = cds_request, 
               transfer = TRUE, 
               path = lake_out)
    # extract .nc from .zip
    zip_file <- unzip(file.path(lake_out, sprintf("era5_lat%.2f_lon%.2f.zip", lat, lon)))
    # rename .nc
    file.rename(from = zip_file,
                to = file.path(lake_out,sprintf("era5_lat%.2f_lon%.2f.nc", lat, lon)))
    # remove .zip
    file.remove(file.path(lake_out,sprintf("era5_lat%.2f_lon%.2f.zip", lat, lon)))
  }
}