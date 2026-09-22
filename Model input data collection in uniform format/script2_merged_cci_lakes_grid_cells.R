##--- Merging ESA CCI Lakes products ---##
#------------- Ana I. Ayala -------------#
#---------- Uppsala Univesity -----------#
#-------------- 2026-07-03 --------------# 

# Description:
#   This script processes and merges satellite-derived lake products stored in 
# NetCDF files for multiple lakes. It reads all .nc files for each lake, extracts 
# lake surface water temperature, chlorophyll-a, turbidity, ice cover, water extent
# and water level, and applies quality-control filters to remove unreliable or flagged 
# data. After cleaning, it merges all time steps into a single multi-temporal dataset
# per variable. Finally, it reconstructs a new NetCDF file


# libraries
library(ncdf4)
library(terra)
library(stringr)
library(progress)
library(abind)

# working directory
setwd("C:/2025_EcoTwin/WP3/Data in uniform format")

input_dir <- "script1_download_cci_lakes"
output_dir <- "script2_merged_cci_lakes"

get_files <- function(lake_folder){
  files <- list.files(lake_folder, pattern="\\.nc$", full.names=TRUE)
  files <- sort(files)
  return(files)
}

open_lake_file <- function(file){
  nc <- nc_open(file)
  return(nc)
}

read_variable <- function(nc,var){
  if(!(var %in% names(nc$var)))
    return(NULL)
  ncvar_get(nc,var)
}

read_dimensions <- function(nc){
  list(lon=ncvar_get(nc,"lon"), lat=ncvar_get(nc,"lat"), time=ncvar_get(nc,"time"))
}

variable_exists <- function(nc,var){
  var %in% names(nc$var)
}

read_all_products <- function(nc){
  list(lswt=read_variable(nc, "lake_surface_water_temperature"),
       lswt_quality=read_variable(nc, "lswt_quality_level"),
       lswt_unc=read_variable(nc, "lswt_uncertainty"),
       
       chla=read_variable(nc, "chla_mean"),
       chla_unc=read_variable(nc, "chla_uncertainty"),
       
       turb=read_variable(nc, "turbidity_mean"), 
       turb_unc=read_variable(nc, "turbidity_uncertainty"),
       
       lwlr_quality=read_variable(nc, "lwlr_quality_flag"),
       
       lic=read_variable(nc, "lake_ice_cover_class"),
       
       lwe=read_variable(nc, "lake_surface_water_extent"),
       lwe_quality=read_variable(nc, "lwe_quality_flag"),
       
       lwl=read_variable(nc, "water_surface_height_above_reference_datum"),
       lwl_quality=read_variable(nc, "lwl_quality_flag"))
}

filter_lswt <- function(lswt, quality, uncertainty){
  if(is.null(lswt) || is.null(quality) || is.null(uncertainty)) return(NULL)
  good <- quality %in% c(4,5) 
  lswt[!good] <- NA
  uncertainty[!good] <- NA
  bad <- lswt == 1.862645e-09
  lswt[bad] <- NA
  uncertainty[bad] <- NA
  list(lswt = lswt, lswt_unc = uncertainty)
}

filter_chla <- function(chla, uncertainty, lwlr_flag){
  if(is.null(chla) || is.null(uncertainty) || is.null(lwlr_flag)) return(NULL)
  bad <- bitwAnd(lwlr_flag, 64) != 0 | bitwAnd(lwlr_flag, 128) != 0 | bitwAnd(lwlr_flag, -128) != 0
  chla[bad] <- NA
  uncertainty[bad] <- NA
  list(chla = chla, chla_unc = uncertainty)
}

filter_turb <- function(turb, uncertainty, lwlr_flag){
  if(is.null(turb) || is.null(uncertainty) || is.null(lwlr_flag)) return(NULL)
  bad <- bitwAnd(lwlr_flag, 64) != 0 | bitwAnd(lwlr_flag, 128) != 0 | bitwAnd(lwlr_flag, -128) != 0
  turb[bad] <- NA
  uncertainty[bad] <- NA
  list(turb = turb, turb_unc = uncertainty)
}

filter_lic <- function(lic){
  if(is.null(lic)) return(NULL)
  lic[!(lic %in% c(1,2))] <- NA
  lic
}

filter_lwe <- function(lwe, quality){
  if(is.null(lwe) || is.null(quality)) return(NULL)
  good <- quality %in% c(0,1)
  lwe[!good] <- NA
  lwe
}

filter_lwl <- function(lwl, quality){
  if(is.null(lwl) || is.null(quality)) return(NULL)
  good <- quality %in% c(0,1)
  lwl[!good] <- NA
  lwl
}

process_file <- function(file){
  nc <- nc_open(file)
  on.exit(nc_close(nc))
  
  x <- read_all_products(nc)
  
  lswt_res <- filter_lswt(x$lswt, x$lswt_quality, x$lswt_unc)
  x$lswt <- lswt_res$lswt
  x$lswt_unc <- lswt_res$lswt_unc
  
  chla_res <- filter_chla(x$chla, x$chla_unc, x$lwlr_quality)
  x$chla <- chla_res$chla
  x$chla_unc <- chla_res$chla_unc

  turb_res <- filter_turb(x$turb, x$turb_unc, x$lwlr_quality)
  x$turb <- turb_res$turb
  x$turb_unc <- turb_res$turb_unc

  x$lic <- filter_lic(x$lic)
  x$lwe <- filter_lwe(x$lwe, x$lwe_quality)
  x$lwl <- filter_lwl(x$lwl, x$lwl_quality)

  x
}

merge_files <- function(files){
  
  all <- lapply(files, process_file)
  
  merge_var <- function(name){
    vals <- lapply(all, function(x) x[[name]])
    vals <- vals[!sapply(vals, is.null)]
    
    if(length(vals) == 0) return(NULL)
    
    abind(vals, along = length(dim(vals[[1]])) + 1)
  }
  
  list(lswt = merge_var("lswt"),
       lswt_unc = merge_var("lswt_unc"),
       
       chla = merge_var("chla"),
       chla_unc = merge_var("chla_unc"),
       
       turb = merge_var("turb"),
       turb_unc = merge_var("turb_unc"),
       
       lic = merge_var("lic"),
       lwe = merge_var("lwe"),
       lwl = merge_var("lwl"))
}

get_time_values <- function(files) {
  sapply(files, function(f) {
    nc <- nc_open(f)
    on.exit(nc_close(nc), add = TRUE)
    ncvar_get(nc, "time")
  })
}

write_merged_nc <- function(data, template_file, files, out_file){
  
  nc_in <- nc_open(template_file)
  on.exit(nc_close(nc_in))
  
  lon_vals <- nc_in$dim$lon$vals
  lat_vals <- nc_in$dim$lat$vals
  time_vals <- get_time_values(files)
  
  dim_lon <- ncdim_def("lon", "degrees_east", lon_vals)
  dim_lat <- ncdim_def("lat", "degrees_north", lat_vals)
  dim_time <- ncdim_def("time", nc_in$dim$time$units, time_vals, unlim = TRUE)
  
  dims <- list(dim_lat, dim_lon, dim_time)
  
  fill_float <- -9999
  fill_byte  <- -127
  
  clean_float <- function(x, fill = fill_float) {
    x[is.na(x)] <- fill
    x
  }
  
  data$lswt <- clean_float(data$lswt)
  data$lswt_unc <- clean_float(data$lswt_unc)
  data$chla <- clean_float(data$chla)
  data$chla_unc <- clean_float(data$chla_unc)
  data$turb <- clean_float(data$turb)
  data$turb_unc <- clean_float(data$turb_unc)
  data$lwe <- clean_float(data$lwe)
  data$lwl <- clean_float(data$lwl)
  
  data$lic <- as.integer(data$lic)
  data$lic[is.na(data$lic)] <- fill_byte
  
  vars <- list(lswt = ncvar_def(name="lswt", units="K", dim=dims, missval=fill_float, longname="Lake surface water temperature", prec="float"),
               lswt_unc = ncvar_def(name="lswt_uncertainty", units="K", dim=dims, missval=fill_float, longname="Uncertainty of lake surface water temperature", prec="float"),
               
               chla = ncvar_def(name="chla", units="mg m-3", dim=dims, missval=fill_float, longname="Chlorophyll-a concentration", prec="float"),
               chla_unc = ncvar_def(name="chla_uncertainty", units="percent", dim=dims, missval=fill_float, longname="Uncertainty of chlorophyll-a concentration", prec="float"), 
               
               turb = ncvar_def(name="turb", units="NTU", dim=dims, missval=fill_float, longname="Turbidity", prec="float"),
               turb_unc = ncvar_def("turb_uncertainty", "percent", dim=dims, missval=fill_float, longname="Uncertainty of turbidity", prec="float"), 
               
               lic = ncvar_def(name="lic", units="", dim=dims, missval=fill_byte, longname="Lake ice cover class", prec="byte"),
               lwe = ncvar_def(name="lwe", units="km2", dim=dims, missval=fill_float, longname="Lake surface water extent", prec="float"),
               lwl = ncvar_def(name="lwl", units="m", dim=dims, missval=fill_float, longname="Water surface height above reference datum", prec="float"))
  
  nc_out <- nc_create(out_file, vars)
  
  ncvar_put(nc_out,vars$lswt, data$lswt)
  ncvar_put(nc_out,vars$lswt_unc, data$lswt_unc)
  ncvar_put(nc_out,vars$chla, data$chla)
  ncvar_put(nc_out,vars$chla_unc, data$chla_unc)
  ncvar_put(nc_out,vars$turb, data$turb)
  ncvar_put(nc_out,vars$turb_unc, data$turb_unc)
  data$lic <- as.integer(data$lic)
  ncvar_put(nc_out,vars$lic, data$lic)
  ncvar_put(nc_out,vars$lwe, data$lwe)
  ncvar_put(nc_out,vars$lwl, data$lwl)
  
  ncatt_put(nc_out, "lswt", "comment", "Values filtered to include only lswt_quality_level = 4 (acceptable) and 5 (best)")
  ncatt_put(nc_out, "lswt_uncertainty", "comment", "Values filtered to include only lswt_quality_level = 4 (acceptable) and 5 (best)")
  ncatt_put(nc_out, "chla", "comment", "Values filtered to include only lwlr_quality_flag ≠ 61 (poor consistency) and  -128 (low consistency")
  ncatt_put(nc_out, "chla_uncertainty", "comment", "Values filtered to include only lwlr_quality_flag = 0")
  ncatt_put(nc_out, "turb", "comment", "Values filtered to include only lwlr_quality_flag = 0")
  ncatt_put(nc_out, "turb_uncertainty", "comment", "Values filtered to include only lwlr_quality_flag = 0")
  ncatt_put(nc_out, "lic", "comment", "Values filtered to include only lic = 1 (water) and 2 (ice)")
  ncatt_put(nc_out, "lwe", "comment", "Values filtered to include only lwe_quality_flag = 0 (good) and 1 (medium)")
  ncatt_put(nc_out, "lwl", "comment", "Values filtered to include only lwl_quality_flag = 0 (good) and 1 (medium)")
  
  nc_close(nc_out)
}

# run pipeline
lakes <- c("Mälaren", "Balaton", "Iseo")
for (i in lakes) {
  
  message("Processing lake: ", i)
  
  # input files per lake
  lake_path <- file.path(input_dir, i)
  files <- get_files(lake_path)
  if (length(files) == 0) {
    message("No files found for ", i)
    next
  }
  
  # process + merge
  merged_data <- merge_files(files)
  
  lake_folder <- file.path(output_dir, i)
  dir.create(lake_folder, recursive = TRUE, showWarnings = FALSE)
  
  # output path per lake
  out_file <- file.path(lake_folder, paste0("lake_", tolower(i), "_cci_lakes.nc"))
  
  # write result
  write_merged_nc(data = merged_data, template_file = files[1], files = files, out_file = out_file)
}