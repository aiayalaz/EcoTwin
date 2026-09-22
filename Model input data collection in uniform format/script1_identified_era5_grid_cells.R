##--- Identified grid cell to extract from ERA5 ---##
#------------------- Ana I. Ayala ------------------#
#---------------- Uppsala Univesity ----------------#
#-------------------- 2026-06-25 -------------------# 

# Description:
#   This script identifies the ERA5 0.25° grid cells that intersect selected lake
# polygons. For each lake, it extracts the coordinates of the intersecting grid-cell
# centers, creates a map showing the lake and the corresponding ERA5 grid cells,
# and saves the results as a .csv file, .png figure, and .RData object for subsequent
# ERA5 data extraction.

# libraries
library(sf)
library(ggplot2)
library(purrr)

# working directory
setwd("C:/2025_EcoTwin/WP3/Data in uniform format")


## lake balaton, mälaren, iseo

# HydroLAKES 
lakes <- st_read(file.path("hydrolakes", "HydroLAKES_polys_v10.shp"))

# identify ERA5 0.25° grid cells covering a lake polygon and build spatial plot
get_lake_era5_cells <- function(lakes, lake_id, lake_name) {
  
  # lake
  lake <- lakes[lakes$Hylak_id == lake_id, ]
  
  # lake's bounding box
  bbox <- st_bbox(lake)
  
  # nearest 0.25° grid boundaries (lon, lat)
  lons <- seq(floor(bbox$xmin * 4) / 4, ceiling(bbox$xmax * 4) / 4, by = 0.25)
  lats <- seq(floor(bbox$ymin * 4) / 4, ceiling(bbox$ymax * 4) / 4, by = 0.25)
  
  # grid-cell centers
  center <- expand.grid(lon = lons, lat = lats)
  # sf point object
  center_sf <- st_as_sf(center, coords = c("lon", "lat"), crs = 4326)
  
  # half grid-cell size (0.125 degree)
  h <- 0.25 / 2
  
  # square polygon centered at (x, y)
  make_square <- function(x, y, h) {
    st_polygon(list(matrix(c(x - h, y - h, x + h, y - h, x + h, y + h, x - h, y + h, x - h, y - h),
                           ncol = 2, byrow = TRUE)))
  }
  # square grid-cell polygons around each center point
  squares <- st_sfc(lapply(seq_len(nrow(center)), function(i) {make_square(center[i, 1], center[i, 2], h)}), 
                    crs = 4326)
  # sf polygon object
  squares_sf <- st_sf(geometry = squares)
  
  # grid cells that intersect the lake
  squares_sf$cover_lake <- lengths(st_intersects(squares_sf, lake)) > 0
  
  # grid cell to extract from ERA5
  era5_grid_cell <- round(st_coordinates(st_centroid(squares_sf[squares_sf$cover_lake, ])), 2)
  colnames(era5_grid_cell) <- c("lon", "lat")
  
  # plot
  plot <- ggplot() + 
    # grid cells that do not intersect the lake
    geom_sf(data = squares_sf[!squares_sf$cover_lake, ], fill = "#CCCCCC", color = "#999999", linewidth = 0.2) +
    # grid cells that intersect the lake
    geom_sf(data = squares_sf[squares_sf$cover_lake, ], fill = "#999999", color = "#666666", linewidth = 0.4) +
    # lake
    geom_sf(data = lake, fill = "#003366", color = "#003366", alpha = 0.4) +
    # grid cell centers
    geom_sf(data = center_sf, color = "#000000", size = 2) +
    coord_sf(expand = FALSE) +
    scale_x_continuous(breaks = sort(unique(center$lon))) +
    scale_y_continuous(breaks = sort(unique(center$lat))) +
    labs(x = "Longitude", y = "Latitude", 
         title = paste("Lake", paste0(toupper(substr(lake_name, 1, 1)), substr(lake_name, 2, nchar(lake_name)))))
  
  return(list(era5_grid_cell = era5_grid_cell, plot=plot))
}

run_get_lake_era5_cells <- function(lakes, lake_ids, lake_names, out_dir = "script1_identified_era5_grid_cells") {
  results <- map(seq_along(lake_ids), function(i) {
    lake_id <- lake_ids[i]
    lake_name <- lake_names[i]
    res <- get_lake_era5_cells(lakes, lake_id, lake_name)
    lake_folder <- file.path(out_dir, lake_name)
    dir.create(lake_folder)
    base_name <- paste("lake", lake_name, "identified_era5_grid_cell", sep = "_")
    # save plot
    ggsave(filename = file.path(lake_folder, paste0(base_name, ".png")),
      plot = res$plot, width = 8, height = 6, dpi = 300)
    # save csv
    write.csv(res$era5_grid_cell, file.path(lake_folder, paste0(base_name, ".csv")), row.names = FALSE)
    # save r object
    save(res, file = file.path(lake_folder, paste0(base_name, ".RData")))
    return(res)
  })
  return(results)
}

run_get_lake_era5_cells(lakes = lakes,
                        lake_ids = c(1251, 14185, 102), 
                        lake_names = c("balaton", "iseo", "mälaren"))
  

## lake wylerbergmeer
# lake
lake <- st_read(file.path("lake_wylerbergmeer", "lake wylerbergmeer.shp"))

# lake's bounding box
bbox <- st_bbox(lake)

# nearest 0.25° grid boundaries (lon, lat)
lons <- seq(floor(bbox$xmin * 4) / 4, ceiling(bbox$xmax * 4) / 4, by = 0.25)
lats <- seq(floor(bbox$ymin * 4) / 4, ceiling(bbox$ymax * 4) / 4, by = 0.25)

# grid-cell centers
center <- expand.grid(lon = lons, lat = lats)
# sf point object
center_sf <- st_as_sf(center, coords = c("lon", "lat"), crs = 4326)

# half grid-cell size (0.125 degree)
h <- 0.25 / 2

# square polygon centered at (x, y)
make_square <- function(x, y, h) {
  st_polygon(list(matrix(c(x - h, y - h, x + h, y - h, x + h, y + h, x - h, y + h, x - h, y - h),
                         ncol = 2, byrow = TRUE)))
}
# square grid-cell polygons around each center point
squares <- st_sfc(lapply(seq_len(nrow(center)), function(i) {make_square(center[i, 1], center[i, 2], h)}), 
                  crs = 4326)
# sf polygon object
squares_sf <- st_sf(geometry = squares)

# grid cells that intersect the lake
squares_sf$cover_lake <- lengths(st_intersects(squares_sf, lake)) > 0

# grid cell to extract from ERA5
era5_grid_cell <- round(st_coordinates(st_centroid(squares_sf[squares_sf$cover_lake, ])), 2)
colnames(era5_grid_cell) <- c("lon", "lat")

# plot
plot <- ggplot() + 
  # grid cells that do not intersect the lake
  geom_sf(data = squares_sf[!squares_sf$cover_lake, ], fill = "#CCCCCC", color = "#999999", linewidth = 0.2) +
  # grid cells that intersect the lake
  geom_sf(data = squares_sf[squares_sf$cover_lake, ], fill = "#999999", color = "#666666", linewidth = 0.4) +
  # lake
  geom_sf(data = lake, fill = "#003366", color = "#003366", alpha = 0.4) +
  # grid cell centers
  geom_sf(data = center_sf, color = "#000000", size = 2) +
  coord_sf(expand = FALSE) +
  scale_x_continuous(breaks = sort(unique(center$lon))) +
  scale_y_continuous(breaks = sort(unique(center$lat))) +
  labs(x = "Longitude", y = "Latitude", title = "Lake Wylerbergmeer")

dir.create(file.path("script1_identified_era5_grid_cell", "wylerbergmeer"))
# save plot
ggsave(filename = file.path("script1_identified_era5_grid_cell/wylerbergmeer", "lake_wylerbergmeer.png"),
       plot = plot, width = 8, height = 6, dpi = 300)
# save csv
write.csv(era5_grid_cell, file.path("script1_identified_era5_grid_cell/wylerbergmeer", "lake_wylerbergmeer.csv"), row.names = FALSE)
# save r object
res <- list(era5_grid_cell, plot)
save(res, file = file.path("script1_identified_era5_grid_cell/wylerbergmeer", "lake_wylerbergmeer.RData"))
