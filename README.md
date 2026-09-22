# EcoTwin Model Input Data Collection in Uniform Format

## Overview

This repository contains scripts and workflows used to extract, process, quality-control, and harmonize lake-specific climate and Earth observation datasets from:

- ERA5 reanalysis meteorological data
- ISIMIP3b climate projection data
- ESA CCI Lakes Level-3S merged products (Version 2.1)

The workflows were developed for four study lakes:

- Lake Balaton (Hungary)
- Lake Iseo (Italy)
- Lake Mälaren (Sweden)
- Lake Wylerbergmeer (Netherlands)

For each data source, the workflow:

1. Identifies the source-data grid cells intersecting each lake.
2. Extracts meteorological or lake-observation time series.
3. Applies quality control (where applicable).
4. Produces harmonized lake-specific NetCDF datasets.

> Important: The scripts contained in this repository generate derived datasets. Users should cite the original data providers (ERA5, ESA CCI Lakes, ISIMIP3b, and HydroLAKES) and comply with the associated licenses.

---

## Files

ERA5
├── script1_identify_era5_grid_cells.R
├── script2_download_era5.R
└── script3_merge_era5_grid_cells.R

ESA_CCI_Lakes
├── script1_download_extract_lakes.R
└── script2_filter_merge_reconstruct.R

ISIMIP3b
├── script1_identify_isimip_grid_cells.R
├── script2_download_isimip3b.R
└── script3_extract_merge_isimip3b.R


---

## Study Lakes

| Lake | ERA5 Grid Cells | ISIMIP3b Grid Cells | ESA CCI Lakes |
|------|------|------|------|
| Balaton | 7 | 4 | Available (ID 310) |
| Iseo | 1 | 2 | Available (ID 300014185) |
| Mälaren | 17 | 9 | Available (ID 163) |
| Wylerbergmeer | 1 | 1 | Not available |

Lake polygons for Balaton, Iseo, and Mälaren were obtained from HydroLAKES. A custom shapefile was used for Wylerbergmeer.

---

# 1. ERA5 Meteorological Data

## Script 1: Identify ERA5 Grid Cells
File: `script1_identify_era5_grid_cells.R`

Purpose:
- Read lake polygons
- Generate ERA5 0.25° grid
- Identify intersecting grid cells
- Export coordinates, maps and RData objects

## Script 2: Download ERA5 Data
File: `script2_download_era5.R`

Downloads hourly ERA5 data (1980-01-01 to 2026-05-31).

Variables:
- t2m
- d2m
- sp
- ssrd
- strd
- tp
- u10
- v10
- tcc

## Script 3: Merge ERA5 Grid Cells
File: `script3_merge_era5_grid_cells.R`

Creates one multi-station NetCDF file per lake.

Outputs:
- lake_balaton_era5.nc
- lake_iseo_era5.nc
- lake_mälaren_era5.nc
- lake_wylerbergmeer_era5.nc

---

# 2. ESA CCI Lakes Products

## Script 1: Download and Extract Lake Data
File: `script1_download_extract_lakes.R`

Downloads daily ESA CCI Lakes products (1992–2022) and extracts lake-specific subsets.

Available lakes:
- Mälaren (ID 163)
- Balaton (ID 310)
- Iseo (ID 300014185)

## Script 2: Filter, Merge and Reconstruct
File: `script2_filter_merge_reconstruct.R`

Applies quality control and produces harmonized lake datasets.

Variables:
- Lake surface water temperature (LSWT)
- Chlorophyll-a
- Turbidity
- Ice cover
- Water extent
- Water level

Final outputs:
- lake_mälaren_cci_lakes.nc
- lake_balaton_cci_lakes.nc
- lake_iseo_cci_lakes.nc

---

# 3. ISIMIP3b Climate Forcing Data

## Script 1: Identify ISIMIP3b Grid Cells
File: `script1_identify_isimip_grid_cells.R`

Identifies 0.5° × 0.5° ISIMIP3b grid cells intersecting each study lake.

## Script 2: Download ISIMIP3b Data
File: `script2_download_isimip3b.R`

Climate models:
- GFDL-ESM4
- IPSL-CM6A-LR
- MPI-ESM1-2-HR
- MRI-ESM2-0
- UKESM1-0-LL

Scenarios:
- Historical (1850–2014)
- SSP126 (2015–2100)
- SSP370 (2015–2100)
- SSP585 (2015–2100)

## Script 3: Extract and Merge Time Series
File: `script3_extract_merge_isimip3b.R`

Produces one NetCDF file per:
- Lake
- GCM
- Scenario

Examples:
- balaton_isimip3b_gfdl-esm4_historical.nc
- balaton_isimip3b_gfdl-esm4_ssp126.nc
- mälaren_isimip3b_mpi-esm1-2-hr_ssp585.nc

---

## Data Sources

- ERA5 Reanalysis (Copernicus Climate Data Store)
- ESA Climate Change Initiative Lakes v2.1
- ISIMIP3b
- HydroLAKES

## Data Availability

The final processed datasets produced by the workflows in this repository have been published in:
Ayala, A. I. (2026). EcoTwin Model Input Data Collection in Uniform Format [Dataset]. Zenodo. https://doi.org/10.5281/zenodo.22876525
