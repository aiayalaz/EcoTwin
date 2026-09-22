##--- Download ESA CCI Lakes products ---##
#-------------- Ana I. Ayala -------------#
#----------- Uppsala Univesity -----------#
#--------------- 2026-06-29 --------------# 

# https://climate.esa.int/en/projects/lakes/
# https://github.com/cci-lakes/lakes_cci_tools

import os
import numpy as np
import xarray as xr
import datetime

# lakes mask file
maskfile = "esa_cci_lakes_v2.1/ESA_CCI_static_lake_mask_v2.1.nc"

# lake Mälaren ID
lake_id = 163 # Mälaren: 163, Balaton: 310, Iseo: 300014185

# date range
mindate = "1992-09-26"
maxdate = "2022-12-31"

# dataset version
version = "2.1"

# output
outdir = "script1_download_cci_lakes/Mälaren"
outprefix = "Mälaren_"

# prepare dates
mindate = datetime.datetime.strptime(mindate, "%Y-%m-%d")
maxdate = datetime.datetime.strptime(maxdate, "%Y-%m-%d")

mindate = max(mindate, datetime.datetime(1992, 9, 26)) # 1992-09-26
maxdate = min(maxdate, datetime.datetime(2022, 12, 31))

# create output directory
os.makedirs(outdir, exist_ok=True)

# read lake mask
mask_xr = xr.open_dataset(maskfile)
mask = mask_xr["CCI_lakeid"].values
mask_ind = np.where(mask == lake_id)
minx = np.min(mask_ind[1]) - 1
maxx = np.max(mask_ind[1]) + 1
miny = np.min(mask_ind[0]) - 1
maxy = np.max(mask_ind[0]) + 1

mask_lake = mask[miny:maxy + 1, minx:maxx + 1]
mask_lake = (mask_lake == lake_id).astype(np.uint8)

## download loop
base_url = ("https://data.cci.ceda.ac.uk/thredds/dodsC/esacci/lakes/data/lake_products/L3S/v2.1/merged_product/")
for ordinal in range(mindate.toordinal(), maxdate.toordinal() + 1):
    current_date = datetime.datetime.fromordinal(ordinal)
    date_str = current_date.strftime("%Y%m%d")
    print(f"Downloading data from ESACCI-LAKES-L3S-LK_PRODUCTS-MERGED-{date_str}-fv{version}.nc")
    path = (f"{base_url}"
        f"{current_date.year}/"
        f"{current_date.month:02d}/"
        f"ESACCI-LAKES-L3S-LK_PRODUCTS-MERGED-{date_str}-fv{version}.0.nc")
        
    dataset = xr.open_dataset(path, engine="netcdf4")

    # extract data in the defined zone
    dataset = dataset.isel(lat=slice(miny, maxy + 1), lon=slice(minx, maxx + 1))

    # apply lake mask
    for var in dataset.data_vars:
        if len(dataset[var].dims) == 3:
            fill_value = dataset[var].encoding.get("_FillValue", np.nan)
            data = dataset[var][0, :, :].values.copy()
            data[mask_lake == 0] = fill_value
            dataset[var][0, :, :] = data

    # create output file
    outfile = os.path.join(outdir, f"{outprefix}ESACCI-LAKES-L3S-LK_PRODUCTS-MERGED-{date_str}-fv{version}.nc")
    dataset.to_netcdf(outfile)

 
