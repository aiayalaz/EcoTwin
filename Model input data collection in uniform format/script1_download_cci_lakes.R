##--- Download ESA CCI Lakes products ---##
#-------------- Ana I. Ayala -------------#
#----------- Uppsala Univesity -----------#
#--------------- 2026-06-29 --------------# 

# Description:
#    This scsript download remote sensing data from the ESA CCI Lakes dataset for a 
# single lake according to user defined period. The zone will be defined by de boundaries 
# of the polygon defining the lake.
# https://climate.esa.int/en/projects/lakes/

# libraires
library(reticulate)

# working directory
setwd("C:/2025_EcoTwin/WP3/Data in uniform format")

py_run_file("lakes_cci_donwload1lake_by_id.py")

