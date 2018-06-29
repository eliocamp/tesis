#!/bin/bash

################################################################################
# NCEP/NCAR Reanalysis Monthly Means and Other Derived Variables
# From https://www.esrl.noaa.gov/psd/data/gridded/data.ncep.reanalysis.html
# Downloads monthly means

# Air temperature
wget ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis.derived/pressure/air.mon.mean.nc -P "DATA/NCEP Reanalysis/"

# Geopotential height
wget ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis.derived/pressure/hgt.mon.mean.nc -P "DATA/NCEP Reanalysis/" 

# U wind
wget ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis.derived/pressure/uwnd.mon.mean.nc -P "DATA/NCEP Reanalysis/"  

# V wind
wget ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis.derived/pressure/vwnd.mon.mean.nc -P "DATA/NCEP Reanalysis/" 

# Surface height
wget ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis/surface/hgt.sfc.nc -P "DATA/NCEP Reanalysis/" 

# Surface pressure
wget ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis.derived/surface/pres.mon.mean.nc -P "DATA/NCEP Reanalysis/" 

# MSL pressure
wget ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis.derived/surface/slp.mon.mean.nc -P "DATA/NCEP Reanalysis/" 


# Steamfunction
wget ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis.derived/sigma/psi.mon.mean.nc -P "DATA/NCEP Reanalysis/" 