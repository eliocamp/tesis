# Lee los datos de sst y guarda una versión simétricamente zonal.

library(ncdf4)
library(data.table)    # soy un adicto a data.table
source("scripts/readctl.R")    # ¿y este script de dónde salió? ¿lo escribí yo?

dir <- "speedy_ver41_ws_clim_var_2015/data/bc/t30zonal/clim/"
descriptor <- paste0(dir, "sst_7908clim.t30.sea.ctl")
outfile <- paste0(dir, "sst_7908clim.t30.sea.grd")

field <- MakeZonal(descriptor)

writeBin(field, outfile, size = 4, endian = "big")

