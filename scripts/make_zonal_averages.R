# Lee los datos de sst y guarda una versión simétricamente zonal.

library(ncdf4)
library(data.table)    # soy un adicto a data.table
source("scripts/ReadCtl.R")    # ¿y este script de dónde salió? ¿lo escribí yo?

MakeZonal <- function(descriptor) {
    field <- ReadCtl(descriptor, hd.rm = T)
    dims <- dim(field)
    field <- as.data.table(melt(field))
    colnames(field) <- c("lon", "lat", "lev", "var", "month", "value")

    field[, value.z := mean(value[value > -999]), by = .(lat, lev, var, month)]
    field[value < -99, value.z := min(value)]
    field[, value := NULL]
    field <- c(array(field$value.z,  dim = dims))    # creo que no tiene sentido

    # Tengo que agregarle los headers.
    outfield <- c(ReadCtl(descriptor = descriptor, hd.rm = F))
    outfield[outfield != outfield[1]] <- field
    return(outfield)
}

dir <- "speedy_ver41_ws_clim_var_2015/data/bc/t30_zonal/clim/"
descriptor <- paste0(dir, "sst_7908clim.t30.sea.ctl")
outfile <- paste0(dir, "sst_7908clim.t30.sea.grd")

field <- MakeZonal(descriptor)

writeBin(field, outfile, size = 4, endian = "big")

