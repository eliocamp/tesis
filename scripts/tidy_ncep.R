# Baja resolución y hace "manejable" reanálisis de NCEP
library(data.table)
library(lubridate)
library(fields)

library(compiler)
library(magrittr)
# library(akima)
source("scripts/helperfun.R")    # para ReadNCEP() y InterpolateNCEP()
enableJIT(3)

# Listado de variables, sus nombres y los archivos.
# (variables atmosféricas)
variables      <- c("hgt", "air", "uwnd", "vwnd", "olr")
variables.name <- c("gh", "t", "u", "v", "olr")
basedir        <- "DATA/NCEP/"
files          <- c("hgt.mon.mean.nc",
                    "air.mon.mean.nc",
                    "uwnd.mon.mean.nc",
                    "vwnd.mon.mean.nc") %>%
    paste0(basedir, .)

# Grilla para interpolar.
lon.sp <- ncvar_get(nc_open("DATA/SPEEDY/attm.nc"), "lon")
lat.sp <- ncvar_get(nc_open("DATA/SPEEDY/attm.nc"), "lat")

# Hacemos las cosas para cada archivo.
for (i in seq_along(files)) {
    message(paste0("Variable: ", variables.name[i]))
    # Leo el campo
    file  <- files[i]
    field <- ReadNCEP(files[i], variables[i])

    # Interpolo a resolución speedy.
    field.small <- InterpolateNCEP(field, lon.sp, lat.sp)

    # Paso a formato long. Si es el primero, uso melt, sino, simplemente lo
    # pongo como vector (es más rápido).
    if (i == 1) {
        ncep <- as.data.table(melt(field.small, value.name = variables.name[i]))
    } else {
        ncep[, (variables.name[i]) := c(field.small)]
    }
}

ncep[, date1 := as.Date(date[1], format = "%Y-%m-%d"), by = .(date)] %>%
    .[, date := NULL] %>%
    setnames("date1", "date")

# Guardo todo.
saveRDS(ncep, file = paste0(basedir, "ncep.Rds"), compress = "gzip")
remove(ncep)
# Antes calculaba la media climatológica mensual y lo guardaba, pero eso mejor
# lo dejo para después.


###  Otras variables

# OLR
file <- paste0(basedir, "olr.mon.mean_sub.nc")
olr  <- ReadNCEP(file, "olr", levs = F) %>%
    melt(value.name = "olr") %>%
    setDT() %>%
    .[, date1 := as.Date(date[1]), by = date] %>%
    .[, date := NULL] %>%
    setnames("date1", "date")
saveRDS(olr, file = paste0(basedir, "olr.Rds"))
remove(olr)

# SST
file <- paste0(basedir, "sst_sub.nc")
sst  <- ReadNCEP(file, "sst", levs = F, date.fun = "days") %>%
    melt(value.name = "sst") %>%
    setDT() %>%
    .[, date1 := as.Date(date[1]), by = date] %>%
    .[, date := NULL] %>%
    setnames("date1", "date")

## Agrego máscara de océanos (esto debería hacerse en el tidy_ncep.R)
library(maptools)
library(maps)

make_mask <- function(lat, lon) {
    seamask <- map("world2", fill=TRUE, col = "transparent", plot = F)
    IDs <- sapply(strsplit(seamask$names, ":"), function(x) x[1])
    seamask <- map2SpatialPolygons(seamask, IDs = IDs,
                                   proj4string = CRS("+proj=longlat +datum=WGS84"))

    points <- SpatialPoints(expand.grid(lon, lat),
                            proj4string = CRS(proj4string(seamask)))
    sea <-  is.na(over(points, seamask))
    return(sea)
}
lat <- unique(sst$lat)
lon <- unique(sst$lon)
sea <- make_mask(lat, lon)
sst[, sea := sea]

saveRDS(sst, file = paste0(basedir, "sst.Rds"))
remove(sst)