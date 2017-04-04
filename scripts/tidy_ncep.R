# Baja resolución y hace "manejable" reanálisis de NCEP
library(data.table)
library(lubridate)
library(fields)
library(ncdf4)
library(compiler)
library(magrittr)
# library(akima)
source("scripts/helperfun.R")
enableJIT(3)


# Defino funciones de ayuda
ReadNCEP <- function(file, var, levs = T, date.fun = "hours") {
    # Lee archivos nc de NCEP y guarda todo en un array con las dimensiones
    # bien puestas.
    # Entra:
    #   file: ruta del archivo
    #   var: nombre de la variable a leer (al pedo, en realidad, porque estos
    #        archivos sólo tienen una variable)
    #   levs: ¿la variable tiene varios niveles?
    #   date.fun: la función para modificar el date.
    # Sale:
    #   un array de 4 dimensiones (lon, lat, lev, date) nombradas y en período
    #   de tiempo necesario.
    ncfile <- nc_open(file)
    temp <- ncvar_get(ncfile, var)
    lat <- ncvar_get(ncfile, "lat")
    lon <- ncvar_get(ncfile, "lon")
    date <- ncvar_get(ncfile, "time")
    date.fun <- match.fun(date.fun)
    date <- ymd_hms("1800-01-01 00:00:00") + date.fun(date)

    if (levs) {
        lev <- ncvar_get(ncfile, "level")
        dimnames(temp) <- list(lon = lon, lat = lat, lev = lev, date = as.character(date))
        temp <- temp[, , , year(date) > 1984 & year(date) < 2016]
    } else {
        dimnames(temp) <- list(lon = lon, lat = lat, date = as.character(date))
        temp <- temp[, , year(date) > 1984 & year(date) < 2016]
    }
    nc_close(ncfile)
    return(temp)
}

InterpolateNCEP <- function(field, lon, lat, verbose = F) {
    # Interpolación bilineal.
    # Entra:
    #   fiel: un campo como sale de ReadNCEP
    #   lon: grilla de longitud
    #   lat: grilla de latitud
    #   verbose: ¿te lleno de mensajes para demostrar que no estoy tildado?
    # Sale:
    #   un array de 4 dimensiones (lon, lat, lev, date) nombradas
    grid <- list(x = lon, y = lat)
    lev <- dimnames(field)$lev
    date <- dimnames(field)$date
    progress.bar <- txtProgressBar(min = 0, max = length(date),
                                   style = 3)

    # Array "allocated" (¿re loca?) donde guardar el campo interpolado
    field.small <- array(dim = c(length(lon), length(lat), length(lev),
                                 length(date)))
    # Hago la interpolación para cada fecha y cada nivel.
    # Nota: esto tarda bastante y podría paralelizarse, pero hacerlo es un
    # bolonqui y no vale la pena para algo que se corre 1 o 2 veces.
    for (t in 1:length(date)) {
        for (l in 1:length(lev)) {
            int <- interp.surface.grid(list(x = lon, y = lat,
                                            z = field[, , l, t]), grid)
            field.small[, , l, t] <- int$z
        }
        if (verbose) {
            setTxtProgressBar(progress.bar, t)
        }
    }
    close(progress.bar)
    dimnames(field.small) <- list(lon = lon, lat = lat, lev = lev,
                                  date = as.character(date))
    return(field.small)
}


# Listado de variables, sus nombres y los archivos.
# (variables atmosféricas)
variables      <- c("hgt", "air", "uwnd", "vwnd", "olr")
variables.name <- c("gh", "t", "u", "v", "olr")
basedir        <- "DATA/NCEP/"
files          <- c("hgt.mon.mean_sub.nc",
                    "air.mon.mean_sub.nc",
                    "uwnd.mon.mean_sub.nc",
                    "vwnd.mon.mean_sub.nc") %>%
    paste0(basedir, .)

# Grilla para interpolar.
lon.sp         <- ncvar_get(nc_open("DATA/SPEEDY/attm.nc"), "lon")
lat.sp         <- ncvar_get(nc_open("DATA/SPEEDY/attm.nc"), "lat")
lat.sp         <- lat.sp[lat.sp <= 0]

# Hacemos las cosas para cada archivo.
for (i in seq_along(files)) {
    message(paste0("Variable: ", variables.name[i]))
    # Leo el campo
    file <- files[i]
    field <- ReadNCEP(files[i], variables[i])

    # Interpolo a resolución speedy.
    field.small <- InterpolateNCEP(field, lon.sp, lat.sp, verbose = T)

    # Paso a formato long. Si es el primero, uso melt, sino, simplemente lo
    # pongo como vector (es más rápido).
    if (i == 1) {
        ncep <- as.data.table(melt(field.small, value.name = variables.name[i]))
    } else {
        ncep[, (variables.name[i]) := c(field.small)]
    }
}

# Guardo todo.
saveRDS(ncep, file = paste0(basedir, "ncep.Rds"), compress = "gzip")
remove(ncep)
# Antes calculaba la media climatológica mensual y lo guardaba, pero eso mejor
# lo dejo para después.


###  Otras variables

# OLR
file <- paste0(basedir, "olr.mon.mean_sub.nc")
olr <- ReadNCEP(file, "olr", levs = F) %>%
    melt(value.name = "olr") %>%
    setDT()
saveRDS(olr, file = paste0(basedir, "olr.Rds"))
remove(olr)

# SST
file <- paste0(basedir, "sst_sub.nc")
sst <- ReadNCEP(file, "sst", levs = F, date.fun = "days") %>%
    melt(value.name = "sst") %>%
    setDT()
saveRDS(olr, file = paste0(basedir, "sst.Rds"))
