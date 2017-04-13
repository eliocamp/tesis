# Lee y le da formato a la salida de Speedy

library(data.table)
library(lubridate)
library(fields)
library(ncdf4)
source("scripts/helperfun.R")

# Valores medios.
file <- "DATA/SPEEDY/attm.nc"
vars <- c("gh", "u", "v", "psi", "temp")
speedy <- ReadNetCDF(file, vars)

# Selecciono sólo el sector que me importa y le pongo formato a la fecha.
speedy <- speedy[lat < 20]
speedy[, time := as.Date("1985-01-01 00:00:00") + time/24]
setnames(speedy, "time", "date")
setnames(speedy, "temp", "t")
setindex(speedy, lon, lat, lev, date)

# Guardo una versión en binario y otra en csv
saveRDS(speedy, file = "DATA/SPEEDY/speedy.Rds")


file <- "DATA/SPEEDY/attm106.nc"
vars <- c("gh", "u", "v", "psi", "temp")
speedy.clim <- ReadNetCDF(file, vars)
speedy.clim <- speedy.clim[lat < 20]
speedy.clim[, time := as.Date("1985-01-01 00:00:00") + time/24]
setnames(speedy.clim, "time", "date")
setnames(speedy.clim, "temp", "t")
setindex(speedy.clim, lon, lat, lev, date)
saveRDS(speedy.clim, file = "DATA/SPEEDY/speedy.clim.Rds")