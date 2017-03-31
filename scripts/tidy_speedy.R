# Lee y le da formato a la salida de Speedy

library(data.table)
library(lubridate)
library(fields)
library(ncdf4)
source("scripts/helperfun.R")

# Valores medios.
file <- "DATA/SPEEDY/attm.nc"
ncfile <- nc_open(file)
vars <- c("gh", "u", "v", "psi", "temp")
speedy <- ReadNetCDF(file, vars)

# Selecciono sólo el sector que me importa y le pongo formato a la fecha.
speedy <- speedy[lat < 0]
speedy[, time := as.Date("1985-01-01 00:00:00") + time/24]
setnames(speedy, "time", "date")
setnames(speedy, "temp", "t")
setindex(speedy, lon, lat, lev, date)

# Guardo una versión en binario y otra en csv
save(speedy, file = "DATA/SPEEDY/speedy.Rda")
fwrite(speedy, "DATA/SPEEDY/todo_speedy.csv")