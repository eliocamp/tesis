# Lee y le da formato a la salida de Speedy

library(data.table)
library(lubridate)
library(fields)
library(ncdf4)
source("scripts/helperfun.R")


TidySpeedy <- function(file) {
    # file <- "DATA/SPEEDY/attmnoice.nc"
    vars <- c("gh", "u", "v", "psi", "temp")
    speedy.clim <- ReadNetCDF(file, vars)
    speedy.clim <- speedy.clim[lat < 20]
    speedy.clim[, time := as.Date("1985-01-01 00:00:00") + time/24]
    setnames(speedy.clim, "time", "date")
    setnames(speedy.clim, "temp", "t")
    setindex(speedy.clim, lon, lat, lev, date)

    return(speedy.clim)
}


files <- c("DATA/SPEEDY/attm.nc",
           "DATA/SPEEDY/attm106.nc",
           "DATA/SPEEDY/attmzon.nc",
           "DATA/SPEEDY/attmnoice.nc",
           "DATA/SPEEDY/attmnoland.nc")
out.files <- c("speedy.Rds",
               "speedy.clim.Rds",
               "speedy.zonal]Rds",
               "speedy.noice.Rds",
               "speedy.noland.Rds")

for (f in seq_along(files)) {
    saveRDS(TidySpeedy(files[f]),
            file = paste0("DATA/SPEEDY/", out.files[f]))
}
