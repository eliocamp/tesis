# Baja resolución y hace "manejable" reanálisis de NCEP
library(data.table)
library(lubridate)
library(fields)
library(ncdf4)
library(compiler)
# library(akima)
source("scripts/helperfun.R")
enableJIT(3)

# Lee .nc
file <- "DATA/NCEP/hgt.mon.mean_sub.nc"
gh <- ReadNetCDF(file, "hgt")
setnames(gh, "time", "date")
gh[, date := ymd_hms("1800-01-01 00:00:00") + hours(date)]
gh <- gh[lat <= 0 & year(date) > 1984 & year(date) < 2016]

ncfile <- nc_open(file)
lev <- ncvar_get(ncfile, "level")
lat <- ncvar_get(ncfile, "lat")
lon <- ncvar_get(ncfile, "lon")
date <- ncvar_get(ncfile, "time")
date <-
# sp_levs <- ncvar_get(nc_open("attm.nc"), "lev")

# Quedarnos con 1985-2015 (no hace falta, en realidad)
gh <- ncvar_get(ncfile, "hgt")
nc_close(ncfile)
dimnames(gh) <- list(lon, lat, lev, as.character(date))
hgt <- hgt[, lat <= 0, , year(date) > 1984 & year(date) < 2016]

ncfile <- nc_open("DATA/NCEP/air.mon.mean_sub.nc")
temp <- ncvar_get(ncfile)
date <- ncvar_get(ncfile, "time")
date <- ymd_hms("1800-01-01 00:00:00") + hours(date)
nc_close(ncfile)
dimnames(temp) <- list(lon, lat, lev, as.character(date))
temp <- temp[, lat <= 0, , year(date) > 1984 & year(date) < 2016]

ncfile <- nc_open("DATA/NCEP/uwnd.mon.mean_sub.nc")
U <- ncvar_get(ncfile, "uwnd")
date <- ncvar_get(ncfile, "time")
date <- ymd_hms("1800-01-01 00:00:00") + hours(date)
nc_close(ncfile)
dimnames(U) <- list(lon, lat, lev, as.character(date))
U <- U[, lat <= 0, , year(date) > 1984 & year(date) < 2016]

ncfile <- nc_open("DATA/NCEP/vwnd.mon.mean_sub.nc")
V <- ncvar_get(ncfile, "vwnd")
date <- ncvar_get(ncfile, "time")
date <- ymd_hms("1800-01-01 00:00:00") + hours(date)
nc_close(ncfile)
dimnames(V) <- list(lon, lat, lev, as.character(date))
V <- V[, lat <= 0, , year(date) > 1984 & year(date) < 2016]

time <- time[year(time) > 1984 & year(time) < 2016]
# lev <- lev[lev %in% sp_levs]
lat <- lat[lat <= 0]

# bajo resolución a 1 grado
sp_lon <- ncvar_get(nc_open("attm.nc"), "lon")
sp_lat <- ncvar_get(nc_open("attm.nc"), "lat")
sp_lat <- sp_lat[sp_lat <= 0]
grid = list(x = sp_lon, y = sp_lat)

hgt_small <- array(dim = c(length(sp_lon), length(sp_lat), length(lev), length(time)))
for (t in 1:length(time)) {
    for (l in 1:length(lev)) {
        int <- interp.surface.grid(list(x = lon, y = lat, z = hgt[, , l, t]), grid)
        hgt_small[, , l, t] <- int$z
    }
    # message(paste0("Terminado tiempo ", as.character(t), " de ", as.character(length(time))))
}

dimnames(hgt_small) <- list(sp_lon, sp_lat, lev, as.character(time))
# remove(hgt)

temp_small <- array(dim = c(length(sp_lon), length(sp_lat), length(lev), length(time)))
for (t in 1:length(time)) {
    for (l in 1:length(lev)) {
        int <- interp.surface.grid(list(x = lon, y = lat, z = temp[, , l, t]), grid)
        temp_small[, , l, t] <- int$z
    }
    # message(paste0("Terminado tiempo ", as.character(t), " de ", as.character(length(time))))
}
dimnames(temp_small) <- list(sp_lon, sp_lat, lev, as.character(time))
# remove(temp)

U_small <- array(dim = c(length(sp_lon), length(sp_lat), length(lev), length(time)))
for (t in 1:length(time)) {
    for (l in 1:length(lev)) {
        int <- interp.surface.grid(list(x = lon, y = lat, z = U[, , l, t]), grid)
        U_small[, , l, t] <- int$z
    }
    # message(paste0("Terminado tiempo ", as.character(t), " de ", as.character(length(time))))
}
dimnames(U_small) <- list(sp_lon, sp_lat, lev, as.character(time))
# remove(U)

V_small <- array(dim = c(length(sp_lon), length(sp_lat), length(lev), length(time)))
for (t in 1:length(time)) {
    for (l in 1:length(lev)) {
        int <- interp.surface.grid(list(x = lon, y = lat, z = V[, , l, t]), grid)
        V_small[, , l, t] <- int$z
    }
    # message(paste0("Terminado tiempo ", as.character(t), " de ", as.character(length(time))))
}
dimnames(V_small) <- list(sp_lon, sp_lat, lev, as.character(time))
# remove(V)

# pasa a formato long

hgt <- as.data.table(melt(hgt_small, value.name = "gh",
                          varnames = c("lon", "lat", "lev", "time")))
# temp <- as.data.table(melt(temp_small, value.name = "temp",
# varnames = c("lon", "lat", "lev", "time")))
# U <- as.data.table(melt(U_small, value.name = "U",
#varnames = c("lon", "lat", "lev", "time")))
hgt[, temp := c(temp_small)]
hgt[, u := c(U_small)]
hgt[, v := c(V_small)]

# guardo en disco
# fwrite(hgt, "DATA/NCEP/hgt_ncep.csv")
# fwrite(temp, "DATA/NCEP/temp_ncep.csv")
# fwrite(U, "DATA/NCEP/U_ncep.csv")
# hgt <- fread("DATA/NCEP/todo_ncep.csv")
# hgt2 <- fread("DATA/NCEP/todo_ncep.csv")

hgt[, year := stringi::stri_sub(time, from = 1, to = 4)]
hgt[, month := stringi::stri_sub(time, from = 6, to = 7)]
ncep <- hgt
fwrite(hgt, "DATA/NCEP/todo_ncep.csv")
save(ncep, file = "DATA/NCEP/todo_ncep.rda")

ncep_vars <- c("gh", "temp", "u", "v")
m_ncep <- hgt[, lapply(.SD, FUN = mean), by = .(lon, lat, lev, month),
              .SDcols = ncep_vars]
m_ncep[, month := as.numeric(month)]
m_ncep[, month := month.abb[month]]
fwrite(m_ncep, "DATA/NCEP/todo_m_ncep.csv")
save(m_ncep, file = "DATA/NCEP/todo_m_ncep.rda")

remove(hgt, temp, u, htg_small, temp_small, U_small, V_small, m_ncep)

###  Otras variables


# OLR
file <- "DATA/NCEP/olr.mon.mean_sub.nc"
ncfile <- nc_open(file)
lat <- ncvar_get(ncfile, "lat")
lon <- ncvar_get(ncfile, "lon")
time <- ncvar_get(ncfile, "time")
time <- ymd_hms("1800-01-01 00:00:00") + hours(time)
olr <- ncvar_get(ncfile, "olr")
nc_close(ncfile)
dimnames(olr) <- list(lon, lat, as.character(time))


# bajo resolución a 1 grado
sp_lon <- ncvar_get(nc_open("attm.nc"), "lon")
sp_lat <- ncvar_get(nc_open("attm.nc"), "lat")
# sp_lat <- sp_lat[sp_lat <= 0]
grid = list(x = sp_lon, y = sp_lat)

olr_small <- array(dim = c(length(sp_lon), length(sp_lat), length(time)))
for (t in 1:length(time)) {
    int <- interp.surface.grid(list(x = lon, y = lat, z = olr[, ,t]), grid)
    olr_small[, ,t] <- int$z
  # message(paste0("Terminado tiempo ", as.character(t), " de ", as.character(length(time))))
}

dimnames(olr_small) <- list(sp_lon, sp_lat, as.character(time))

olr <- setDT(melt(olr_small, value.name = "olr",
                          varnames = c("lon", "lat", "time")))
save(olr, file = "DATA/NCEP/olr_todo.rda")
fwrite(olr, file = "DATA/NCEP/olr_todo.csv")

remove(olr, olr_small)

# SST

file <- "DATA/NCEP/sst_sub.nc"
ncfile <- nc_open(file)
lat <- ncvar_get(ncfile, "lat")
lon <- ncvar_get(ncfile, "lon")
time <- ncvar_get(ncfile, "time")
time <- ymd_hms("1800-01-01 00:00:00") + days(time)
sst <- ncvar_get(ncfile, "sst")
nc_close(ncfile)
dimnames(sst) <- list(lon, lat, as.character(time))


# bajo resolución a 1 grado
sp_lon <- ncvar_get(nc_open("attm.nc"), "lon")
sp_lat <- ncvar_get(nc_open("attm.nc"), "lat")
# sp_lat <- sp_lat[sp_lat <= 0]
grid = list(x = sp_lon, y = sp_lat)

sst_small <- array(dim = c(length(sp_lon), length(sp_lat), length(time)))
for (t in 1:length(time)) {
  int <- interp.surface.grid(list(x = lon, y = lat, z = sst[, ,t]), grid)
  sst_small[, ,t] <- int$z
  # message(paste0("Terminado tiempo ", as.character(t), " de ", as.character(length(time))))
}

dimnames(sst_small) <- list(sp_lon, sp_lat, as.character(time))

sst <- setDT(melt(sst_small, value.name = "sst",
                  varnames = c("lon", "lat", "time")))
save(sst, file = "DATA/NCEP/sst_todo.rda")
fwrite(sst, file = "DATA/NCEP/sst_todo.csv")

remove(sst, sst_small)
