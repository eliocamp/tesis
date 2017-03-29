# Hace medias mensuales a partir del  reanálisis de ozono
library(data.table)
library(lubridate)
library(fields)
library(ncdf4)

month.abb <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
               "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
# Lee .nc
file <- "MSR-2.nc"
ncfile <- nc_open(file)
o3 <- ncvar_get(ncfile, "Average_O3_column")
lat <- ncvar_get(ncfile, "latitude")
lon <- ncvar_get(ncfile, "longitude")
time <- ncvar_get(ncfile, "time")
time <- ymd("1970-01-15") + months(time)
dimnames(o3) <- list(lon, lat, as.character(time))

# medias por cada mes
o3_mean <- array(dim = c(720, 361, 12))
for (m in 1:12) {
  o3_mean[, , m] <- apply(o3[, , month(time) == m], c(1, 2), FUN = mean, na.rm = T)
}
dimnames(o3_mean) <- list(lon, lat, month.abb)

# bajo resolución a 1 grado
ds = 1
x = seq(-180+ds, 180, by = ds)
y = seq(-90, 90, by = ds)
grid = list(x = x, y = y)

o3_small <- array(dim = c(length(x), length(y), 12))
dimnames(o3_small) <- list(x, y, month.abb)
for (m in 1:12) {
  int <- interp.surface.grid(list(x = lon, y = lat, z = o3_mean[, , m]), grid)
  o3_small[, , m] <- int$z
}

# pasa a formato long
o3 <- as.data.table(melt(o3_small, value.name = "Ozono", varnames = c("lon", "lat", "month")))

# guardo en disco
fwrite(o3, "o3_mean.csv")
