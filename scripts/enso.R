library(data.table)
library(metR)
source("scripts/ReadCtl.R")

# Lee los datos y les pone forma
descriptor <- "speedy_ver41_ws_clim_var_2015/data/bc/t30/anom/hadisst_anom_1_1_1870_2014_mean1979_2008.t30.ctl"
field <- ReadCtl(descriptor, hd.rm = TRUE)
dims <- dim(field)
lat <- c(-87.16, -83.47, -79.78, -76.07, -72.36, -68.65, -64.94, -61.23, -57.52,
         -53.81, -50.10, -46.39, -42.68, -38.97, -35.26, -31.54, -27.83, -24.12,
         -20.41, -16.70, -12.99,  -9.28,  -5.57,  -1.86, 1.86, 5.57, 9.28,  
         12.99,  16.70,  20.41,  24.12,  27.83,  31.54,  35.26, 38.97,  42.68,  
         46.39,  50.10,  53.81,  57.52,  61.23,  64.94,  68.65, 72.36,  76.07, 
         79.78,  83.47, 87.16)
lat <- lat[length(lat):1]
lon <- seq(0, 360 - 3.75, by = 3.75)
time <- seq.Date(as.Date("1987-01-01"),by = "1 month", length.out = 1740)
dimnames(field) <- list(lon = lon, lat = lat, lev = 0, var = "ssta", time1 = as.character(time))
field <- as.data.table(melt(field))
field[, time := lubridate::ymd(time1[1]), by = time1]
field[, time1 := NULL]

# Anomalía media en la zona de El Nio 3.4 para cada tiempo 
# Separa entre niño, niña y neutro con +-0.5°C 
field[, enso := mean(value[lat %between% c(-5, 5) & lon %between% c(190, 240)]), by = time]
field[, enso2 := cut(enso, c(-Inf, -0.5, 0.5, Inf), labels = c("Niña", "Neutro", "Niño"))]

# Anomalía de temperatura media para cada mes y longitud y latitud
# para cada estado.
field[, niña := mean(value[enso2 == "Niña"]), by = .(lon, lat, month(time))]
field[, niño := mean(value[enso2 == "Niño"]), by = .(lon, lat, month(time))]
field[, neutro := mean(value[enso2 == "Neutro"]), by = .(lon, lat, month(time))]

# Guardo las cosas. 
name <- paste0(dirname(descriptor), "/", "enso_global_pattern_nina")
WriteCtl(field[, niña], name, descriptor)

name <- paste0(dirname(descriptor), "/", "enso_global_pattern_nino")
WriteCtl(field[, niño], name, descriptor)

name <- paste0(dirname(descriptor), "/", "enso_global_pattern_neutral")
WriteCtl(field[, neutro], name, descriptor)
