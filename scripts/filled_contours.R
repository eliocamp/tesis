
## Intento de hacer contornos rellenos. anda casi perfecto.
## Dos problemas:
##  1) Al cambiar el sistema de coordenadas no se puede hacer "zoom" para no
##     mostrar la "trampa"
##  2) Los contornos cerrados internos a veces quedan tapados por contornos
##     más grandes.
library(ggplot2)
library(reshape2)
library(data.table)

# Datos para probar
ncep <- readRDS("DATA/NCEP/ncep.Rds")
ncep.sub <- ncep[lev == 300 & date == date[1]]
ncep.sub[, gh.z := Anomaly(gh), by = .(lat)]
ncep.sub <- ncep.sub[lat < 0 & lev == 300]


gdata <-  RepeatLon(ncep.sub[lat < 0, .(lon, lat, gh.z)])

# Expando los límites en una unidad en todas direcciones.
dx <- gdata[, lon - shift(lon), by = lat]$V1[2]
dy <- gdata[, lat - shift(lat), by = lon]$V1[2]
range.data <- as.data.table(sapply(gdata, range))
extra <- as.data.table(rbind(
    expand.grid(lat = c(range.data$lat[2] + dy, range.data$lat[1] - dy),
                lon = unique(gdata$lon)),
    expand.grid(lat = unique(gdata$lat),
                lon = c(range.data$lon[1] - dx, range.data$lon[2] + dx))))

# Y le doy un valor muy bajo.
binwidth = 100
extra[, gh.z := range.data$gh.z[1] - binwidth*1.5]

gdata2 <- rbind(gdata, unique(extra))

g <- ggplot(gdata2, aes(lon, lat, z = gh.z)) +
    stat_contour(geom = "polygon", aes(fill = ..level..), binwidth = binwidth) +
    geom_contour(data = gdata, binwidth = binwidth) +
    map.SH.3

# Con coodernadas cartesianas sale relativamente bien, salvo el problema 2.
g + coord_cartesian(xlim = c(0, 360), ylim = c(-87, -10), expand = F)

# En coordenadas polares (o cualquier otra), aparecen ambos problemas.
g + coord_map(projection = "stereographic", orientation = c(-90, 0, 0))
