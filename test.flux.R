library(metR)
library(data.table)
library(ggplot2)

guide_colorbar_bottom <- guide_colorbar(title.position = "top", title.hjust = 0.5,
                                        barheight = unit(0.5, "lines"),
                                        barwidth = unit(8, "lines"))


theme_elio <- theme_minimal() +
    theme(legend.position = "bottom")
theme_set(theme_elio)


stream <- ReadNetCDF("DATA/NCEP/srteam.mon.mean.nc")
setnames(stream, "level", "lev")

stream[, psi.z := Anomaly(psi), by = .(lat, lev, date)]

stream[, c("psi.z.dx", "psi.z.dy") := Derivate(psi.z ~ lon + lat, bc = c("cyclic", "none")),
       by = date]
stream[, c("psi.z.dxx", "psi.z.dyy") := Derivate(psi.z ~ lon + lat, order = 2,
                                                 bc = c("cyclic", "none")),
       by = date]
stream[, psi.z.dxy := Derivate(psi.z.dx ~ lon + lat, bc = "none")[[2]],
       by = .(date)]

ggplot(stream[date == date[1]], aes(lon, lat)) +
    geom_contour(aes(z = psi.z.dxy)) +
    geom_arrow(aes(dx = psi.z.dx, dy = psi.z.dy), scale = 0.0000008, skip = 3,
               arrow.size = 0.2)

p <- 250*100
a <- 6371000

stream[, c("fx", "fy") := list(p*(psi.z.dx^2 - psi.z*psi.z.dxx)/(cos(lat*pi/180)*2000*a^2),
                               p*(psi.z.dx*psi.z.dy - psi.z*psi.z.dxy)/(2000*a^2))]

ggplot(stream[month(date) == 9][date == date[1]], aes(lon, lat)) +
    stat_contour_fill(aes(z = psi.z), exclude = 0) +
    geom_path(data = map_data("world2"), aes(long, lat, group = group),
              color = "black", size = 0.3) +
    geom_arrow(aes(dx = fx, dy = fy), scale = 10, skip = 2,
               arrow.size = 0.2) +
    scale_fill_divergent(name = "Anomalía zonal de función corriente",
                         guide = guide_colorbar_bottom) +
    scale_y_latitude() + scale_x_longitude() +
    coord_quickmap()
