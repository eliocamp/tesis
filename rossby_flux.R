# Datos de ejemplo para probar
ncep <- readRDS("DATA/NCEP/ncep.Rds")
ncep.sub <- ncep[lev == 300 & date == unique(ncep$date)[1]]

ncep.sub[, gh.z := Anomaly(gh), by = .(lat)]

flux <- ncep.sub[, WaveFlux(gh.z, u, v, lon, lat, lev), by = date]

flux[abs(lat) < 10, w.x := NA]

s <- 200
g <- ggplot(RepeatLon(flux[Mag(w.y, w.x) > 0.01 & date == date[1]]), aes(lon, lat)) +
    geom_contour(aes(z = gh.z, color = ..level..),
                 data = RepeatLon(ncep.sub[date == date[1]])) +
    map.SH.countries +
    ylim(c(-80, -10)) +
    scale_x_longitude(ticks = 10, sec.axis = dup_axis(),
                      limits = c(180 - 50, 180 + 50)) +
    scale_size_continuous(range = c(0, 15), guide = "none") +
    coord_map(projection = "conic", lat0 = -50) +
    scale_color_gradient2(low = muted("blue"), high = muted("red"), name = "Z*") +
    facet_grid(~date)

LabelContours(g) +
    geom_segment(aes(xend = lon + s*w.x, yend = lat + s*w.y),
                 arrow = arrow(length = unit(0.2, "lines"),
                               angle = 20))



