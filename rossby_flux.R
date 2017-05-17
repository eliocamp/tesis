







# Datos de ejemplo para probar
ncep <- readRDS("DATA/NCEP/ncep.Rds")
ncep.sub <- ncep[lev == 300 & date == unique(ncep$date)[1:2]]

ncep.sub[, gh.z := Anomaly(gh), by = .(lat)]

flux <- ncep.sub[, WaveFlux(gh.z, u, v, lon, lat, lev), by = date]

flux[abs(lat) < 10, w.x := NA]

g <- ggplot(RepeatLon(flux[Mag(w.y, w.x) > 0.01 & date == date[1]]), aes(lon, lat)) +
    map.SH.countries +
    geom_contour(aes(z = gh.z, color = ..level..),
                 data = RepeatLon(ncep.sub[date == date[1]])) +
    ylim(c(-80, -10)) +
    scale_x_longitude(ticks = 10, sec.axis = dup_axis(),
                      limits = c(190 - 40, 190 + 40)) +
    scale_size_continuous(range = c(0, 15), guide = "none") +
    coord_map(projection = "conic", lat0 = -50) +
    scale_color_viridis() +
    facet_grid(~date) +
    # geom_arrow(aes(vx = w.x, vy = w.y))
    geom_arrow(aes(mag = Mag(w.x, w.y), angle = atan2(w.y, w.x)*180/pi))
g

g + geom_label(data = make_labels(g, step = 1), aes(x, y, label = level),
               fill = "white", label.r = unit(0, "lines"), color = NA) +
    geom_text(data = make_labels(g, step = 1), aes(x, y, label = level))

LabelContours <- function(plot, step = 1) {
    g <- make_labels(plot, step = step)
    return(plot +
        geom_label(data = g, aes(x, y, label = level),
               fill = "white", label.r = unit(0, "lines"), color = NA, ...) +
        geom_text(data = g, aes(x, y, label = level), ...))
}



test_f <- function(df) {
    colname <- names(df)
    names(df) <- c("x", "y", "z")
    Range <- as.data.frame(sapply(df, range))
    Dim <- as.data.frame(t(sapply(df, function(x) length(unique(x)))))
    arb_z = Range$z[1] - diff(Range$z)/20
    adf2 <- rbind(df,
                 expand.grid(x = c(Range$x[1] - diff(Range$x)/20, Range$x[2] + diff(Range$x)/20),
                             y = seq(Range$y[1], Range$y[2], length = Dim$y), z = arb_z),
                 expand.grid(x = seq(Range$x[1], Range$x[2], length = Dim$x),
                             y = c(Range$y[1] - diff(Range$y)/20, Range$y[2] + diff(Range$y)/20), z = arb_z))
    g <- ggplot(adf2, aes(x, y, z = z)) +
        labs(x = colname[1], y = colname[2], fill = colname[3]) +
        stat_contour(geom="polygon", aes(fill=..level..))
        # coord_cartesian(xlim=c(Range$x), ylim=c(Range$y), expand = F)
    return(g)
}

library(ggplot2); library(reshape2)
volcano3d <- melt(volcano)
names(volcano3d) <- c("xxx", "yyy", "zzz")
test_f(volcano3d) + scale_fill_gradientn(colours = terrain.colors(10))

ncep.sub <- RepeatLon(ncep.sub)

dx <- ncep.sub[, lon - shift(lon), by = lat]$V1[2]
dy <- ncep.sub[, lat - shift(lat), by = lon]$V1[2]
range.data <- as.data.table(sapply(ncep.sub, range))

extra <- as.data.table(rbind(expand.grid(lon = c(range.data$lon[1] - dx, range.data$lon[2] + dx),
                                   lat = ncep.sub$lat),
               expand.grid(lon = ncep.sub$lon,
                           lat = c(range.data$lat[1] - dy, range.data$lat[2] + dy))))
binwidth = 100
extra[, gh.z := range.data$gh.z[1] - binwidth*1.5 ]

gdata <- rbind(ncep.sub, unique(extra))

ggplot(gdata, aes(lon, lat, z = gh.z)) +
    stat_contour(geom = "polygon", aes(fill = ..level..), binwidth = binwidth) +
    # geom_contour(data = ncep.sub, binwidth = binwidth) +
    map.SH.3 +
    coord_cartesian(xlim = c(0, 360), ylim = c(-90, -10), expand = F)
