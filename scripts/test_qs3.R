### Para probar distintas formas de calcular la amplitud media de la onda 3.
### 1. Media estacional de la onda 3.
### 2. Onda 3 de la media estacional.

library(ggplot2)
library(metR)
library(data.table)
library(magrittr)

tipos <- c("media fit\nmensual",  "fit\nmedia estacional")
guide_colorstrip_bottom <- function(width = 25, height = 0.5, ...) {
    guide_colorstrip(title.position = "top", title.hjust = 0.5,
                     barheight = height,
                     barwidth = width, ...)
}


theme_elio <- theme_minimal(base_size = 11) +
    theme(legend.position = "bottom", legend.box = "vertical",
          panel.spacing.y = unit(5, "mm"),
          legend.spacing = unit(2, "mm"),
          plot.margin = grid::unit(rep(3, 4), "mm"),
          legend.title = element_blank())
theme_set(theme_elio)


ncep <- ReadNetCDF("DATA/NCEP/hgt.mon.mean_sub.nc", var = c(gh = "hgt")) %>%
    setnames("level", "lev")

qs.ncep <- ncep[, FitQsWave(gh, 3), by = .(lat, lev, date)] %>%
    .[, lapply(.SD, mean),
      by = .(lat, lev, season(date))] %>%
    .[, tipo := tipos[1]]

qs.ncep.mean <- ncep[, lapply(.SD, mean),
                     by = .(lon, lat, lev, season(date))] %>%
    .[, FitQsWave(gh, 3), by = .(lat, lev, season)] %>%
    .[, tipo := tipos[2]]

qs.ncep <- rbindlist(list(qs.ncep.mean, qs.ncep))

qs.ncep[, tipo := factor(tipo, levels = tipos)]


breaks <- scales::fullseq(c(0, 70), 10)

ggfun <- function(field, name) {
    g <- ggplot(field, aes(lat, lev)) +
        geom_contour_fill(aes(z = amplitude), breaks = breaks) +
        scale_y_level(limits = c(1000, 10), name = "nivel") +
        scale_x_latitude(trans = "reverse", limits = c(0, -90), name = "latitud") +
        scale_fill_viridis_c(guide = guide_colorstrip_bottom(),
                             breaks = breaks, direction = 1) +
        facet_grid(season ~ tipo) +
        labs(title = "Distintas formas de calcular la amplitud de la onda 3",
             subtitle = name)
    ggsave(plot = g, file = paste0("test.qs.", name, ".png"), width = 540/72, height = 600/72,
           dpi = 72)
}

ggfun(qs.ncep, "ncep")



sp <- ReadNetCDF("DATA/SPEEDY/attm-Control.nc", var = "gh") %>%
    .[lat <= 0]

qs.sp <- sp[, FitQsWave(gh, 3), by = .(lat, lev, date)] %>%
    .[, lapply(.SD, mean),
      by = .(lat, lev, season(date))] %>%
    .[, tipo := tipos[1]]

qs.sp.mean <- sp[, lapply(.SD, mean),
                 by = .(lon, lat, lev, season(date))] %>%
    .[, FitQsWave(gh, 3), by = .(lat, lev, season)] %>%
    .[, tipo := tipos[2]]

qs.sp <- rbindlist(list(qs.sp.mean, qs.sp))

qs.sp[, tipo := factor(tipo, levels = tipos)]


breaks <- scales::fullseq(c(0, 70), 10)

ggfun(qs.sp, "speedy")
