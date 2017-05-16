# Flujos de actividad de onda.
# Adaptado de https://github.com/marisolosman/Reunion_Clima/blob/master/WAF/Calculo_WAF.ipynb y
# Takata y Nakamura 2001


# Datos de ejemplo para probar
ncep <- readRDS("DATA/NCEP/ncep.Rds")
ncep.sub <- ncep[lev == 300 & date == date[1]]
gh <- ncep.sub$gh.z
lon <- ncep.sub$lon
lat <- ncep.sub$lat
u <- ncep.sub$u
v <- ncep.sub$v


# Acá empezaría la función.

WaveFlux <- function(gh, u, v, lon, lat, lev) {

    # Parámetros
    g  <- 9.81
    a <- 6371000
    p0 <- 100000    # normalizo a 100hPa

    dt <- data.table(lon = lon, lat = lat, gh = gh, u = u, v = v)
    dt[, f := 2*pi/(3600*24)*sin(lat*pi/180)]
    dt[, `:=`(psi = g/f*gh,
              u.mean = mean(u),
              v.mean = mean(v))]

    dt[, `:=`(psi.dx = Derivate(psi, lon),
              psi.dxx = Derivate(psi, lon, 2)), by = lat]

    dt[, `:=`(psi.dy = Derivate(psi, lat),
              psi.dyy = Derivate(psi, lat, 2),
              psi.dxy = Derivate(psi.dx, lat)), by = lon]

    flux <- dt[, {
        wind <- sqrt(u.mean^2 + v.mean^2)

        xu <- psi.dx^2      - psi*psi.dxx
        xv <- psi.dx*psi.dy - psi*psi.dxy
        yv <- psi.dy^2      - psi*psi.dyy

        coslat <- cos(lat*pi/180)
        coeff <- lev*100/p0/(2*wind*a^2)

        w.x <- coeff*(u.mean/coslat*xu + v.mean*xv)
        w.y <- coeff*(u.mean/xv   + v.mean*coslat*yv)

        list(lon = lon, lat = lat,
             w.x = w.x, w.y = w.y)
    }]

}
scale <- 5000
ggplot(flux, aes(lon, lat)) +
    geom_contour(aes(z = gh), data = dt) +
    geom_segment(aes(xend = lon + w.x*scale, yend = lat + w.y*scale)) +
    ylim(c(-90, -20)) + xlim(c(0, 360))
