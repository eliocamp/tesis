## Archivo con funciones

#Librerías
library(ggplot2)
library(ggforce)
library(stringi)
library(ggthemes)
library(magrittr)
library(data.table)
library(viridis)
library(lubridate)
library(directlabels)
library(akima)
library(compiler)
library(RColorBrewer)
enableJIT(0)

# Mapa
BuildMap <- function(res = 1, smooth = 1, pm = 180,
                     countries = FALSE, ...) {
    # Caraga datos de mapas y cambia el meridiano principal, suaviza y cambia
    # resolución para plotear más rápido.
    # Entra:
    #   res: resolución (cuantos puntos se eliminan entre los que quedan)
    #   smooth: factor de suavizado (ventana del promedio corrido)
    #   pm: longitud del meridiano central
    #   countries: ¿datos a nivel país?
    # Sale:
    #   un data.table con las coordenadas de cada polígono y su grupo
    library(data.table)
    if (countries) {
        library(rworldxtra)
        data("countriesLow")
        m <- countriesLow
    } else {
        library(rworldmap)
        data(coastsCoarse)
        m <- coastsCoarse
    }

    m <- as.data.table(fortify(m))
    m[, group1 := .GRP, by = group]
    m[, id1 := .GRP, by = id]
    m[, c("group", "id") := NULL]
    setnames(m, c("group1", "id1"), c("group", "id"))

    # Cambio el prime meridian.
    m2 <- copy(m)
    m2[, long := long + 360]
    m2[, group := group + max(group) + 1]
    m <- rbind(m, m2)
    m <- m[long >= pm - 180 & long <= 180 + pm]

    m[, MO := max(order), by = group]

    # Suavizo.
    if (smooth > 1) {
        cut <- max(smooth, res)
        notsmooth <- m[, .N, by = group][N < cut + 4, group]
        m <- m[!(group %in% notsmooth)]    # saco los grupos muy chicos

        m[order != 1 & order != MO,
          `:=`(long = zoo::rollmean(long, smooth, fill = "extend"),
               lat = zoo::rollmean(lat, smooth, fill = "extend")),
          by = group]
    }
    # Bajo la resolución.
    suppressWarnings(m[, keep := c(1, rep(0, res - 1)), by = group])
    m <- m[order == 1 | order == MO | keep == 1, .SD, by = group]

    return(m)
}

geom_map2 <- function(map, size = 0.2, color = "gray50") {
    # Un geom_path con defaults copados para agregar mapas
    g <- geom_path(data = map, aes(long, lat, group = group),
                   inherit.aes = F, color = color, size = size)
    return(g)
}

# Repetir vector en negativo
fill_neg <- function(vector) {
    c(-vector[order(-vector)], vector)
}


# Nombres de los meses en español
month.abb_sp <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
                  "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
names(month.abb_sp) <- as.character(1:12)



# Para interpolación en data table
Interpolate.DT <- function(z, x, y, yo = unique(y), xo = unique(x), ...){
    na <- is.na(z)
    int <- akima::interp(x = x[!na], y = y[!na], z = z[!na], yo = yo, xo = xo, ...)
    names <- c(deparse(substitute(x)),
               deparse(substitute(y)),
               deparse(substitute(z)))    # muy feo, sí
    r <- with(int, {
        grid <- expand.grid(x, y)
        r <- list(grid[,1], grid[, 2], c(z))
        names(r) <- names
        return(r)
    })
}

# Función que hace autocorrelograma y su test según Anderson o large lag.
acf.sig <- function(x, lag.max=0.3*length(x), alpha = 0.05,
                    method=c("anderson","large.lag", "salas"), sided="one") {
    autocor <- acf(x, lag.max=lag.max, plot = F)$acf
    N <- length(x)
    e <- -1/(N-1)
    if (method[1]=="anderson"){
        var <- (N-2)/(N-1)^2
    } else if (method[1]=="large.lag"){
        var <- vector()
        for (i in 1:length(autocor)){
            v <- ifelse(i==1,1/N, 1/N * (1+2*sum(autocor[1:i-1]^2)))
            var<- c(var, v)
        }
    } else if (method[1]=="salas"){
        var <- vector()

        for (i in 1:length(autocor)){
            v <- (N-1-i)/(N-i)^2
            var<- c(var, v)
            e <- -1/(N-i)
        }
    }
    if (sided=="one"){
        a <- alpha
        q <- qnorm(a, lower.tail=F)
        sigupp <- e + sqrt(var)*q
        ret <- data.table(lag=0:lag.max, acf=autocor, sig.cut=sigupp)
    } else if (sided == "two"){
        a <- alpha/2
        q <- qnorm(a, lower.tail=F)
        sigupp <- e+sqrt(var)*q
        siginf <- e-sqrt(var)*q
        ret <- data.table(lag=0:lag.max, acf=autocor, upp.sig.cut=sigupp, low.sig.cut=siginf)
    }
    ret
}



# Convierte la salida de la función fft en un formato
# legible por humanos.

convert.fft <- function(cs, sample.rate=1, full=T) {
    distance.center <- function(c) Mod(c)
    angle <- function(c) Arg(c)
    is.even <- function(x) ceiling(x/2) == x/2
    N <- length(cs)
    if (full==T){
        nyq <- ifelse(is.even(N), N/2+1, (N+1)/2)
        cs <- cs[2:nyq]
    }
    NP <- length(cs)
    cs <- cs / N # normalize

    df <- data.frame(cycle    = 1:(NP),
                     freq     = 1:(NP) * sample.rate / N,
                     per      = N/(1:(NP) * sample.rate),
                     ampl = sapply(cs, Mod),
                     delay    = sapply(cs, angle),
                     spect    = sqrt(sapply(cs, Mod)),
                     comp = cs)

    non.unique <- ifelse(is.even(NP), NP-1, NP)
    df$ampl [1:non.unique] <- df$ampl[1:non.unique]*2
    df
}

# Abreviatura para as.data.table
as.dt <- function(...) {
    as.data.table(...)
}

factor2cols <- function(x, column, factors) {
    column <- deparse(substitute(column))
    for(h in factors){
        x[, (h) := ifelse(get(column) == h, 1, 0)]
    }
    x[is.na(get(column)), (factors) := 0]
}

# convierte una fecha en formato AAAA-MM-DD en mes con factor y ordenado según estaciones
date_month2factor <- function(x) {
    factor(as.numeric(stringi::stri_sub(x, 6, 7)), levels = c(12, 1:11), ordered = T)
}

date2month <- function(x) {
    as.integer(stringr::str_sub(x, 6, 7))
}

ExtractLm <- function(model) {
    # Extrae el estimador y el error estándar de cada regressor para un modelo
    # lineal (¿o cualquier modelo?).
    # Entra:
    #   model: un modelo lineal (o no)
    # Sale:
    #   una lista con 3 vectores: el nombre de los elementos, los estimadores y
    #   el error estándar.
    a <- summary(model)
    return(list(regressor = rownames(a$coefficients),
                estimate  = unname(a$coefficients[, 1]),
                se        = unname(a$coefficients[, 2])))
}


WaveFlux <- function(psi, p = 250, a = 6371000) {
    k <- p*100/(a^2*2000)
    psi <- copy(psi)
    psi[, c("psi.dlon", "psi.dlat") := Derivate(psi.z ~ lon + lat,
                                                cyclical = c(TRUE, FALSE))] %>%
        .[, psi.ddlon := Derivate(psi.z ~ lon, cyclical = TRUE, order = 2),
          by = lat] %>%
        .[, psi.dlondlat := Derivate(psi.dlon ~ lat),
          by = lon] %>%
        .[, `:=`(f.lon = k/cos(lat*pi/180)*(psi.dlon^2 - psi.z*psi.ddlon),
                 f.lat = k*(psi.dlon*psi.dlat - psi.z*psi.dlondlat))]
    list(f.lon = psi$f.lon, f.lat = psi$f.lat)
}

polar.SH <- list(coord_polar(),
                 scale_y_latitude(limits = c(-90, 0)),
                 scale_x_longitude())

guide_colorstrip_bottom <- function(width = 25, height = 0.5, ...) {
    guide_colorstrip(title.position = "top", title.hjust = 0.5,
                     barheight = height,
                     barwidth = width, ...)
}

scale_x_longitude <- function(ticks = 60, breaks = seq(0, 360, by = ticks), ...) {
    metR::scale_x_longitude(ticks = ticks, breaks = breaks, ...)
}

scale_s_map <- function(limits.lat = c(-90, 0),
                        limits.lon = c(0, 360)) list(scale_y_latitude(limits = limits.lat),
                               scale_x_longitude(limits = limits.lon))


AddSuffix <- function(suffix = "") {
    function(string) {
        paste0(string, suffix)
    }
}

AddPreffix <- function(preffix = "") {
    function(string) {
        paste0(preffix, string)
    }
}

lev.lab <- AddSuffix(" hPa")
qs.lab <- AddPreffix("QS ")



yearmonth <- function(date, day = 1) {
    months <- lubridate::month(date)
    years <- lubridate::year(date)
    lubridate::ymd(paste(years, months, day, sep = "-"))
}

yearly <- function(date, day = 182) {
    years <- lubridate::year(date)
    d <- lubridate::ymd(paste(years, "01", "01", sep = "-"))
    yday(d) <- day
    d
}

geom_index.region <- function(data) {
    geom_rect(data = data, aes(xmin = latmin, xmax = latmax,
                               ymin = levmin, ymax = levmax),
              inherit.aes = F, linetype = 3, color = "black", fill = NA)
}


PeriodicWavelet <- function(x, k) {
    period <- length(x)/k
    x1 <- rep(x, 3)
    keep <- (length(x)+1):(2*length(x))
    res <- list()
    for (p in seq_along(period)) {
        w <- WaveletComp::WaveletTransform(x1, dt = 1, upperPeriod = period[p],
                                           lowerPeriod = period[p])
        res[[paste0("k", ".", k[p])]] <- w$Ampl[keep]*sd(x)

    }
    return(res)
}

ReconstructWavelet <- function(x, k) {
    period <- length(x)/k
    x1 <- rep(x, 3)
    keep <- (length(x)+1):(2*length(x))
    w <- WaveletComp::analyze.wavelet(data.frame(x1), make.pval = F,
                                      loess.span = 0, verbose = F)
    r <- WaveletComp::reconstruct(w, sel.period = period,
                                  plot.rec = F, verbose = F)$series$x1.r
    r[keep]
}


greater <- function(x, N) {
    r <- frank(-x, ties.method = "first")
    r <= N
}


decade <- function(year) {
    substr(year, 3, 4)
}

fsign <- function(x) {
    f <- sign(x)
    factor(ifelse(f == 0, NA, f))
}



ifelse2 <- function(x, expression, yes = NA, no = x) {
    e <- eval(parse(text = deparse(substitute(expression))), envir = environment())
    yes <- eval(parse(text = deparse(substitute(yes))), envir = environment())
    no <- eval(parse(text = deparse(substitute(no))), envir = environment())
    ifelse(e, yes, no)
}

geom_label_contour2 <- function(...) {
    list(geom_label_contour(fill = "white", label.r = unit(0, "lines"),
                            label.padding = unit(0.06, "lines"), color = NA, ...),
         geom_text_contour(..., rotate = FALSE))
}


# cache.file <- function(file, expression) {
#     if (file.exists(file)) {
#         message("Reading data from file.")
#         return(readRDS(file))
#     } else {
#         message("Evaluating expression.")
#         r <- eval(expression)
#         message("Saving data to file.")
#         saveRDS(r, file = file, compress = FALSE)
#         return(r)
#     }
# }

memoise <- function(..., cache = cache.file("/Rcache")) {
    memoise::memoise(..., cache = cache)
}

cache.file <- function (path, algo = "xxhash64")
{
    if (!dir.exists(path)) {
        dir.create(path, showWarnings = FALSE)
    }
    cache_reset <- function() {
        cache_files <- list.files(path, full.names = TRUE)
        file.remove(cache_files)
    }
    cache_set <- function(key, value) {
        saveRDS(value, file = file.path(path, key), compress = FALSE)
    }
    cache_get <- function(key) {
        readRDS(file = file.path(path, key))
    }
    cache_has_key <- function(key) {
        file.exists(file.path(path, key))
    }
    list(digest = function(...) digest::digest(..., algo = algo),
         reset = cache_reset, set = cache_set, get = cache_get,
         has_key = cache_has_key, keys = function() list.files(path))
}


mode.circular <- function(x, limits = c(0, 2/3*pi)) {
    if (length(x) > 1) {
        x1 <- c(x - limits[2], x, x + limits[2])
        keep <- (length(x) + 1):(2*length(x))
        d <- density(x1)
        y <- d$y[d$x %b% limits]
        x2 <- d$x[d$x %b% limits]
        x2[which.max(y)]
    } else {
        x
    }
}

qs.season <- function(month) {
    if (metR:::.is.somedate(month)) month <- lubridate::month(month)

    qs.seasons <- factor(c(rep("EFM", 3),
                           rep("AM", 2),
                           rep("JJ", 2),
                           rep("ASO", 3),
                           rep("ND", 2)))

    return(factor(qs.seasons[month], levels = c("EFM", "AM", "JJ",
                                                "ASO", "ND")))
}


geom_contour_back <- function(..., color = "black", size = 0.2, alpha = 0.5) {
    geom_contour2(..., color = color, size = size, alpha = alpha)
}
geom_label_contour_back <- function(...) {
    geom_text_contour(..., alpha = 0.5, size = 3, rotate = FALSE,
                      stroke = 0.2)
}

geom_contour_fine <- function(...) geom_contour(..., size = 0.4)
stat_contour_fine <- function(...) stat_contour(..., size = 0.4)

geom_cross <- function(x = 0, y = 0, ...) {
    list(geom_vline(xintercept = x, ...),
        geom_hline(yintercept = y, ...))
}

labeller.date <- function(sep = " - ") {
    function(s) {
        s <- as.Date(s)
        m <- month(s)
        y <- year(s)
        paste0(month.abb_sp[m], sep, y)
    }
}

no.zero_ <- function(x) {
    if (x == 0) return(".0")
    if (abs(x) < 1) {
        s <- ifelse(x < 0, "-", "")
        paste0(s, substr(abs(x), 2, nchar(x)))
    } else {
        x
    }
}

no.zero <- function(x) {
    sapply(seq_along(x), function(i) no.zero_(x[i]))
}

as.numeric <- function(x, ...) {
    if (is.factor(x)) x <- as.character(x)
    if (is.character(x)) {
        dec <- options("OutDec")$OutDec
        x <- stringi::stri_replace(x, ".", fixed = dec)
    }
    base::as.numeric(x, ...)
}


notify <- function(title = "title", text = NULL, time = 2) {
    time <- time*1000
    system(paste0('notify-send "', title, '" "', text, '" -t ', time, ' -a rstudio'))
}
