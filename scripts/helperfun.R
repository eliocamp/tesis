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
    m[, group := as.numeric(group)]
    m[, id := as.numeric(id)]

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

geom_map2 <- function(map, size = 0.2, color = "black") {
    # Un geom_path con defaults copados para agregar mapas
    g <- geom_path(data = map, aes(long, lat, group = group),
                   inherit.aes = F, color = color, size = size)
    return(g)
}

#
# geom_arrow <- function(mapping, scale, step = 1, min = 0, arrow.size = 0.2, arrow.angle = 14, ...) {
#     # Geom para graficar flechas.
#     # Entra:
#     #   aes requeridos: vx y vy, la velocidad en x e y respectivamente
#     #   scale: escala
#     #   step: para no mostra todas
#     #   min: mínima magnitud a mostrar
#     #   arrow.size: tamaño de la flecha
#     #   arrow.angle: ángulo de la flecha
#     # Sale:
#     #   un geom_spoke
#     v <- deparse(mapping$vy)
#     u <- deparse(mapping$vx)
#     angle.string <- paste0("atan2(", v, ", ", u, ")")
#     # step
#     angle.string <- paste0("JumpBy(", angle.string, ", ", step, ", NA)")
#     # min
#     angle.string <- paste0("ifelse(sqrt(", v, "^2 + ", u, "^2) >", min, ", ", angle.string,
#                            ", NA)")
#
#     radius.string <- paste0("sqrt(", v, "^2 + ", u, "^2)*", scale)
#
#     aes.temp <- aes_string(angle = angle.string, radius = radius.string)
#
#     mapping$radius <- aes.temp$radius
#     mapping$angle <- aes.temp$angle
#     mapping$vy <- NULL
#     mapping$vx <- NULL
#
#     geom_spoke(mapping = mapping,
#                arrow = arrow(angle = arrow.angle, length = unit(arrow.size, "lines")),
#                ...)
# }
#
# scale_x_longitude <- function(ticks = 60, name = "", ...) {
#     scale_x_continuous(name = name, expand = c(0, 0),
#                        breaks = seq(0, 360 - ticks, by = ticks),
#                        labels = c(seq(0, 180, by = ticks),
#                                   seq(-180 + ticks, 0 - ticks, by = ticks)),
#                        ...)
# }
#
#
# scale_y_longitude <- function(ticks = 60, name = "", ...) {
#     scale_y_continuous(name = name, expand = c(0, 0),
#                        breaks = seq(0, 360 - ticks, by = ticks),
#                        labels = c(seq(0, 180, by = ticks),
#                                   seq(-180 + ticks, 0 - ticks, by = ticks)),
#                        ...)
# }
#
#
#
# scale_color_divergent <- function(low = muted("blue"), high = muted("red"), binwidth = NA, ...) {
#     # Escala divergente con defaults más razonables.
#     if (!is.na(binwidth)) {
#         breaks <- function(x){
#             c(seq(x[1], 0 - binwidth, by = binwidth), seq(0, x[2], by = binwidth))
#         }
#         return(scale_color_gradient2(low = low, high = high, breaks = breaks, ...))
#     } else {
#         return(scale_color_gradient2(low = low, high = high, ...))
#     }
# }
#
# scale_fill_divergent <- function(low = muted("blue"), high = muted("red"), binwidth = NA, ...) {
#     # Escala divergente con defaults más razonables.
#     if (!is.na(binwidth)) {
#         breaks <- function(x){
#             c(seq(x[1], 0 - binwidth, by = binwidth), seq(0, x[2], by = binwidth))
#         }
#         return(scale_fill_gradient2(low = low, high = high, breaks = breaks, ...))
#     } else {
#         return(scale_fill_gradient2(low = low, high = high, ...))
#     }
# }
#
#
# coord_map_polar <- coord_map("stereographic", orientation = c(-90,0, 60),
#                              ylim = c(-90, -20))
#
# RepeatLon <- function(x) {
#     # Repite la longitud, para cerrar los contornos en un plot polar
#     # Entra:
#     #   x: un data.table con datos espaciales. La longitud tiene que
#     #      estar en convención 0:360 y llamarse 'lon'.
#     # Sale:
#     #   un data.table igual que x
#     border <- x[lon == min(lon), ]
#     border$lon <- 360 + min(x$lon)
#     rbind(x, border)
# }
#
#
# FitQsWave <- function(x, n = 1) {
#     # Calcula los parámetros de números de onda. Se lleva bien con n vector
#     # y data.table.
#     # Entra:
#     #   x: vector de entrada
#     #   n: vector con los números de onda a calcular
#     # Sale:
#     #   una lista con la amplitud, la fase, la varianza explicada y el número
#     #   de onda.
#     f <- fft(x)
#     f <- f/length(f)
#     amp <- Mod(f)*2
#     fase <- -Arg(f)
#
#     # Hago que la fase esté entre 0 y 2/n*pi
#     fase[fase < 0] <- fase[fase < 0] + 2*pi
#     fase <- fase/(seq_along(fase) - 1)
#
#     r <- amp^2/(2*sd(x)^2)
#     n <- n + 1
#
#     cols <- c("amplitude", "phase", "r2", "k")
#     ret <- list(amp[n], fase[n], r[n], n - 1)
#     names(ret) <- cols
#     return(ret)
# }
#
# BuildQsField <- function(x, amplitude, phase, k) {
#     amplitude*cos((x - phase)*k)
# }
#
#
# ReadNetCDF <- function(file, vars = NULL, list.vars = F) {
#     # Usa la librería netcdf para leer archivos y organiza todo en un data.table
#     # Entra:
#     #   file: la ruta del archivo
#     #   vars: las variables a leer. Si es NULL, lee todas.
#     #   list.vars: leer los datos o sólo listar las variables y dimensiones
#     # Sale:
#     #   si list.vars == F, un elemento de clase data.table con las variables
#     #   en cada columna.
#     #   si list.vars == T, una lista con el nombre de las variables y las
#     #   dimensiones.
#
#     library(ncdf4)
#     library(data.table)
#     ncfile <- nc_open(file)
#
#     if (is.null(vars)) {
#         vars <- names(ncfile$var)
#     }
#     # Leo las dimensiones
#     dims <- names(ncfile$dim)
#     dims <- dims[dims != "nbnds"]
#     ids <- vector()
#     dimensions <- list()
#     for (i in seq_along(dims)) {
#         dimensions[[dims[i]]] <- ncvar_get(ncfile, dims[i])
#         ids[i] <- ncfile$dim[[i]]$id
#     }
#     names(dims) <- ids
#
#
#     if ("time" %in% names(dimensions)) {
#         date.unit <- ncfile$dim$time$units
#         date.unit <- strsplit(date.unit, " since ", fixed = TRUE)[[1]]
#         library(lubridate)
#         date.fun <- match.fun(date.unit[1])
#         dimensions[["time"]] <- as.character(ymd_hms(date.unit[2]) + date.fun(dimensions[["time"]]))
#     }
#
#     if (list.vars == T) {
#         r <- list(vars = vars, dimensions = dimensions)
#         return(r)
#     }
#
#     # Leo la primera variable para luego hacer melt y obtener el data.table
#     # al que luego le agrego las otras variables
#     var1 <- ncvar_get(ncfile, vars[1], collapse_degen = FALSE)
#     order <- ncfile$var[[vars[1]]]$dimids
#     dimensions <- dimensions[dims[as.character(order)]]
#     dimnames(var1) <- dimensions
#     nc <- melt(var1, varnames = names(dimensions), value.name = vars[1])
#     setDT(nc)
#     if ("time" %in% names(dimensions)) {
#         nc[, date := as.Date(time[1]), by = time]
#         nc[, time := NULL]
#     }
#     if (length(vars) > 1) {
#         nc[, c(vars[-1]) := lapply(vars[-1], ncvar_get, nc = ncfile)]    # otras variables
#     }
#     # Dejemos todo prolijo antes de cerrar.
#     nc_close(ncfile)
#     return(nc)
# }
#
#
#
# # "Similar"
# `%~%` <- function(x, target) {
#     x <- abs(x - target)
#     return(x == min(x))
# }
#
# `%b%` <- function(x, limits) {
#     # Operador "between"
#     return(x >= min(limits) & x <= max(limits))
# }
#
#
#
# # Salteado
# JumpBy <- function(x, by, fill = c("skip", NA)) {
#     if (is.na(fill[1])) {
#         x[-seq(1, length(x), by = by)] <- NA
#     } else {
#         x <- x[seq(1, length(x), by = by)]
#     }
#     return(x)
# }
#
# # Para compatibilidad de código viejo.
# jumpby <- function(...) {
#     JumpBy(...)
# }

# Repetir vector en negativo
fill_neg <- function(vector) {
    c(-vector[order(-vector)], vector)
}

#
# library("scales")
# reverselog_trans <- function(base = 10) {
#     trans <- function(x) -log(x, base)
#     inv <- function(x) base^(-x)
#
#     trans_new(paste0("reverselog-", format(base)), trans, inv,
#               log_breaks(base = base),
#               domain = c(1e-100, Inf))
# }


# Nombres de los meses en español
month.abb_sp <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
                  "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
names(month.abb_sp) <- as.character(1:12)



# # Asigna estaciones del año
# asign_season <- function(month) {
#     seasons <- c("Verano", "Verano", rep(c("Otoño", "Invierno", "Primavera"), each = 3), "Verano")
#     return(factor(seasons[month], levels = c("Verano", "Otoño", "Invierno", "Primavera")))
# }

# # Para hacer anomalías.
# Anomaly <- function(x) {
#     as.numeric(scale(x, scale = F))
# }

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
acf.sig <- function(x, lag.max=0.3*length(x), alpha = 0.05, method=c("anderson","large.lag", "salas"), sided="one") {
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
        ret <- data.frame(lag=0:lag.max, acf=autocor, sig.cut=sigupp)
    } else if (sided == "two"){
        a <- alpha/2
        q <- qnorm(a, lower.tail=F)
        sigupp <- e+sqrt(var)*q
        siginf <- e-sqrt(var)*q
        ret <- data.frame(lag=0:lag.max, acf=autocor, upp.sig.cut=sigupp, low.sig.cut=siginf)
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


source("scripts/geom_contourlabel.R")
# source("scripts/stat_fill_contour.R")

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


# DivideTimeseries <- function(g, x, n = 2, xlab = "x", ylab = "y") {
#     # Función que agarra un plot de timeseries de ggplot  y lo divide en paneles
#     # respetando que todos tengan la misma escala y comportándose bien con los
#     # estadísticos generados.
#     # Entra:
#     #   g: un ggplot
#     #   x: el rango del eje x  (este es un hack medio feo, estaría bueno sacarlo
#     #      automáticamente del plot)
#     #   n: el número de paneles
#     #   xlab: el nombre del eje x
#     #   ylab: el nombre del eje y (de nuevo, estaría bueno sacarlo del plot)
#     # Sale:
#     #   un plot.
#     # Por ahora (y posiblemente no cambie en el futuro cercano) no es muy
#     # versatil y seguramente no funcione con plots medianamente complejos.
#     M <- max(x)
#     m <- min(x)
#     step <- (M - m)/n
#     g <- g + labs(x="", y="") + theme(axis.title = element_blank())
#     library(grid)
#     library(gridExtra)
#     plots <- list()
#
#     for (i in 1:n) {
#         if (i == 1) {
#             pl <- ggplot_gtable(ggplot_build(g))
#             leg <- which(sapply(pl$grobs, function(x) x$name) == "guide-box")
#             try(legend.new <- pl$grobs[[leg]])
#         }
#         pl <- g + coord_cartesian(xlim = as.Date(c(m + step*(i-1), m + i*step))) +
#             theme(axis.title = element_blank()) + guides(color = FALSE)
#         pl <- ggplot_gtable(ggplot_build(pl))
#         plots[[i]] <- pl
#     }
#     if (exists("legend.new")) {
#         plots[[n + 1]] <- legend.new
#     }
#     grid.arrange(grobs = plots, ncol = 1, heights = c(rep(10, n), 2), bottom = xlab, left = ylab)
# }
#
#
# theme_elio <- theme_minimal() +
#     theme(legend.position = "bottom")


ReadNCEP <- function(file, var, levs = T, date.fun = "hours", since = "1800-01-01 00:00:00") {
    # Lee archivos nc de NCEP y guarda todo en un array con las dimensiones
    # bien puestas.
    # Entra:
    #   file: ruta del archivo
    #   var: nombre de la variable a leer (al pedo, en realidad, porque estos
    #        archivos sólo tienen una variable)
    #   levs: ¿la variable tiene varios niveles?
    #   date.fun: la función para modificar el date.
    # Sale:
    #   un array de 4 dimensiones (lon, lat, lev, date) nombradas y en período
    #   de tiempo necesario.
    library(ncdf4)
    ncfile   <- nc_open(file)
    temp     <- ncvar_get(ncfile, var)
    lat      <- ncvar_get(ncfile, "lat")
    lon      <- ncvar_get(ncfile, "lon")
    date     <- ncvar_get(ncfile, "time")
    date.fun <- match.fun(date.fun)
    date     <- ymd_hms(since) + date.fun(date)

    if (levs) {
        lev <- ncvar_get(ncfile, "level")
        dimnames(temp) <- list(lon = lon, lat = lat, lev = lev, date = as.character(date))
        temp <- temp[, , , year(date) > 1984 & year(date) < 2016]
    } else {
        dimnames(temp) <- list(lon = lon, lat = lat, date = as.character(date))
        temp <- temp[, , year(date) > 1984 & year(date) < 2016]
    }
    nc_close(ncfile)
    return(temp)
}

InterpolateNCEP <- function(field, lon, lat, cores = 3) {
    # Interpolación bilineal.
    # Entra:
    #   fiel: un campo como sale de ReadNCEP
    #   lon: grilla de longitud
    #   lat: grilla de latitud
    #   cores: cantidad de núcleos para usar la paralelización
    # Sale:
    #   un array de 4 dimensiones (lon, lat, lev, date) nombradas
    lon.original <- as.numeric(dimnames(field)$lon)
    lat.original <- as.numeric(dimnames(field)$lat)
    grid         <- list(x = lon, y = lat)
    lev          <- dimnames(field)$lev
    date         <- dimnames(field)$date

    # Hago la interpolación para cada fecha y cada nivel.
    # Nota: perdí más tiempo para averiguar cómo paralelizarlo que el que
    # ahorré con el aumento de velocidad.
    library(doParallel)
    library(abind)
    registerDoParallel(cores)
    datebind <- function(...) {
        abind(..., along = 4)
    }

    levbind <- function(...) {
        abind(..., along = 3)
    }

    field.small <- foreach(t = seq_along(date), .combine = "datebind") %:%
        foreach(l = seq_along(lev), .combine = "levbind") %dopar% {
            int <- fields::interp.surface.grid(list(x = lon.original, y = lat.original,
                                                    z = field[, , l, t]), grid)$z

        }
    stopImplicitCluster()
    dimnames(field.small) <- list(lon = lon, lat = lat, lev = lev,
                                  date = as.character(date))
    return(field.small)
}

ReadERA <- function(file, var, levs = T, date.fun = "hours", since = "1800-01-01 00:00:00") {
    # Lee archivos nc de NCEP y guarda todo en un array con las dimensiones
    # bien puestas.
    # Entra:
    #   file: ruta del archivo
    #   var: nombre de la variable a leer (al pedo, en realidad, porque estos
    #        archivos sólo tienen una variable)
    #   levs: ¿la variable tiene varios niveles?
    #   date.fun: la función para modificar el date.
    # Sale:
    #   un array de 4 dimensiones (lon, lat, lev, date) nombradas y en período
    #   de tiempo necesario.
    library(ncdf4)
    ncfile   <- nc_open(file)
    temp     <- ncvar_get(ncfile, var)
    lat      <- ncvar_get(ncfile, "latitude")
    lon      <- ncvar_get(ncfile, "longitude")
    date     <- ncvar_get(ncfile, "time")
    date.fun <- match.fun(date.fun)
    date     <- ymd_hms(since) + date.fun(date)
    years    <- year(date)

    if (levs) {
        lev <- ncvar_get(ncfile, "level")
        dimnames(temp) <- list(lon = lon, lat = lat, lev = lev, date = as.character(date))
        temp <- temp[, , , years > 1984 & years < 2016]
    } else {
        dimnames(temp) <- list(lon = lon, lat = lat, date = as.character(date))
        temp <- temp[, , years > 1984 & years < 2016]
    }
    nc_close(ncfile)
    return(temp)
}
#
# ConvertLongitude <- function(lon, from = 360) {
#     # Pasa la longitud entre convenciones.
#     # Entra:
#     #   lon: un vector de longitudes
#     #   from: la convención desde la cual se convierte
#     #   (360 = 0:360, 180 = -180:180)
#     # Sale:
#     #   un vector con la longitud convertida a la otra convención.
#     # Ojo que no hay ningún chequeo de los argumentos. Si se pasa un vector
#     # en convención 0:360 y se le dice que está en -180:180, lo "convierte"
#     # igual y tira cualquier batata.
#     if (from == 360) {
#         lon <- ifelse(lon <= 180, lon, lon - 360)
#     } else if (from == 180) {
#         lon <- ifelse(lon <= 180 & lon >= 0, lon, lon + 360)
#     }
# }


# Derivate <- function(x, y, order = 1, bc = "cyclic") {
#     # Calcula derivada centrada 1da o 2da
#     # Entra:
#     #  x: la variable a derivar
#     #  y: la variable que deriva
#     #  order: orden de la derivada (1 o 2)
#     #  bc: condiciones de borde (por ahora, sólo impliementadas cíclicas o nada)
#     # Sale:
#     #  un vector de la misma longitud que x e y con la derivada
#     # ¡Asume que la grilla es uniforme!
#     library(data.table)
#     N <- length(x)
#
#     d <- y[2] - y[1]
#
#     if (order == 1) {
#         dxdy <- (x[c(2:N, 1)] - x[c(N, 1:(N-1))])/(2*d)
#
#     } else if (order == 2) {
#         dxdy <- (x[c(2:N, 1)] + x[c(N, 1:(N-1))] - 2*x)/d^2
#     }
#     if (bc != "cyclic") {
#         dxdy[c(1, N)] <- NA
#     }
#
#     return(dxdy)
# }


WaveFlux <- function(gh, u, v, lon, lat, lev) {
    # Flujos de actividad de onda. Adaptado de
    # https://github.com/marisolosman/Reunion_Clima/blob/master/WAF/Calculo_WAF.ipynb y
    # Takata y Nakamura 2001
    # Entra:
    #   gh: campo de altura geopotencial (anomalía zonal)
    #   u: velocidad zonal
    #   v: velocidad meridional
    #   lon: longitudes
    #   lat: latitudes
    #   lev: nivel
    # Sale:
    #   una lista con longitud, latitud, y las componentes zonales y
    #   meridionales del flujo de actividad de onda.
    g  <- 9.81
    a <- 6371000
    p0 <- 100000    # normalizo a 100hPa

    # Todo en una data.table para que sea más cómodo.
    dt <- data.table(lon = lon, lat = lat,
                     lonrad = lon*pi/180, latrad = lat*pi/180,
                     gh = gh, u.mean = u, v.mean = v)
    setkey(dt, lat, lon)
    dt[, f := 2*pi/(3600*24)*sin(latrad)]
    dt[, psi := g/f*gh]

    # Derivadas
    dt[, `:=`(psi.dx  = Derivate(psi, lonrad),
              psi.dxx = Derivate(psi, lonrad, 2)), by = lat]
    dt[, `:=`(psi.dy  = Derivate(psi, latrad, bc = "none"),
              psi.dyy = Derivate(psi, latrad, 2, bc = "none"),
              psi.dxy = Derivate(psi.dx, latrad, bc = "none")), by = lon]

    # Cálculo del flujo (al fin!)
    flux <- dt[, {
        wind <- sqrt(u.mean^2 + v.mean^2)

        xu <- psi.dx^2      - psi*psi.dxx
        xv <- psi.dx*psi.dy - psi*psi.dxy
        yv <- psi.dy^2      - psi*psi.dyy

        coslat <- cos(latrad)
        coeff <- lev*100/p0/(2*wind*a^2)

        w.x <- coeff*(u.mean/coslat*xu + v.mean*xv)
        w.y <- coeff*(u.mean*xv + v.mean*coslat*yv)

        list(lon = lon, lat = lat,
             w.x = w.x, w.y = w.y)}
        ]
    return(flux)
}

#
# Percentile <- function(x) {
#     ecdf(x)(x)
# }
#
# Mag <- function(x, y) {
#     sqrt(x^2 + y^2)
# }
#
#
#
# EOF <- function(z, lon, lat, date, n = 1, return = c("index", "field")) {
#     # Calcula EOF del campo z.
#     # Entra:
#     #   z: campo (en vector)
#     #   lon: vector de longitudes
#     #   lat: vector de latitudes
#     #   date: vector de fechas
#     #   n: número de valores principales a calcular
#     #   return: devolver el índice o el campo
#     # Sale:
#     #   un data.table con el campo o el índice.
#
#     field <- data.table(lon, lat, date, z)
#     field[, z.w := z*sqrt(cos(lat*pi/180))]    # peso.
#
#     g <- dcast(field, lon + lat ~ date, value.var = "z.w")
#     eof <- svd::propack.svd(as.matrix(g[,-(1:2)]), neig = n)
#
#     if (return[1] == "index") {
#         eof <- as.data.table(eof$v)
#         eof <- cbind(eof, data.table(date = colnames(g[, -c(1, 2)])))
#     } else {
#         eof <- as.data.table(eof$u)
#         eof <- cbind(eof, g[, c(1, 2)])
#     }
#
#     return(eof)
# }

polar.SH <- list(coord_polar(),
                 scale_y_latitude(limits = c(-90, 0)),
                 scale_x_longitude())

guide_colorbar_bottom <- function(width = 25, height = 0.5, ...) {
    guide_colorbar(title.position = "top", title.hjust = 0.5,
                   barheight = height,
                   barwidth = width, ...)
}

scale_s_map <- function() list(scale_y_latitude(), scale_x_longitude())

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

MakeBreaks <- function(binwidth = NULL, exclude = NULL) {
    # If no parameters set, use pretty bins
    if (is.null(binwidth)) {
        breaks <- function(range) {
            b <- pretty(range, 10)
            b[!(b %in% exclude)]
        }
    } else {
        breaks <- function(range) {
            b <- scales::fullseq(range, binwidth)
            b[!(b %in% exclude)]
        }
    }
}


coriolis <- function(lat) {
    2*2*pi/(3600*24)*sin(lat*pi/180)
}

beta <- function(lat, a = 6731) {
    a <- a*1000
    2*2*pi/(3600*24)*cos(lat*pi/180)/a
}



yearmonth <- function(date, day = 1) {
   months <- lubridate::month(date)
   years <- lubridate::year(date)
   lubridate::ymd(paste(years, months, day, sep = "-"))
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


cache.file <- function(file, expression) {
    if (file.exists(file)) {
        message("Reading data from file.")
        return(readRDS(file))
    } else {
       r <- eval(expression)
       saveRDS(r, file = file)
       return(r)
    }
}
