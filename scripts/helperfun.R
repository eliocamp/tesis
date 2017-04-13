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

    # Suavizo.
    cut <- max(smooth, res)
    notsmooth <- m[, .N, by = group][N < cut + 4, group]
    m <- m[!(group %in% notsmooth)]   # saco los grupos muy chicos
    m[, MO := max(order), by = group]
    m[order != 1 & order != MO,
      `:=`(long = zoo::rollmean(long, smooth, fill = "extend"),
           lat = zoo::rollmean(lat, smooth, fill = "extend")),
      by = group]

    # Bajo la resolución.
    m[, keep := c(1, rep(0, res)), by = group]
    m <- m[order == 1 | order == MO | keep == 1, .SD, by = group]

    return(m)
}

geom_map2 <- function(map) {
    # Un geom_path con defaults copados para agregar mapas
    g <- geom_path(data = map, aes(long, lat, group = group),
                   color = "black",
                   inherit.aes = F, size = 0.2)
    return(g)
}


scale_x_longitude <- function(ticks = 60, ...) {
    scale_x_continuous(breaks = seq(0, 360 - ticks, by = ticks),
                       labels = c(seq(0, 180, by = ticks), seq(-180 + ticks, 0 - ticks, by = ticks)),
                       ...)
}


scale_y_longitude <- function(ticks = 60, ...) {
    scale_y_continuous(breaks = seq(0, 360 - ticks, by = ticks),
                       labels = c(seq(0, 180, by = ticks), seq(-180 + 60, 0, by = ticks)),
                       ...)
}

coord_map_polar <- coord_map("stereographic", orientation = c(-90,0, 60),
                             ylim = c(-90, -20))

RepeatLon <- function(x) {
    # Repite la longitud, para cerrar los contornos en un plot polar
    # Entra:
    #   x: un data.table con datos espaciales. La longitud tiene que
    #      estar en convención 0:360 y llamarse 'lon'.
    # Sale:
    #   un data.table igual que x
    border <- x[lon == min(lon), ]
    border$lon <- 360 + min(x$lon)
    rbind(x, border)
}


FitQsWave <- function(x, n = 1) {
    # Calcula los parámetros de números de onda. Se lleva bien con n vector
    # y data.table.
    # Entra:
    #   x: vector de entrada
    #   n: vector con los números de onda a calcular
    # Sale:
    #   una lista con la amplitud, la fase, la varianza explicada y el número
    #   de onda.
    f <- fft(x)
    f <- f/length(f)
    amp <- Mod(f)*2
    fase <- Arg(f)
    r <- amp^2/(2*sd(x)^2)
    n <- n + 1

    cols <- c("amplitude", "phase", "r2", "k")
    ret <- list(amp[n], fase[n], r[n], n - 1)
    names(ret) <- cols
    return(ret)
}

# qs_fit_m <- memoise(qs_fit)


ReadNetCDF <- function(file, vars) {
    # Usa la librería netcdf para leer archivos y organiza todo en un data.table
    # Entra:
    #   file: la ruta del archivo
    #   vars: las variables a leer
    # Sale:
    #   un elemento de clase data.table con las variables en cada columna

    library(ncdf4)
    library(data.table)
    ncfile <- nc_open(file)

    # Leo las dimensiones
    dims <- names(ncfile$dim)
    ids <- vector()
    dimensions <- list()
    for (i in seq_along(dims)) {
        dimensions[[dims[i]]] <- ncvar_get(ncfile, dims[i])
        ids[i] <- ncfile$dim[[i]]$id
    }
    names(dims) <- ids


    # Leo la primera variable para luego hacer melt y obtener el data.table
    # al que luego le agrego las otras variables
    var1 <- ncvar_get(ncfile, vars[1])
    order <- ncfile$var[[vars[1]]]$dimids
    dimensions <- dimensions[dims[as.character(order)]]
    dimnames(var1) <- dimensions
    nc <- melt(var1, varnames = names(dimensions), value.name = vars[1])
    setDT(nc)
    if (length(vars) > 1) {
        nc[, c(vars[-1]) := lapply(vars[-1], ncvar_get, nc = ncfile)]    # otras variables
    }
    # Dejemos todo prolijo antes de cerrar.
    nc_close(ncfile)
    return(nc)
}


# "Similar"
`%~%` <- function(x, target) {
    x <- abs(x - target)
    return(x == min(x))
}

`%b%` <- function(x, limits) {
    # Operador "between"
    return(x >= min(limits) & x <= max(limits))
}



# Salteado
jumpby <- function(vector, by) {
    vector[seq(1, length(vector), by = by)]
}

# Repetir vector en negativo
fill_neg <- function(vector) {
    c(-vector[order(-vector)], vector)
}


library("scales")
reverselog_trans <- function(base = 10) {
    trans <- function(x) -log(x, base)
    inv <- function(x) base^(-x)
    trans_new(paste0("reverselog-", format(base)), trans, inv,
              log_breaks(base = base),
              domain = c(1e-100, Inf))
}


# Nombres de los meses en español
month.abb <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
               "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
names(month.abb) <- as.character(1:12)



# Asigna estaciones del año
asign_season <- function(month) {
    seasons <- c("Verano", "Verano", rep(c("Otoño", "Invierno", "Primavera"), each = 3), "Verano")
    return(factor(seasons[month], levels = c("Verano", "Otoño", "Invierno", "Primavera")))
}

# Para hacer anomalías.
Anomaly <- function(x) {
    as.numeric(scale(x, scale = F))
}

# Para interpolación en data table
Interpolate.DT <- function(z, x, y, yo = unique(y), xo = unique(x), ...){
    int <- akima::interp(x = x, y = y, z = z, yo = yo, xo = xo, ...)
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



# Función para agregar etiquetas a contornos
# Uso: primero crear un objeto ggplot con todo el plot
# luego agregar un geom_text con data = make_labels(g)

minvar <- function (x, y){
    counts = length(x)
    xdiffs = diff(x)
    ydiffs = diff(y)
    avgGradient = ydiffs/xdiffs
    squareSum = avgGradient * avgGradient
    variance = (squareSum - (avgGradient * avgGradient) / counts / counts)
    variance = c(9999999, variance) #99999 pads this so the length is same as original and the first values are not selected
    return(variance == min(variance))
}

make_labels <- function(g, step = 2) {
    tmp3 <- as.data.table(ggplot_build(g)$data[[1]])
    tmp3[, var := minvar(x, y), by = group]
    return(tmp3[var == T][seq(1, .N, by = step)])
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

# genera coeficiente y error estandar de regresión lineal. Para uso en data.table
lmcoef <- function(formula) {
    a <- summary(fastLm(formula = formula))
    list(estimate  = a$coefficients[2, 1],
         se        = a$coefficients[2, 2])
}



DivideTimeseries <- function(g, x, n = 2, xlab = "x", ylab = "y") {
    # Función que agarra un plot de timeseries de ggplot  y lo divide en paneles
    # respetando que todos tengan la misma escala y comportándose bien con los
    # estadísticos generados.
    # Entra:
    #   g: un ggplot
    #   x: el rango del eje x  (este es un hack medio feo, estaría bueno sacarlo
    #      automáticamente del plot)
    #   n: el número de paneles
    #   xlab: el nombre del eje x
    #   ylab: el nombre del eje y (de nuevo, estaría bueno sacarlo del plot)
    # Sale:
    #   un plot.
    # Por ahora (y posiblemente no cambie en el futuro cercano) no es muy
    # versatiil y seguramente no funcione con plots medianamente complejos.
    M <- max(x)
    m <- min(x)
    step <- (M - m)/n
    g <- g + labs(x="", y="") + theme(axis.title = element_blank())
    library(grid)
    library(gridExtra)
    plots <- list()

    for (i in 1:n) {
        if (i == 1) {
            pl <- ggplot_gtable(ggplot_build(g))
            leg <- which(sapply(pl$grobs, function(x) x$name) == "guide-box")
            try(legend.new <- pl$grobs[[leg]])
        }
        pl <- g + coord_cartesian(xlim = as.Date(c(m + step*(i-1), m + i*step))) +
            theme(axis.title = element_blank()) + guides(color = FALSE)
        pl <- ggplot_gtable(ggplot_build(pl))
        plots[[i]] <- pl
    }
    if (exists("legend.new")) {
        plots[[n + 1]] <- leg
    }
    grid.arrange(grobs = plots, ncol = 1, heights = c(rep(10, n), 2), bottom = xlab, left = ylab)
}


theme_elio <- theme_minimal() +
    theme(legend.position = "bottom")


ReadNCEP <- function(file, var, levs = T, date.fun = "hours") {
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
    date     <- ymd_hms("1800-01-01 00:00:00") + date.fun(date)

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


ConvertLongitude <- function(lon, from = 360) {
    # Pasa la longitud entre convenciones.
    # Entra:
    #   lon: un vector de longitudes
    #   from: la convención desde la cual se convierte
    #   (360 = 0:360, 180 = -180:180)
    # Sale:
    #   un vector con la longitud convertida a la otra convención.
    # Ojo que no hay ningún chequeo de los argumentos. Si se pasa un vector
    # en convención 0:360 y se le dice que está en -180:180, lo "convierte"
    # igual y tira cualquier batata.
    if (from == 360) {
        lon <- ifelse(lon <= 180, lon, lon - 360)
    } else if (from == 180) {
        lon <- ifelse(lon <= 180 & lon >= 0, lon, lon + 360)
    }
}

