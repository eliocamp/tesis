gh.f <- gh[lev == 300 & date == date[1], ]
gh.f <- ncep[lev == 300 & date == date[1], .(lon, lat, gh)]
a <- 6371000

gh.f[, gh := Anomaly(gh), by = lat]
gh.f[, gh.dxx := Derivate(gh, lon*pi/180, 2)/(a*cos(lat*pi/180))^2, by = lat]
gh.f[, gh.dyy := Derivate(gh, lat*pi/180, 2, bc = "none")/a^2, by = lon]
gh.f[, gh.lap := gh.dxx + gh.dyy]

ggplot(gh.f, aes(lon, lat)) +
    stat_fill_contour(aes(z = gh)) +
    geom_contour(aes(color = ..level.., z = gh.lap)) +
    scale_fill_divergent() +
    scale_color_divergent()



gh.f[!is.na(gh.lap) & lat < 0 , gh.l.SH := InverseLaplacian(gh.lap, lon*pi/180, lat*pi/180)]
gh.f[!is.na(gh.lap), gh.l := InverseLaplacian(gh.lap, lon*pi/180, lat*pi/180)]


ggplot(gh.f, aes(gh.l.SH, gh.l)) + geom_point()

ggplot(gh.f[lat < 0], aes(lon, lat)) +
    stat_fill_contour(aes(z = gh)) +
    geom_contour(aes(z = gh.l, color = ..level..)) +
    scale_fill_divergent() +
    scale_color_divergent()


ggplot(gh.f[lat < 0], aes(lon, lat)) +
    stat_fill_contour(aes(z = gh)) +
    geom_contour(aes(z = gh.l.SH, color = ..level..)) +
    scale_fill_divergent() +
    scale_color_divergent()



InverseLaplacian <- function(v, x, y) {
    a <- 6371000
    field <- data.table(x, y, v)

    field[, c("k", "l", "v.hat") := fft2d(v, x, y)]
    field[, lp := l*2*pi/a]
    field[, kp := k*2*pi/(a*cos(y*pi/180))]

    field[, v.hat := -v.hat/(k^2 + l^2)]
    # field[, v.hat := -v.hat/(kp^2 + lp^2)]
    field[, v.hat := c(0, v.hat[2:.N])]

    field[, c("k", "l", "v.inv") := fft2d(v.hat, k, l, inverse = TRUE)]

    return(Re(field$v.inv))
}

fft2d <- function(v, x, y, inverse = FALSE) {

    field <- as.matrix(reshape2::dcast(data.table(v, x, y),
                             x ~ y, value.var = "v"))

    field.fft <- fft(field[, -1], inverse = inverse)

    dimnames(field.fft) <- list(
        k = 1:nrow(field.fft) - 1,
        l = 1:ncol(field.fft) - 1)

    field.fft <- setDT(melt(field.fft, value.name = "f"))

    with(field.fft, list(k, l, f))
}


# Prueba

test.fun <- function(x, y) {
    sin(x) + cos(y)
}


test <- setDT(expand.grid(
    x = seq(0, 2*pi, length.out = 30),
    y = seq(0, 2*pi, length.out = 30)
))

test[, v := test.fun(x, y), by = .(x, y)]
test[, v.lap1 := -v]
test[, v.dxx := Derivate(Derivate(v, x, 1), x), by = y]
test[, v.dyy := Derivate(Derivate(v, y, 1), y), by = x]
test[, v.lap2 := v.dxx + v.dyy]

test[, v.inv1 := InverseLaplacian(v.lap1, x, y)]
test[, v.inv2 := InverseLaplacian(v.lap2, x, y)]

ggplot(test, aes(x, y)) +
    geom_contour(aes(z = as.numeric(scale(v)))) +
    geom_contour(aes(z = as.numeric(scale(v.inv1))), color = "red")

ggplot(test, aes(v, v.inv1)) +
    geom_point() +
    xlab("Variable original") +
    ylab("Laplaciano analítico invertido")

ggplot(test, aes(v.lap1, v.lap2)) +
    geom_point() +
    xlab("Variable original") +
    ylab("Laplaciano analítico invertido")


ggplot(test, aes(v, v.inv2)) +
    geom_point() +
    xlab("Variable original") +
    ylab("Laplaciano numérico invertido")


summary(lm(v ~ v.inv, data = test))
||