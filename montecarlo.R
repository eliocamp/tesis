library(metR)
library(data.table)
library(ggplot2)
library(magrittr)

gh <- ReadNetCDF("DATA/NCEP/hgt.mon.mean_sub.nc") %>%
    setnames(c("level", "hgt"), c("lev", "gh"))
gh <- gh[lev == 300]

qs <- gh[, FitQsWave(gh, 3), by = .(lat, date)]

index <- qs[, .(amplitude = mean(amplitude)), by = date]

index[, rank := frank(-amplitude, ties.method = "first"), by = .(month(date))]

gh[, gh.a := Anomaly(gh), by = .(lon, lat, lev, month(date))]

bootfun2 <- function(x, n = 10) {
    N <- 1:length(x)
    function(i) {
        i <- sample(N, n, replace = TRUE)
        mean(x[i])
    }
}

boot <- gh[,
           setNames(as.list(quantile(sapply(1:100, bootfun2(gh.a)),
                                     c(0.025, 0.975), names = F)),
                    c("min", "max")),
           , by = .(lat, lon, month(date))] %>%
    .[, `:=`(min = predict(loess(min ~ lon, span = 1/3)),
             max = predict(loess(max ~ lon, span = 1/3))),
      by = .(lat, month)]


dates <- index[rank <= 10, date]
comp <- gh[date %in% dates, .(gh = mean(gh.a)), by = .(lat, lon, month(date))]
comp <- comp[boot, on = c("lat", "lon", "month")]
comp[, sig := !(gh %b% c(min, max)), by = .(lon, lat, month)]
ggplot(comp, aes(lon, lat)) +
    stat_contour(aes(z = gh, color = ..level..)) +
    geom_point(data = comp[sig == TRUE], size = 0.1) +
    facet_wrap(~month)

ggplot(comp[lon == 200 & month == 1], aes(lat)) +
    geom_line(aes(y = gh)) +
    geom_line(aes(y = min)) +
    geom_line(aes(y = max)) +
    geom_point(aes(y = ifelse(gh %b% c(min, max), 0, 25)))
