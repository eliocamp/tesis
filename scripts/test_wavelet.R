library(meteoR)
library(data.table)
library(ggplot2)
library(magrittr)

ncep <- ReadNetCDF("DATA/NCEP/hgt.mon.mean_sub.nc")
test <- ncep[level == 300 & date == date[1] & lat %~% -55]

ggplot(test, aes(lon, hgt)) + geom_line()

library(WaveletComp)

test.copy <- copy(test)
test.copy[, lon := lon + 360]
test <- rbind(test, test.copy)

w <- analyze.wavelet(test, "hgt", dt = 1)

wt.image(w)

w <- WaveletTransform(test$hgt, dt = 1, upperPeriod = nrow(test))

ampl <- w$Ampl
dimnames(ampl) <- list(Period = w$Period, lon = test$lon)



ggplot(as.data.table(melt(ampl))[lon %b% c(180, 360 + 180)], aes(lon, nrow(test)/Period/2)) +
    geom_tile(aes(fill = value)) +
    scale_y_log10(name = "Número de onda zonal") +
    geom_hline(yintercept = 1, linetype = 3) +
    geom_hline(yintercept = 3, linetype = 3) +
    viridis::scale_fill_viridis(name = "Amplitud")


test <- ncep[level == 300 & date == date[1] & lat %~% -55]

PeriodicWavelet <- function(x) {
    x1 <- rep(x, 3)
    w <- WaveletTransform(x1, dt = 1, upperPeriod = length(x1))
    ampl <- w$Ampl
    dimnames(ampl) <- list(Period = w$Period, y = 1:length(x1))
    ampl <- melt(ampl[, (length(x)+1)
                      :(2*length(x)+1)])
}