# stat y geom para anotar contornos en la parte más "chata"


minvar <- function (x, y){
    counts = length(x)
    xdiffs = diff(x)
    ydiffs = diff(y)
    avgGradient = ydiffs/xdiffs
    squareSum = avgGradient * avgGradient
    variance = (squareSum - (avgGradient * avgGradient) / counts / counts)
    variance = c(9999999, variance) #99999 pads this so the length is same as original and the first values are not selected
    return(variance == min(variance, na.rm = T))
}


StatContourLabel <- ggproto("StatContourLabel", Stat,
                       required_aes = c("x", "y", "z"),
                       default_aes = aes(label = ..level..),

                       compute_group = function(data, scales, bins = NULL, binwidth = NULL,
                                                breaks = NULL, complete = FALSE, na.rm = FALSE,
                                                step = 2, exclude = NA, include = NA) {
                           # If no parameters set, use pretty bins
                           if (is.null(bins) && is.null(binwidth) && is.null(breaks)) {
                               breaks <- pretty(range(data$z), 10)
                           }
                           # If provided, use bins to calculate binwidth
                           if (!is.null(bins)) {
                               binwidth <- diff(range(data$z)) / bins
                           }
                           # If necessary, compute breaks from binwidth
                           if (is.null(breaks)) {
                               breaks <- fullseq(range(data$z), binwidth)
                           }

                           breaks.keep <- breaks[seq(1, length(breaks), by = step)]
                           breaks.keep <- breaks.keep[!(breaks.keep %in% exclude)]
                           breaks.keep <- c(breaks.keep, include)
                           breaks.keep <- breaks.keep[!is.na(breaks.keep)]

                           contours <- ggplot2:::contour_lines(data, breaks.keep, complete = complete)
                           contours.dt <- as.data.table(contours)
                           contours.dt[, var := minvar(x, y), by = .(piece)]

                           as.data.frame(contours.dt[var == T][, head(.SD, 1), by = piece])
                       }
)

stat_contourlabel <- function(mapping = NULL, data = NULL,
                         geom = "text", position = "identity",
                         ...,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
    layer(
        data = data,
        mapping = mapping,
        stat = StatContourLabel,
        geom = geom,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            na.rm = na.rm,
            ...
        )
    )
}

geom_contourlabel <- function(...) {
    list(stat_contourlabel(geom = "label", fill = "white", label.r = unit(0, "lines"),
                      label.padding = unit(0.04, "lines"), color = NA, ...),
        stat_contourlabel(...))
}
