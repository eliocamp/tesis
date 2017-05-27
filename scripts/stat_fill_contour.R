stat_fill_contour <- function(mapping = NULL, data = NULL,
                              geom = "polygon", position = "identity",
                              ...,
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE) {
    layer(
        data = data,
        mapping = mapping,
        stat = StatFillContour,
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

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatFillContour <- ggproto("StatFillContour", Stat,
                           required_aes = c("x", "y", "z"),
                           default_aes = aes(fill = ..int.level..),

                           compute_group = function(data, scales, bins = NULL, binwidth = NULL,
                                                    breaks = NULL, complete = FALSE, na.rm = FALSE,
                                                    exclude = NA) {
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

                               if (is.null(binwidth)) {
                                   binwidth <- diff(range(data$z)) / length(breaks)
                               }

                               breaks.keep <- breaks[!(breaks %in% exclude)]
                               # f <<- data    # debug
                               dx <- abs(diff(subset(data, y == data$y[1])$x)[1])
                               dy <- abs(diff(subset(data, x == data$x[1])$y)[1])


                               range.data <- as.data.frame(sapply(data[c("x", "y", "z")], range))

                               extra <- rbind(
                                   expand.grid(y = c(range.data$y[2] + dy, range.data$y[1] - dy),
                                               x = unique(data$x)),
                                   expand.grid(y = c(unique(data$y), range.data$y[2] + dy, range.data$y[1] - dy),
                                               x = c(range.data$x[1] - dx, range.data$x[2] + dx))
                               )

                               # Y le doy un valor muy bajo.
                               # extra$z <- range.data$z[1] - 3*binwidth
                               mean.z <- mean(data$z)
                               mean.level <- breaks.keep[breaks.keep %~% mean.z]
                               extra$z <- mean.z
                               extra$PANEL <- data$PANEL[1]
                               cur.group <- data$group[1]
                               extra$group <- data$group[1]

                               data2 <- rbind(data, extra)

                               cont <- ggplot2:::contour_lines(data2, breaks.keep, complete = complete)



                               setDT(cont)


                               co <<- copy(cont)    # debug
                               data3 <<- data2    # debug
                               cont <- CorrectFill(cont, data2, breaks)


                               i <-  which(breaks.keep == mean.level)
                               correction <- (breaks.keep[i + sign(mean.z - mean.level)] - mean.level)/2
                               # correction <- 0
                               mean.cont  <- data.frame(
                                   level = mean.level,
                                   x = c(rep(range.data$x[1], 2), rep(range.data$x[2], 2)),
                                   y = c(range.data$y[1], rep(range.data$y[2], 2), range.data$y[1]),
                                   piece = max(cont$piece) + 1,
                                   int.level = mean.level + correction)

                               mean.cont$group <- factor(paste(cur.group, sprintf("%03d", mean.cont$piece), sep = "-"))
                               cont <- rbind(cont, mean.cont)
                               co.2 <<- copy(cont)    # debug

                               areas <- cont[, .(area = abs(area(x, y))), by = .(piece)][
                                   , rank := frank(-area, ties.method = "dense")]
                               areas <- areas[, head(.SD, 1), by = piece]
                               cont <-cont[areas, on = "piece"]
                               cont[, piece := rank]
                               cont[, group := factor(paste(cur.group,
                                                            sprintf("%03d", piece), sep = "-"))]

                               cont$x[cont$x > range.data$x[2]] <- range.data$x[2]
                               cont$x[cont$x < range.data$x[1]] <- range.data$x[1]
                               cont$y[cont$y < range.data$y[1]] <- range.data$y[1]
                               cont$y[cont$y > range.data$y[2]] <- range.data$y[2]

                               cont
                           }

)


# From https://stat.ethz.ch/pipermail/r-help/2004-December/063046.html
area <- function(x, y){
    X <- matrix(c(x, y), ncol = 2)
    X <- rbind(X,X[1,])
    x <- X[,1]
    y <- X[,2]
    lx <- length(x)
    -sum((x[2:lx] - x[1:lx-1])*(y[2:lx] + y[1:lx-1]))/2
}


CorrectFill <- function(cont, data, breaks) {
    levels <- breaks
    m.level <- -levels[2] + 2*levels[1]
    M.level <- 2*levels[length(levels)] - levels[length(levels) - 1]
    levels <- c(m.level, levels, M.level)
    cont[, int.level := 0]
    pieces <- unique(cont$piece)

    data <- as.data.table(data)
    x.data <- unique(data$x)
    x.data <- x.data[order(x.data)]
    x.N <- length(x.data)
    # range.x <- range(data$x)
    y.data <- unique(data$y)
    y.data <- y.data[order(y.data)]
    y.N <- length(y.data)
    # range.y <- range(data$y)

    for (p in pieces) {
        level <- cont[piece == p, level[1]]

        i <- which(levels == level)
        cur.piece <- cont[piece == p]

        p0 <- cur.piece[x >= x.data[2] & x <= x.data[x.N-1]
                        & y >= y.data[2] & y <= y.data[y.N-1]][1]
        if (nrow(p0[!is.na(x)]) == 0) {
            inside.z <- level
        } else {
            if (p0$x %in% x.data) {
                y1 <- Closest(y.data, p0$y, sign = 1)
                y2 <- Closest(y.data, p0$y, sign = -1)
                p1 <- data[x == p0$x & y == y1]
                p2 <- data[x == p0$x & y == y2]
            } else {
                x1 <- Closest(x.data, p0$x, sign = 1)
                x2 <- Closest(x.data, p0$x, sign = -1)

                p1 <- data[x == x1 & y == p0$y]
                p2 <- data[x == x2 & y == p0$y]
            }

            if (IsInside(p1$x, p1$y, cur.piece$x, cur.piece$y)) {
                inside.z <- p1$z
            } else {
                inside.z <- p2$z
            }
        }

        correction <- (levels[i + sign(inside.z - level)] - level)/2

        cont[piece == p, int.level := level + correction]
    }
    return(cont)
}

# "Similar"
`%~%` <- function(x, target) {
    x <- abs(x - target)
    return(x == suppressWarnings(min(x)))
}

Closest <- function(x, target, sign = c(1, -1)) {
    tmp <- (x - target)*sign[1]
    tmp[tmp<0] <- NA
    x[which.min(abs(tmp))]
}

IsInside <- function(xp, yp, x, y) {
    !(sp::point.in.polygon(xp, yp, x, y) == 0)
}

# v3d <- reshape2::melt(volcano)
# names(v3d) <- c("x", "y", "z")
#
# ggplot(v3d, aes(x, y, z = z)) + scale_color_brewer(type = "qual", palette = 3)+
#     stat_fill_contour(color = "black", size = 0.2, binwidth = 10)


# dates <- unique(ncep$date)
# ggplot(RepeatLon(ncep[date %in% dates[6]]), aes(lon, lat, z = gh.t)) +
#     stat_fill_contour() +
#     map.SH.3 +
#     geom_contour(color = "black", size = 0.2) +
#     stat_contourlabel(step = 1, size = 2.5, geom = "label",
#                       label.padding = unit(0.1, "lines"),
#                       label.r = unit(0, "lines")) +
#     # coord_map(projection = "stereographic", orientation = c(-90, 0, 0)) +
#     coord_polar() +
#     scale_x_longitude() +
#     ylim(c(-90, -10)) +
#     facet_wrap(~date) +
#     # scale_fill_viridis()
#     scale_fill_divergent(name = "Geopotential Height Anomalies") +
#     guides(fill = guide_colorbar(title.position = "top")) +
#     ggtitle("Filled Contours and easy \ncontour labels in ggplot2")
# # stat_fill_contour(aes(fill = ..levelc.., label = ..rank..), geom = "text",
# # size = 3)
# # xlim(c(0, 50)) + ylim(c(0, 90))


# ggplot(h, aes(x, y)) +
#     geom_path(aes(group = group)) +
#     geom_point(color = "red") +
#     geom_point(data = f)
