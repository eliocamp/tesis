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
                           default_aes = aes(fill = ..levelc..),

                           compute_group = function(data, scales, bins = NULL, binwidth = NULL,
                                                    breaks = NULL, complete = FALSE, na.rm = FALSE) {
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

                               f <<- data
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
                               extra$z <- range.data$z[1] - 1.5*binwidth
                               extra$PANEL <- data$PANEL[1]
                               cur.group <- data$group[1]
                               extra$group <- data$group[1]

                               data2 <- rbind(data, extra)
                               # f <<- data2
                               cont <- contour_lines(data2, breaks, complete = complete)



                               setDT(cont)

                               areas <- cont[, .(area = abs(area(x, y))), by = .(piece)][
                                   , rank := frank(-area, ties.method = "dense")]
                               areas <- areas[, head(.SD, 1), by = piece]
                               cont <-cont[areas, on = "piece"]
                               cont[, piece := rank]
                               cont[, group := factor(paste(cur.group,
                                                            sprintf("%03d", piece), sep = "-"))]
                               h <<- cont
                               cont <- CorrectFilll(cont)

                               cont$x[cont$x > range.data$x[2]] <- range.data$x[2]
                               cont$x[cont$x < range.data$x[1]] <- range.data$x[1]
                               cont$y[cont$y < range.data$y[1]] <- range.data$y[1]
                               cont$y[cont$y > range.data$y[2]] <- range.data$y[2]

                               cont
                           }

)


# v3d <- reshape2::melt(volcano)
# names(v3d) <- c("x", "y", "z")
#
# breaks <- seq(95, 195, length.out = 10)
# contours <- contourLines(v3d, breaks)
# ggplot(contours, aes(x, y)) +
#   geom_path() +
#   facet_wrap(~piece)
contour_lines <- function(data, breaks, complete = FALSE) {
    z <- tapply(data$z, data[c("x", "y")], identity)

    if (is.list(z)) {
        stop("Contour requires single `z` at each combination of `x` and `y`.",
             call. = FALSE)
    }

    cl <- grDevices::contourLines(
        x = sort(unique(data$x)), y = sort(unique(data$y)), z = z,
        levels = breaks)

    if (length(cl) == 0) {
        warning("Not possible to generate contour data", call. = FALSE)
        return(data.frame())
    }

    # Convert list of lists into single data frame
    lengths <- vapply(cl, function(x) length(x$x), integer(1))
    levels <- vapply(cl, "[[", "level", FUN.VALUE = double(1))
    xs <- unlist(lapply(cl, "[[", "x"), use.names = FALSE)
    ys <- unlist(lapply(cl, "[[", "y"), use.names = FALSE)
    pieces <- rep(seq_along(cl), lengths)
    # Add leading zeros so that groups can be properly sorted later
    groups <- paste(data$group[1], sprintf("%03d", pieces), sep = "-")

    data.frame(
        level = rep(levels, lengths),
        x = xs,
        y = ys,
        piece = pieces,
        group = groups
    )
}

# From https://stat.ethz.ch/pipermail/r-help/2004-December/063046.html
#
area<-function(x, y){
    X <- matrix(c(x, y), ncol = 2)
    X<-rbind(X,X[1,])
    x<-X[,1]; y<-X[,2]; lx<-length(x)
    -sum((x[2:lx]-x[1:lx-1])*(y[2:lx]+y[1:lx-1]))/2
}


CorrectFilll <- function(cont) {
    levels <- c(NA, unique(cont$level), NA)
    m.level <- min(levels)
    M.level <- max(levels)
    cont[, levelc := 0]
    pieces <- unique(cont$piece)

    for (p in pieces) {
        level <- cont[piece == p, level[1]]

        i <- which(levels == level)
        cur.piece <- cont[piece == p]

        # Possible adjacent pieces (based on adjcent levels).
        # Select only one point
        close.pieces <- cont[level %in% levels[c(i-1, i+1)]]

        # Check if points are inside current piece
        # Mejorar chequeando sólo en el chull de los pieces

        close.pieces <- close.pieces[, head(.SD, 1), by = piece]
        close.pieces[, inside := IsInside(x, y, cur.piece$x, cur.piece$y), by = .(piece)]
        inside.pieces <- close.pieces[inside == TRUE]


        if (nrow(inside.pieces) == 0 ){
            cur.piece <- cur.piece[1]
            close.pieces <- cont[piece %in% close.pieces$piece]
            close.pieces[, inside := IsInside(cur.piece$x, cur.piece$y, x, y), by = piece][
                    inside == TRUE, ][
                        area == min(area), level][1]

            correction <- (level - previous.level)/2

        } else {
            # Choose the biggest
            next.level <- inside.pieces[inside == TRUE, ][area == max(area), level]
            correction <- (next.level - level)/2
        }

        cont[piece == p, levelc := level + correction]
    }
    return(cont)
}

IsInside <- function(xp, yp, x, y) {
    !(sp::point.in.polygon(xp, yp, x, y) == 0)
}

v3d <- reshape2::melt(volcano)
names(v3d) <- c("x", "y", "z")

ggplot(v3d, aes(x, y, z = z)) + scale_color_brewer(type = "qual", palette = 3)+
    stat_fill_contour(color = "gray45", binwidth = 15, aes(fill = ..levelc..))
    # stat_fill_contour(color = "gray45", binwidth = 15, aes(label = ..levelc..), geom = "text") +
    # stat_fill_contour(breaks = 165) +
    stat_contour(binwidth = 15) +
    stat_contourlabel(binwidth = 15, step = 1)
    # scale_color_brewer(type = "qual", palette = 3) +

