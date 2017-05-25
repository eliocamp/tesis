


ggplot(gdata[month %in% c(3)], aes(lat, lev, z = control)) +
    stat_fill_contour(binwidth = 2.5) +
    stat_contour(aes(label = ..level..), geom = "text", binwidth = 2.5) +
    # stat_fill_contour(binwidth = 2.5, aes(label = ..int.level..), geom = "text") +
    # geom_contour(color = "red", binwidth = 2.5) +
    # stat_fill_contour(binwidth = 2.5, aes(label = ..level..), geom = "text") +
    scale_x_reverse() +
    scale_y_continuous(trans = "reverselog") +
    scale_fill_divergent() +
    facet_wrap(~month)


ggplot(ncep[date == date[1]], aes(lon, lat, z = gh.t)) +
    stat_fill_contour() +
    map.SH.3 +
    # stat_fill_contour( aes(label = ..int.level..), geom = "text") +
    # geom_contour(aes(linetype = factor(-sign(..level..)))) +
    # geom_contourlabel(step = 1) +
    scale_fill_divergent() +
    geom_text(data = ncep[date == date[1] & lon %~% 200], aes(label = round(gh.t, 2)))

ggplot(co.2, aes(x, y, group = group)) +
    geom_path(aes(color = level))
    # geom_text(aes(label = int.level)) +
    # geom_text(aes(label = round(z, 1)), data = subset(data3, y == y[112])) +
    # geom_point(data = subset(data3, y == y[112]))
    # geom_polygon(data = co[piece == 4]) +
    geom_path(data = co[piece == 4], color = "red") +

    geom_point(data = p0, color = "red") +
    geom_point(data = f) +
    geom_point(data = p1, color = "blue") +
    geom_point(data = p2, color = "green")

library(contoureR)
library(ggplot2)
a = -2; b = +2; n = 150
x = runif(n*n,a,b)
y = runif(n*n,a,b)
df = data.frame(x,y)
df$z = with(df,-x*y*exp(-x^2-y^2))
df.sub = subset(df,x^2 + y^2 < 2)

ggplot(df.sub, aes(x, y, z = z)) +
    goem:


    df.cnt = getContourLines(df.sub,nlevels=20)
ggplot(data=df.cnt,aes(x,y,group=Group,colour=z)) + geom_path() + theme_bw()
