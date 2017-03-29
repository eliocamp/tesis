# Pasa Speedy a .csv
library(data.table)
library(lubridate)
library(fields)
library(ncdf4)
source("helperfun.R")

# Valores medios
file <- "DATA/SPEEDY/attm.nc"
ncfile <- nc_open(file)
vars <- c("gh", "u", "v", "psi", "temp")
speedy<- ncread(file, vars)

# Selecciono sólo el sector que me importa.
speedy <- speedy[lat < 0]
speedy[, time :=as.Date("1985-01-01 00:00:00") + time/24]
speedy[, month := factor(month.abb[data.table::month(time)], levels = month.abb)]
speedy[, year := year(time)]
# setindex(speedy, lon, lat, lev, month, year)

fwrite(speedy, "DATA/SPEEDY/todo_speedy.csv")


m_speedy <- speedy[, lapply(.SD, mean),
                   by = .(lon, lat, lev, month),
                   .SDcols = vars]

fwrite(m_speedy, "DATA/SPEEDY/todo_m_speedy.csv")
remove(speedy, m_speedy)
