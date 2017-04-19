getdef <- function(var, text) {
  x <- text[grep(var, text)]
  x <- suppressWarnings(as.numeric(strsplit(x, " ")[[1]]))
  x <- x[which.max(!is.na(x))]
  return(x)
}

ReadCtl <- function(descriptor, hd.rm = T) {
  lines <- readLines(descriptor)

  file <- lines[grep("DSET", lines)]
  file <- strsplit(file, " ")[[1]]
  file <- file[which.max((file[-1] != "")) + 1]

  if (substr(file, 1, 1) == "^") {
    if (dirname(descriptor) == ".") {
      dir <- ""
    } else {
      dir <- dirname(descriptor)
    }
    file <- paste0(dir, "/", substr(file, 2, nchar(file)))
  } else {
    dir <- dir
  }

  x <- getdef("XDEF", lines)
  y <- getdef("YDEF", lines)
  z <- getdef("ZDEF", lines)
  vars <- getdef("VARS", lines)
  t <- getdef("TDEF", lines)
  na.val <- getdef("UNDEF", lines)
  e <- 1
  number_lines <- file.info(file)$size/4

  f <- file(file, "rb")
  fields <- readBin(f, "double", n  = number_lines, size = 4, endian = "big")
  close(f)

  # Remove bullsh*t headers
  if (hd.rm == T) {
      fields <- fields[fields != fields[1]]
      fields <- array(fields, dim = c(x, y, z, vars, t))
      return(fields)
  } else {
      return(fields)
  }

}



MakeZonal <- function(descriptor) {
    field <- ReadCtl(descriptor, hd.rm = T)
    dims <- dim(field)
    field <- as.data.table(melt(field))
    colnames(field) <- c("lon", "lat", "lev", "s", "month", "var")

    field[, var.z := mean(var[var > -999]), by = .(lat, month)]
    field[var < -99, var.z := min(var)]
    field[, var := NULL]
    field <- c(array(field$var.z,  dim = dims))

    # Tengo que agregarle los headers.
    outfield <- c(ReadCtl(descriptor = descriptor, hd.rm = F))
    outfield[outfield != outfield[1]] <- field
    return(outfield)
}