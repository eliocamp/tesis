ReadCtl <- function(descriptor, hd.rm = T) {
    # Lee el archivo binario de grads a partir de un de un sescriptor .ctl
    # Entra:
    #   descriptor: archivo .ctl
    #   hd.rm: ¿Remover los headers? Si no se hace, no tiene sentido devolver
    #          un array, por lo que sólo devuelve un vector. Es útil para
    #          utilzar como template al guardar de nuevo el .grb.
    # Sale:
    #   si hd.rm = T, un array multidimensional, si hd.rm = F, un vector.
    # Esta función es medio misteriosa porque la encontré en el directorio de
    # Speedy con sólo un vago recuerdo de haber necesitado y estrito una función
    # con esta funcionalidad. ¿Será esta? ¿Quién sabe? No está escrita en mi
    # estilo y tiene 1 comentario que tampoco suena a mí.
    # Tiene la gran limitación de sólo funcionar si el .ctl refiere a un único
    # archivo .grb.
    getdef <- function(var, text) {
        x <- text[grep(var, text)]
        x <- suppressWarnings(as.numeric(strsplit(x, " ")[[1]]))
        x <- x[which.max(!is.na(x))]
        return(x)
    }
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
