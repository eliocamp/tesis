library(ggplot2)
library(data.table)
library(ReporteRs)
library(magrittr)

mydoc <- pptx() %>%
    addSlide("Title Only") %>%
    addTitle("Composición de campos de Geopotencial") %>%
    addSlide("Title and Content") %>%
    addPlot(function(x) print(g)) %>%
    writeDoc("presentacion.pptx")
