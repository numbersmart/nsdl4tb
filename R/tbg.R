#' Funciones para graficar
#'
#' Grafica de datos tidy
#' @param data datos tidy
#' @param tipo "line" o "column"
#' @import highcharter
#' @import dplyr
#' @export
tbg_hc_pagos <- function(data, tipo){

  hc_tidy <- function(hc, data,
                      group,
                      nombres,
                      values, ...){

    arguments <- as.list(match.call())
    cats <- eval(arguments$group, data)

    n <- length(unique(as.character(cats)))
    if (n > 0) {
      for (i in 1:n) {
        nm <- as.character(unique(cats)[i])
        dat <- eval(arguments$values, data)
        dat <- dat[cats == nm]
        hc <- hc_add_series(hc, name = nombres[i], data = dat, ...)
      }
    }
    hc
  }

  hc <- highchart() %>%
    hc_tidy(data = data,
            group = FormaPago,
            nombres = gsub(pattern = "Pago_",
                           x = unique(data$FormaPago), replacement = ""),
            values = Pesos) %>%
    hc_chart(type = tipo)
  hc
}



