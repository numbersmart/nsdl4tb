#' Funciones para manipular
#'
#'ojo, cambiar el nombre!
#' Manipula datos de corte, produce tipo de objeto tidy para gráficar
#' @param data Datos
#' @param tipo Actualmente por: "pagos"
#' @import dplyr
#' @import tidyr
#' @export
tbm_crear_graficar <- function(data, tipo){
  if(tipo == "pagos"){
    d <- data %>%
      select(c(FechaCorte, starts_with("Pago_"))) %>%
      tidyr::gather(key = FechaCorte)
    names(d) <- c("FechaCorte", "FormaPago", "Pesos")
    # d <- d %>% filter(Pesos>0)
  }else{
    #otros tipos
  }
  return(d)
}
#' Funciones para manipular
#'
#' Manipula datos de inventario fisico para gráficar
#' @param data Datos
#' @import dplyr
#' @import tidyr
#' @export
tbm_grafica_fisicos <- function(data){
  d <- data %>%
    group_by(DESC_FAM) %>%
    summarise("ARTICULOS" = n_distinct(ID),
              "PRECIO_PROM" = mean(PRECIO),
              "MARGEN_PROM" = mean(PRECIO)/mean(COSTO),
              "DIAS_FISICO_PROM" = mean(DIAS_FISICO)) %>%
    mutate("CORTE" = "TOTAL")

  d_nuevos <- data %>%
    filter(DIAS_FISICO<90) %>%
    group_by(DESC_FAM) %>%
    summarise("ARTICULOS" = n_distinct(ID),
              "PRECIO_PROM" = mean(PRECIO),
              "MARGEN_PROM" = mean(PRECIO)/mean(COSTO),
              "DIAS_FISICO_PROM" = mean(DIAS_FISICO)) %>%
    mutate("CORTE" = "M90")

  d <- rbind.data.frame(d, d_nuevos)

  return(d)
}
