#' Funciones para manipular
#'
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
    d <- d %>% filter(Pesos>0)
  }else{
    #otros tipos
  }
  return(d)
}
