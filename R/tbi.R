#' Funciones para inicializar
#'
#' Importa y guarda en data.frame objetos de diferentes bases de datos.
#' @param db base de datos ("cortes")
#' @param l Local (Computadora): 1 = Negocio local, 2 = Casa (Personal), 3 = Usuario
#' @export
tbi_importar <- function(db, l){
  if(db == "cortes"){
    s <- tbi_h_escogerlocal(l)
    archivo <- paste0(s, "TotalControl/BackEnd.xlsm")
    print(paste0(" >> Usando: tbi_importer_cortes en local: ", l))
    d <- tbi_importer_cortes(archivo)
  }
return(d)
}
#' Funciones para inicializar
#'
#' Importar y unir cortes diarios
#' @param archivo archivo de cortes
#' @import readxl
#' @import magrittr
#' @import reshape2
#' @import dplyr
#' @export
tbi_importer_cortes <- function(archivo){

  # 1. Importar
  header <- readxl::read_excel(archivo, sheet = "BD_Cortes_Header")
    print("Header importado...")
  clientes <- readxl::read_excel(archivo, sheet = "BD_Cortes_Clientes")
    print("Clientes importados...")
  vendedor <- readxl::read_excel(archivo, sheet = "BD_Cortes_Vendedor")
    print("Vendedores importados...")
  pagos <- readxl::read_excel(archivo, sheet = "BD_Cortes_Pagos")
    print("Pagos importados...")

  # 2. Limpiar
  header %<>% filter(VentaTotal!=0)
  clientes %<>% filter(Compra=!0)
  vendedor %<>% filter(Venta!=0)
  pagos %<>% filter(Venta!=0)

  # 3. Abrir las categorias, para hacer bind...
  # -----
    pagos <- reshape2::dcast(data = pagos,
                           formula = Hora_Fecha + Dia + Mes + Year + Sucursal ~ Pago,
                           fill = 0, value.var = "Venta", fun.aggregate = sum)
    pagos <- tbi_h_renombrar(pagos,
                             6:length(names(pagos)),
                             tipo = "pre", char = "Pago_")
  # -----
    clientes <- reshape2::dcast(data = clientes,
                                formula = Hora_Fecha + Dia + Mes + Year + Sucursal ~ TipoCliente,
                                fill = 0, value.var = "Compra", fun.aggregate = sum)
    clientes <- tbi_h_renombrar(clientes,
                                6:length(names(clientes)),
                                tipo = "pre", char = "Cliente_")
  # -----
    vendedor <- reshape2::dcast(data = vendedor,
                                formula = Hora_Fecha + Dia + Mes + Year + Sucursal ~ Vendedora,
                                fill = 0, fun.aggregate = sum, value.var = "Venta")
    vendedor <- tbi_h_renombrar(vendedor,
                                6:length(names(vendedor)),
                                tipo = "pre", char = "Vendedora_")
  # 4. Homologar algunas columnas
  names(clientes)[names(clientes)=="Cliente_Los demás"] <- "Cliente_LosDemas"

  header %<>% mutate("FechaCorte" = as.Date(paste0(Year,"/", Mes, "/", Dia)))

  vendedor %<>% mutate("FechaCorte" = as.Date(paste0(Year,"/", Mes, "/", Dia)),
                       "TimeStamp_v" = Hora_Fecha) %>%
    select(-c(Hora_Fecha, Dia, Mes, Year))

  clientes %<>% mutate("FechaCorte" = as.Date(paste0(Year,"/", Mes, "/", Dia)),
                       "TimeStamp_c" = Hora_Fecha) %>%
    select(-c(Hora_Fecha, Dia, Mes, Year))

  pagos %<>% mutate("FechaCorte" = as.Date(paste0(Year,"/", Mes, "/", Dia)),
                       "TimeStamp_p" = Hora_Fecha) %>%
    select(-c(Hora_Fecha, Dia, Mes, Year))

  # 5. Unir y exportar
  d <- header %>%
    left_join(., pagos) %>%
    left_join(., vendedor) %>%
    left_join(., clientes)
return(d)
}




#' Funciones para inicializar
#'
#' Retorna string de path a archivos, dependiendo el local
#' @param l Local (Computadora): 1 = Negocio local, 2 = Casa (Personal), 3 = Usuario
#' @export
tbi_h_escogerlocal <- function(l){
  if(l == 1){
    s <- "c:/usuario.joyeria/"
  }else{
    if(l == 2){
      s <- "/Users/eduardoflores/Documents/Dropbox/Proyectos/Tara Brooch/"
    }else{
      if(l == 3){
        s <- "c:/usuario.mama/"
      }else{
        stop("local incorrecto, escoger 1, 2 o 3")
      }
    }
  }
  return(s)
}
#' Funciones para inicializar
#'
#' Retorna string de path a archivos, dependiendo el local
#' @param df data.frame
#' @param columnas Numeros de columnas a renombrar
#' @param tipo tipo de renombre ("pre" = antes o "pos" = despues)
#' @param char caracteres a poner antes o después
#' @export
tbi_h_renombrar <- function(df, columnas, tipo = "pre", char){

  if(tipo == "pre"){
    s <- names(df)[columnas]
    s_n <- paste0(char, s)
  }else{
    if(tipo == "pos"){
      s <- names(df)[columnas]
      s_n <- paste0(s, char)
    }else{
      stop("Escoger pre o pos")
    }
  }
  names(df)[columnas] <- s_n
  return(df)
}
