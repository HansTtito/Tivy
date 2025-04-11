#' Extrae y procesa datos de pesca juvenil y adulta desde una página web de IMARPE
#'
#' @description
#' La función `data_juv_imarpe()` extrae datos de una tabla contenida en la página web de IMARPE (Instituto del Mar del Perú) y los procesa para su análisis.
#' Esta tabla contiene las cantidades de pesca juvenil, adulta y total (en toneladas) para diferentes fechas. Los datos extraídos son limpiados, convirtiendo
#' las fechas a un formato estándar y las cantidades a valores numéricos sin espacios.
#'
#' @param link Una cadena de texto que contiene la URL de la página web de IMARPE con la tabla de datos.
#'
#' @return Un `data.frame` con las columnas "fecha", "juv_ton", "adu_ton" y "total_ton", donde:
#'   - "fecha" es la fecha de los datos, convertida a formato `Date`.
#'   - "juv_ton", "adu_ton" y "total_ton" son las cantidades en toneladas de pesca juvenil, adulta y total, respectivamente.
#'
#' @examples
#' link <- "https://www.imarpe.gob.pe/imarpe/archivos/reportes/Reporte_Anchoveta_II_2024.html"
#' datos <- data_juv_imarpe(link)
#' print(datos)
#'
#' @export
data_juv_imarpe <- function(link){



  library(wdman)

  # Descargar e iniciar chromedriver
  driver <- chrome(driver = NULL)  # Esto descarga y ejecuta la versión más reciente de chromedriver
  driver
  remDr <- driver[["client"]]
  remDr


  remDr$navigate("https://www.imarpe.gob.pe/imarpe/archivos/reportes/Reporte_Anchoveta_II_2024.html")




  url <- "https://www.imarpe.gob.pe/imarpe/archivos/reportes/Reporte_Anchoveta_II_2024.html#"

  pagina <- read_html(url)

  # Agregar un agente de usuario similar a un navegador web
  user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36"

  link <- 'https://www.imarpe.gob.pe/imarpe/archivos/reportes/Reporte_Anchoveta_II_2024.html#indicadores-diarios'
  # Usar httr para hacer la solicitud con el agente de usuario
  response <- httr::GET(link, httr::user_agent(user_agent))

  webpage <- rvest::read_html(httr::content(response, "text", encoding = "UTF-8"))

  # Leer la página web
  webpage <- webpage %>%
    rvest::html_elements("#dashboard-container") %>%
    rvest::html_table() %>%
    purrr::simplify() %>%
    first()

  # Extraer los datos relevantes hasta la fila que contiene "Total"
  tabla <- webpage[1:(grep(pattern = "Total", x = webpage$Fecha)-1),]

  # Asignar nombres a las columnas
  names(tabla) <- c("fecha", "juv_ton", "adu_ton", "total_ton")

  # Limpiar y transformar los datos
  tabla <- tabla %>%
    mutate(fecha = convertir_a_fecha(fecha), # Convertir fechas al formato adecuado
           juv_ton = as.numeric(str_remove_all(juv_ton, pattern = " ")), # Eliminar espacios y convertir a numérico
           adu_ton = as.numeric(str_remove_all(adu_ton, pattern = " ")),
           total_ton = as.numeric(str_remove_all(total_ton, pattern = " ")))

  # Retornar los datos procesados
  return(tabla)

}
