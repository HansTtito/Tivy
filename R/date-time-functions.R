#' Convierte un vector de fechas en diferentes formatos a un formato de fecha estándar
#'
#' @description
#' La función `convertir_a_fecha()` toma un vector de fechas en distintos formatos y las convierte a un formato de fecha estándar en R.
#' La función intenta analizar cada fecha utilizando una serie de formatos predefinidos y devuelve la primera fecha válida encontrada para cada entrada.
#' Si una fecha no puede ser interpretada por ninguno de los formatos, se asigna como `NA`.
#'
#' @param vector_fecha Un vector de caracteres que contiene fechas en diversos formatos.
#' @param tipo Tipo de objeto a devolver: "date" para Date, "datetime" para POSIXct (por defecto).
#'
#' @return Un vector de objetos `Date` o `POSIXct` según el parámetro tipo, o `NA` si la fecha no puede ser convertida.
#'
#' @examples
#'
#' fechas <- c("2025-04-10", "10/04/2025", "April 10, 2025")
#'
#' fechas_convertidas <- convertir_a_fecha(fechas)
#'
#' print(fechas_convertidas)
#'
#' @export
#' @importFrom lubridate parse_date_time
convertir_a_fecha <- function(vector_fecha, tipo = "datetime") {
  # Verificar si el vector está vacío
  if (length(vector_fecha) == 0) {
    return(vector_fecha)
  }

  # Códigos de formato para parse_date_time
  ordenes <- c(
    "Ymd HMS", "Ymd HM", "Ymd",
    "Y/m/d HMS", "Y/m/d HM", "Y/m/d",
    "dmy HMS", "dmy HM", "dmy",
    "d/m/Y HMS", "d/m/Y HM", "d/m/Y",
    "mdy HMS", "mdy HM", "mdy",
    "m/d/Y HMS", "m/d/Y HM", "m/d/Y",
    "bd Y HMS", "bd Y HM", "bd Y",
    "db Y HMS", "db Y HM", "db Y"
  )

  # Convertir las fechas usando parse_date_time
  fechas_convertidas <- lubridate::parse_date_time(vector_fecha, orders = ordenes, quiet = TRUE)

  # Convertir al tipo de salida especificado
  if (tipo == "date") {
    fechas_convertidas <- as.Date(fechas_convertidas)
  }

  return(fechas_convertidas)
}
