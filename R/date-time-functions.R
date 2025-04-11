#' Convierte un vector de fechas en diferentes formatos a un formato de fecha estándar
#'
#' @description
#' La función `convertir_a_fecha()` toma un vector de fechas en distintos formatos y las convierte a un formato de fecha estándar en R.
#' La función intenta analizar cada fecha utilizando una serie de formatos predefinidos y devuelve la primera fecha válida encontrada para cada entrada.
#' Si una fecha no puede ser interpretada por ninguno de los formatos, se asigna como `NA`.
#'
#' @param vector_fecha Un vector de caracteres que contiene fechas en diversos formatos.
#'
#' @return Un vector de objetos `Date` o `NA` si la fecha no puede ser convertida.
#'
#' @examples
#' fechas <- c("2025-04-10", "10/04/2025", "April 10, 2025")
#' fechas_convertidas <- convertir_a_fecha(fechas)
#' print(fechas_convertidas)
#'
#' @export
convertir_a_fecha <- function(vector_fecha) {

  # Lista de formatos de fecha posibles
  formatos_fecha <- c(
    "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d",
    "%Y/%m/%d %H:%M:%S", "%Y/%m/%d %H:%M", "%Y/%m/%d",
    "%d-%m-%Y %H:%M:%S", "%d-%m-%Y %H:%M", "%d-%m-%Y",
    "%d/%m/%Y %H:%M:%S", "%d/%m/%Y %H:%M", "%d/%m/%Y",
    "%m-%d-%Y %H:%M:%S", "%m-%d-%Y %H:%M", "%m-%d-%Y",
    "%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M", "%m/%d/%Y",
    "%b %d, %Y %H:%M:%S", "%b %d, %Y %H:%M", "%b %d, %Y",
    "%d %b %Y %H:%M:%S", "%d %b %Y %H:%M", "%d %b %Y",
    "%d-%b-%Y %H:%M:%S", "%d-%b-%Y %H:%M", "%d-%b-%Y",
    "%d/%b/%Y %H:%M:%S", "%d/%b/%Y %H:%M", "%d/%b/%Y"
  )

  # Inicializar un vector para las fechas convertidas
  formatos_finales = Date()

  # Iterar sobre cada elemento del vector de fechas
  for(fecha in vector_fecha){

    # Inicializar un vector vacío para almacenar las fechas convertidas por formato
    dats = Date()

    # Intentar convertir la fecha con cada formato
    for(formato in formatos_fecha){

      intento <- tryCatch({
        parse_date_time(fecha, orders = formato)
      }, warning = function(w) {
        return(NA)
      }, error = function(e) {
        return(NA)
      })

      # Si la conversión es exitosa, agregar la fecha al vector dats
      dats = c(dats, intento[!is.na(intento)])

    }

    # Tomar la primera fecha válida encontrada
    formatos_finales = c(formatos_finales, dats[1])

  }

  return(formatos_finales)

}
