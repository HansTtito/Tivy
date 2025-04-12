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
#' @importFrom lubridate parse_date_time
convertir_a_fecha <- function(vector_fecha) {
  # Validación de entrada
  if (missing(vector_fecha)) {
    stop("Se requiere proporcionar el parámetro 'vector_fecha'.")
  }

  if (length(vector_fecha) == 0) {
    stop("El vector de fechas está vacío.")
  }

  # Convertir a character si es un factor
  if (is.factor(vector_fecha)) {
    vector_fecha <- as.character(vector_fecha)
    warning("El vector de fechas ha sido convertido de factor a character.")
  }


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

  # Inicializar vector para almacenar resultados
  resultado <- vector("list", length(vector_fecha))

  # Procesar cada elemento
  for (i in seq_along(vector_fecha)) {
    fecha_texto <- vector_fecha[i]

    # Manejar valores NA o vacíos
    if (is.na(fecha_texto) || fecha_texto == "") {
      resultado[[i]] <- as.Date(NA)
      next
    }

    # Intentar convertir la fecha con cada formato
    fecha_convertida <- NA
    for (formato in formatos_fecha) {
      intento <- tryCatch({
        fecha_parseada <- lubridate::parse_date_time(fecha_texto, orders = formato, quiet = TRUE)
        # Verificar que la fecha esté en un rango razonable (entre 1900 y 2100)
        if (!is.na(fecha_parseada)) {
          año <- as.numeric(format(fecha_parseada, "%Y"))
          if (año >= 1900 && año <= 2100) {
            fecha_parseada
          } else {
            NA
          }
        } else {
          NA
        }
      }, warning = function(w) {
        NA
      }, error = function(e) {
        NA
      })

      if (!is.na(intento)) {
        fecha_convertida <- as.Date(intento)
        break  # Usar el primer formato exitoso
      }
    }

    resultado[[i]] <- fecha_convertida
  }

  # Combinar resultados
  fechas_finales <- do.call(c, resultado)

  # Verificar si todas las conversiones fallaron
  if (all(is.na(fechas_finales))) {
    warning("No se pudo convertir ninguna fecha. Verifique el formato de los datos de entrada.")
  } else if (any(is.na(fechas_finales))) {
    warning("Algunas fechas no pudieron ser convertidas y se asignaron como NA.")
  }

  return(fechas_finales)
}
