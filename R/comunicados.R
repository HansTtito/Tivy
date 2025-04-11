#' Extrae datos de los comunicados en formato PDF
#'
#' Esta función procesa archivos PDF que contienen comunicados y extrae información relevante, como fechas,
#' horas, coordenadas de latitud y longitud, y millas náuticas. Si no se encuentran longitudes, la función
#' procesará las millas náuticas. El resultado es un data.frame con las fechas de inicio y fin, coordenadas
#' y otra información relacionada con el comunicado.
#'
#' @param vector_pdf_names Un vector de nombres de archivo PDF a procesar.
#'
#' @return Un data.frame con la información extraída de los comunicados. El data.frame incluye las siguientes
#' columnas:
#' \itemize{
#'   \item FechaHoraInicio: Fecha y hora de inicio.
#'   \item FechaHoraFin: Fecha y hora de fin.
#'   \item LatitudInicio: Latitud de inicio.
#'   \item LatitudFin: Latitud de fin.
#'   \item LongitudInicio: Longitud de inicio (si está disponible).
#'   \item LongitudFin: Longitud de fin (si está disponible).
#'   \item MillasNauticasInicio: Millas náuticas de inicio (si no hay longitudes).
#'   \item MillasNauticasFin: Millas náuticas de fin (si no hay longitudes).
#'   \item comunicado: Nombre del archivo del comunicado.
#' }
#'
#' @examples
#' # Suponiendo que tengas una lista de archivos PDF
#' pdf_files <- c("comunicado1.pdf", "comunicado2.pdf")
#' resultados <- extrae_data_comunicados(pdf_files)
#' head(resultados)
#'
#' @export
#' @importFrom pdftools pdf_text
#' @importFrom stringr str_replace_all str_split str_extract_all
extrae_data_comunicados <- function(vector_pdf_names) {
  todos_resultados <- list()

  # Procesar cada archivo PDF en vector_pdf_names
  for (file in vector_pdf_names) {
    # Leer el texto del archivo PDF
    texto <- pdftools::pdf_text(file)

    # Limpiar el texto
    texto_limpio <- stringr::str_squish(texto)

    # Separar el texto en bloques
    bloques <- stringr::str_split(texto_limpio, "DISPONER LA SUSPENSIÓN PREVENTIVA DE LA ACTIVIDAD EXTRACTIVA")[[1]]

    # Inicializar lista para almacenar los resultados de cada archivo PDF
    resultados <- list()

    # Recorrer cada bloque (ignorando el primer bloque que no contiene información relevante)
    for (i in 2:length(bloques)) {
      bloque <- bloques[i]

      # Extraer las fechas y horas de inicio y fin
      pattern_fechas <- "(\\d{2}:\\d{2} horas del \\d{2} de \\w+ de \\d{4})"
      fechas <- gsub("horas del ", "", stringr::str_extract_all(bloque, pattern_fechas)[[1]])
      fechas_final <- as.POSIXct(fechas, format = "%H:%M %d de %B de %Y", tz = "America/Lima")

      # Extraer las coordenadas de latitud y longitud
      pattern_coordenadas_lat <- "\\d{1,2}°\\d{1,2}[''][NS]"
      pattern_coordenadas_lon <- "\\d{1,2}°\\d{1,2}[''][WE]"
      data_posiciones_lat <- stringr::str_extract_all(bloque, pattern_coordenadas_lat)[[1]]
      data_posiciones_lon <- stringr::str_extract_all(bloque, pattern_coordenadas_lon)[[1]]

      # Verificar si hay longitudes presentes
      if (length(data_posiciones_lon) == 0) {
        # Extraer las millas náuticas si no hay longitudes
        pattern_millas <- "(?:entre las )?(\\d+) y (\\d+) millas náuticas|dentro de las (\\d+) millas náuticas"

        # Obtener todos los textos que contienen millas náuticas
        textos_millas <- stringr::str_extract_all(bloque, pattern_millas)[[1]]

        # Inicializar vectores para almacenar las millas de inicio y fin
        millas_inicio <- numeric()
        millas_fin <- numeric()

        # Procesar cada texto de millas náuticas
        for (texto_millas in textos_millas) {
          numeros_millas <- as.numeric(stringr::str_extract_all(texto_millas, "\\d+")[[1]])

          if (length(numeros_millas) > 0) {
            if (length(numeros_millas) == 1) {
              millas_inicio <- c(millas_inicio, NA)
              millas_fin <- c(millas_fin, numeros_millas)
            } else {
              numeros_millas_ordenados <- sort(numeros_millas)
              for (i in seq(1, length(numeros_millas_ordenados), by = 2)) {
                millas_inicio <- c(millas_inicio, numeros_millas_ordenados[i])
                millas_fin <- c(millas_fin, numeros_millas_ordenados[i + 1])
              }
            }
          } else {
            millas_inicio <- c(millas_inicio, NA)
            millas_fin <- c(millas_fin, NA)
          }
        }

        num_posiciones <- length(data_posiciones_lat)
        for (j in seq(1, num_posiciones, by = 2)) {
          df <- data.frame(
            FechaHoraInicio = fechas_final[1],
            FechaHoraFin = fechas_final[2],
            LatitudInicio = data_posiciones_lat[j],
            LatitudFin = data_posiciones_lat[j + 1],
            LongitudInicio = NA,
            LongitudFin = NA,
            MillasNauticasInicio = millas_inicio[ceiling(j/2)],
            MillasNauticasFin = millas_fin[ceiling(j/2)],
            comunicado = basename(file)
          )
          resultados <- append(resultados, list(df))
        }
      } else {
        num_posiciones <- min(length(data_posiciones_lat), length(data_posiciones_lon))

        if (num_posiciones %% 2 != 0) {
          stop("Número de posiciones de latitud y longitud no coincide en el bloque ", i)
        }

        for (j in seq(1, num_posiciones, by = 2)) {
          df <- data.frame(
            FechaHoraInicio = fechas_final[1],
            FechaHoraFin = fechas_final[2],
            LatitudInicio = data_posiciones_lat[j],
            LatitudFin = data_posiciones_lat[j + 1],
            LongitudInicio = data_posiciones_lon[j],
            LongitudFin = data_posiciones_lon[j + 1],
            MillasNauticasInicio = NA,
            MillasNauticasFin = NA,
            comunicado = basename(file)
          )
          resultados <- append(resultados, list(df))
        }
      }
    }

    resultados_finales <- do.call(rbind, resultados)
    todos_resultados <- append(todos_resultados, list(resultados_finales))
  }

  todos_resultados_finales <- do.call(rbind, todos_resultados)
  return(todos_resultados_finales)
}
