#' Extrae datos de los comunicados emitidos por PRODUCE en formato PDF
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
#' @importFrom stringr str_squish str_split str_extract_all
extrae_data_comunicados <- function(vector_pdf_names) {
  # Validación de parámetros
  if (missing(vector_pdf_names)) {
    stop("El parámetro 'vector_pdf_names' es obligatorio.")
  }

  if (length(vector_pdf_names) == 0) {
    warning("No se proporcionaron nombres de archivos PDF para procesar.")
    return(data.frame())
  }

  if (!is.character(vector_pdf_names)) {
    stop("'vector_pdf_names' debe ser un vector de caracteres con nombres de archivos PDF.")
  }

  # Verificar que los archivos existen
  archivos_no_existentes <- vector_pdf_names[!file.exists(vector_pdf_names)]
  if (length(archivos_no_existentes) > 0) {
    warning("Los siguientes archivos no existen: ", paste(archivos_no_existentes, collapse = ", "))
    vector_pdf_names <- vector_pdf_names[file.exists(vector_pdf_names)]

    if (length(vector_pdf_names) == 0) {
      stop("Ninguno de los archivos especificados existe. No se puede continuar.")
    }
  }

  # Verificar que los archivos son PDFs (extensión .pdf)
  no_pdf <- vector_pdf_names[!grepl("\\.pdf$", vector_pdf_names, ignore.case = TRUE)]
  if (length(no_pdf) > 0) {
    warning("Los siguientes archivos no tienen extensión .pdf: ", paste(no_pdf, collapse = ", "))
  }

  # Inicializar lista para almacenar resultados de todos los PDFs
  todos_resultados <- list()

  # Definir estructura para resultados vacíos
  estructura_vacia <- data.frame(
    FechaHoraInicio = as.POSIXct(character()),
    FechaHoraFin = as.POSIXct(character()),
    LatitudInicio = character(),
    LatitudFin = character(),
    LongitudInicio = character(),
    LongitudFin = character(),
    MillasNauticasInicio = numeric(),
    MillasNauticasFin = numeric(),
    comunicado = character(),
    stringsAsFactors = FALSE
  )

  # Procesar cada archivo PDF
  for (file in vector_pdf_names) {
    # Leer el texto del archivo PDF con manejo de errores
    texto <- tryCatch({
      pdftools::pdf_text(file)
    }, error = function(e) {
      warning("Error al leer el archivo PDF '", file, "': ", e$message)
      return(NULL)
    })

    # Verificar si se pudo leer el PDF
    if (is.null(texto) || length(texto) == 0) {
      warning("No se pudo extraer texto del archivo '", file, "' o el archivo está vacío.")
      todos_resultados[[length(todos_resultados) + 1]] <- estructura_vacia
      next
    }

    # Limpiar el texto
    texto_limpio <- stringr::str_squish(texto)

    # Verificar si contiene la cadena de texto clave
    if (!any(grepl("DISPONER LA SUSPENSIÓN PREVENTIVA DE LA ACTIVIDAD EXTRACTIVA", texto_limpio))) {
      warning("El archivo '", file, "' no parece ser un comunicado válido de PRODUCE. No contiene el texto clave.")
      todos_resultados[[length(todos_resultados) + 1]] <- estructura_vacia
      next
    }

    # Separar el texto en bloques
    bloques <- tryCatch({
      stringr::str_split(texto_limpio, "DISPONER LA SUSPENSIÓN PREVENTIVA DE LA ACTIVIDAD EXTRACTIVA")[[1]]
    }, error = function(e) {
      warning("Error al dividir el texto en bloques para el archivo '", file, "': ", e$message)
      return(character(0))
    })

    # Verificar si hay bloques para procesar (después del primero)
    if (length(bloques) <= 1) {
      warning("No se encontraron bloques de información válidos en el archivo '", file, "'.")
      todos_resultados[[length(todos_resultados) + 1]] <- estructura_vacia
      next
    }

    # Inicializar lista para resultados de este archivo
    resultados <- list()

    # Procesar cada bloque (saltar el primero)
    for (i in 2:length(bloques)) {
      bloque <- bloques[i]

      # Extraer fechas y horas con manejo de errores
      tryCatch({
        pattern_fechas <- "(\\d{2}:\\d{2} horas del \\d{2} de \\w+ de \\d{4})"
        fechas_texto <- stringr::str_extract_all(bloque, pattern_fechas)[[1]]

        if (length(fechas_texto) < 2) {
          warning("No se encontraron suficientes fechas en el bloque ", i, " del archivo '", file, "'.")
          next
        }

        fechas <- gsub("horas del ", "", fechas_texto)
        fechas_final <- as.POSIXct(fechas, format = "%H:%M %d de %B de %Y", tz = "America/Lima")

        if (any(is.na(fechas_final))) {
          warning("Error al convertir fechas en el bloque ", i, " del archivo '", file, "'.")
          next
        }
      }, error = function(e) {
        warning("Error al procesar fechas en el bloque ", i, " del archivo '", file, "': ", e$message)
        fechas_final <- rep(as.POSIXct(NA), 2)
      })

      # Extraer coordenadas con manejo de errores
      tryCatch({
        # Varios patrones para intentar capturar diferentes formatos de coordenadas
        patrones_lat <- c(
          "\\d{1,2}°\\d{1,2}['´’][NS]",
          "\\d{1,2}°\\d{1,2}['’][NS]",
          "\\d{1,2}°\\d{1,2}['`][NS]"
        )

        patrones_lon <- c(
          "\\d{1,2}°\\d{1,2}['´’][WEO]",
          "\\d{1,2}°\\d{1,2}['’][WEO]",
          "\\d{1,2}°\\d{1,2}['`][WEO]"
        )

        # Intentar con cada patrón hasta encontrar coincidencias
        data_posiciones_lat <- character(0)
        for (patron in patrones_lat) {
          data_posiciones_lat <- stringr::str_extract_all(bloque, patron)[[1]]
          if (length(data_posiciones_lat) > 0) break
        }

        data_posiciones_lon <- character(0)
        for (patron in patrones_lon) {
          data_posiciones_lon <- stringr::str_extract_all(bloque, patron)[[1]]
          if (length(data_posiciones_lon) > 0) break
        }

      }, error = function(e) {
        warning("Error al extraer coordenadas en el bloque ", i, " del archivo '", file, "': ", e$message)
        data_posiciones_lat <- character(0)
        data_posiciones_lon <- character(0)
      })

      # Verificar si hay longitudes presentes
      if (length(data_posiciones_lon) == 0) {
        # Extraer las millas náuticas si no hay longitudes
        tryCatch({
          # Varios patrones para diferentes formatos de millas náuticas
          patrones_millas <- c(
            "de (\\d+) a (\\d+) millas náuticas",
            "(?:entre las )?(\\d+) y (\\d+) millas náuticas",
            "dentro de las (\\d+) millas náuticas",
            "(\\d+)\\s*a\\s*(\\d+)\\s*mn"
          )

          # Intentar con cada patrón
          textos_millas <- character(0)
          for (patron in patrones_millas) {
            textos_millas <- stringr::str_extract_all(bloque, patron)[[1]]
            if (length(textos_millas) > 0) break
          }

          # Inicializar vectores para millas
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
                for (k in seq(1, length(numeros_millas_ordenados) - 1, by = 2)) {
                  if (k + 1 <= length(numeros_millas_ordenados)) {
                    millas_inicio <- c(millas_inicio, numeros_millas_ordenados[k])
                    millas_fin <- c(millas_fin, numeros_millas_ordenados[k + 1])
                  }
                }
              }
            } else {
              millas_inicio <- c(millas_inicio, NA)
              millas_fin <- c(millas_fin, NA)
            }
          }

          # Si no hay millas, generar valores NA
          if (length(millas_inicio) == 0) {
            warning("No se encontraron millas náuticas en el bloque ", i, " del archivo '", file, "'.")
            millas_inicio <- NA
            millas_fin <- NA
          }

        }, error = function(e) {
          warning("Error al procesar millas náuticas en el bloque ", i, " del archivo '", file, "': ", e$message)
          millas_inicio <- NA
          millas_fin <- NA
        })

        # Crear dataframes para cada par de coordenadas
        num_posiciones <- length(data_posiciones_lat)
        if (num_posiciones >= 2) {
          for (j in seq(1, num_posiciones - 1, by = 2)) {
            if (j + 1 <= num_posiciones) {
              df <- data.frame(
                FechaHoraInicio = fechas_final[1],
                FechaHoraFin = fechas_final[2],
                LatitudInicio = data_posiciones_lat[j],
                LatitudFin = data_posiciones_lat[j + 1],
                LongitudInicio = NA_character_,
                LongitudFin = NA_character_,
                MillasNauticasInicio = millas_inicio[ceiling(j/2)],
                MillasNauticasFin = millas_fin[ceiling(j/2)],
                comunicado = basename(file),
                stringsAsFactors = FALSE
              )
              resultados <- append(resultados, list(df))
            }
          }
        } else if (num_posiciones == 1) {
          # Caso especial: una sola coordenada
          warning("Solo se encontró una coordenada de latitud en el bloque ", i, " del archivo '", file, "'. Se usará como inicio y fin.")
          df <- data.frame(
            FechaHoraInicio = fechas_final[1],
            FechaHoraFin = fechas_final[2],
            LatitudInicio = data_posiciones_lat[1],
            LatitudFin = data_posiciones_lat[1],
            LongitudInicio = NA_character_,
            LongitudFin = NA_character_,
            MillasNauticasInicio = millas_inicio[1],
            MillasNauticasFin = millas_fin[1],
            comunicado = basename(file),
            stringsAsFactors = FALSE
          )
          resultados <- append(resultados, list(df))
        } else {
          warning("No se encontraron coordenadas de latitud en el bloque ", i, " del archivo '", file, "'.")
        }

      } else {
        # Caso con longitudes presentes
        num_posiciones_lat <- length(data_posiciones_lat)
        num_posiciones_lon <- length(data_posiciones_lon)

        if (num_posiciones_lat == 0 || num_posiciones_lon == 0) {
          warning("Faltan coordenadas en el bloque ", i, " del archivo '", file, "'.")
          next
        }

        # Determinar cuántos pares completos podemos formar
        num_posiciones <- min(num_posiciones_lat, num_posiciones_lon)

        if (num_posiciones < 2) {
          warning("No hay suficientes coordenadas para formar un rango en el bloque ", i, " del archivo '", file, "'.")
          next
        }

        # Verificar que podemos formar pares (número par de coordenadas)
        if (num_posiciones %% 2 != 0) {
          warning("Número impar de coordenadas en el bloque ", i, " del archivo '", file, "'. Se usarán solo los pares completos.")
          num_posiciones <- num_posiciones - 1  # Ajustar para usar solo pares completos
        }

        for (j in seq(1, num_posiciones - 1, by = 2)) {
          if (j + 1 <= num_posiciones) {
            df <- data.frame(
              FechaHoraInicio = fechas_final[1],
              FechaHoraFin = fechas_final[2],
              LatitudInicio = data_posiciones_lat[j],
              LatitudFin = data_posiciones_lat[j + 1],
              LongitudInicio = data_posiciones_lon[j],
              LongitudFin = data_posiciones_lon[j + 1],
              MillasNauticasInicio = NA_real_,
              MillasNauticasFin = NA_real_,
              comunicado = basename(file),
              stringsAsFactors = FALSE
            )
            resultados <- append(resultados, list(df))
          }
        }
      }
    }

    # Combinar resultados de este archivo
    if (length(resultados) > 0) {
      resultados_finales <- tryCatch({
        do.call(rbind, resultados)
      }, error = function(e) {
        warning("Error al combinar resultados del archivo '", file, "': ", e$message)
        return(estructura_vacia)
      })
      todos_resultados <- append(todos_resultados, list(resultados_finales))
    } else {
      warning("No se pudo extraer información del archivo '", file, "'.")
      todos_resultados <- append(todos_resultados, list(estructura_vacia))
    }
  }

  # Combinar todos los resultados
  if (length(todos_resultados) > 0) {
    tryCatch({
      todos_resultados_finales <- do.call(rbind, todos_resultados)
      if (nrow(todos_resultados_finales) == 0) {
        warning("No se pudo extraer información de ninguno de los archivos PDF proporcionados.")
        return(estructura_vacia)
      }
      return(todos_resultados_finales)
    }, error = function(e) {
      warning("Error al combinar resultados de todos los archivos: ", e$message)
      return(estructura_vacia)
    })
  } else {
    warning("No se pudo extraer información de ninguno de los archivos PDF proporcionados.")
    return(estructura_vacia)
  }
}
