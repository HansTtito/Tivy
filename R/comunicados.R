#' Extract data from PRODUCE announcements in PDF format
#'
#' This function processes PDF files containing official announcements and extracts relevant information,
#' such as dates, times, latitude and longitude coordinates, and nautical miles. If longitude data is not found,
#' the function will use the nautical mile values instead. The result is a data.frame containing the start and
#' end dates, coordinates, and other information related to the announcement.
#'
#' @param vector_pdf_names A character vector of PDF file names or URLs to be processed.
#'
#' @return A data.frame with the extracted information from the announcements. The data.frame includes the following
#' columns:
#' \itemize{
#'   \item FechaHoraInicio: Start date and time.
#'   \item FechaHoraFin: End date and time.
#'   \item LatitudInicio: Starting latitude.
#'   \item LatitudFin: Ending latitude.
#'   \item LongitudInicio: Starting longitude (if available).
#'   \item LongitudFin: Ending longitude (if available).
#'   \item MillasNauticasInicio: Starting nautical miles (if longitude is not available).
#'   \item MillasNauticasFin: Ending nautical miles (if longitude is not available).
#'   \item comunicado: Name of the PDF file or announcement.
#' }
#'
#' @examples
#' # Using local files
#' \dontrun{
#' pdf_files <- c("comunicado1.pdf", "comunicado2.pdf")
#' results <- extrae_data_comunicados(vector_pdf_names = pdf_files)
#' }
#'
#' # Using URLs
#' pdf_urls <- c(
#'   "https://consultasenlinea.produce.gob.pe/produce/descarga/comunicados/dgsfs/1542_comunicado1.pdf",
#'   "https://consultasenlinea.produce.gob.pe/produce/descarga/comunicados/dgsfs/1478_comunicado1.pdf",
#'   "https://consultasenlinea.produce.gob.pe/produce/descarga/comunicados/dgsfs/1468_comunicado1.pdf"
#' )
#'
#' results <- extrae_data_comunicados(vector_pdf_names = pdf_urls)
#'
#' print(head(results))
#'
#' @export
#' @importFrom pdftools pdf_text
#' @importFrom stringr str_squish str_split str_extract_all str_extract
#' @importFrom utils download.file
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

  # Crear una función auxiliar para verificar si es URL
  is_url <- function(x) {
    grepl("^(http|https|ftp)://", x, ignore.case = TRUE)
  }

  # Crear directorio temporal para archivos descargados
  temp_dir <- tempdir()
  temp_files <- character(length(vector_pdf_names))

  # Procesar URLs y descargar archivos si es necesario
  for (i in seq_along(vector_pdf_names)) {
    if (is_url(vector_pdf_names[i])) {
      # Es una URL, intentar descargar
      file_name <- basename(vector_pdf_names[i])
      temp_path <- file.path(temp_dir, file_name)

      tryCatch({
        download_result <- utils::download.file(
          url = vector_pdf_names[i],
          destfile = temp_path,
          mode = "wb",  # Modo binario para PDFs
          quiet = TRUE
        )

        if (download_result == 0) {
          # Descarga exitosa, usar archivo temporal
          temp_files[i] <- temp_path
        } else {
          warning("Error al descargar la URL: ", vector_pdf_names[i])
          temp_files[i] <- NA_character_
        }
      }, error = function(e) {
        warning("Error al descargar la URL '", vector_pdf_names[i], "': ", e$message)
        temp_files[i] <- NA_character_
      })
    } else {
      # Es un archivo local, usar la ruta tal cual
      temp_files[i] <- vector_pdf_names[i]
    }
  }

  # Eliminar URLs que no se pudieron descargar
  temp_files <- temp_files[!is.na(temp_files)]

  if (length(temp_files) == 0) {
    stop("No se pudo acceder a ninguno de los archivos especificados. No se puede continuar.")
  }

  # Verificar que los archivos existen
  archivos_no_existentes <- temp_files[!file.exists(temp_files)]
  if (length(archivos_no_existentes) > 0) {
    warning("Los siguientes archivos no existen: ", paste(archivos_no_existentes, collapse = ", "))
    temp_files <- temp_files[file.exists(temp_files)]

    if (length(temp_files) == 0) {
      stop("Ninguno de los archivos especificados existe. No se puede continuar.")
    }
  }

  # Verificar que los archivos son PDFs (extensión .pdf)
  no_pdf <- temp_files[!grepl("\\.pdf$", temp_files, ignore.case = TRUE)]
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
    nombre_archivo = character(),
    stringsAsFactors = FALSE,
    comunicado = character()
  )

  # Procesar cada archivo PDF
  for (file in temp_files) {
    # Conservar nombre original para el campo nombre_archivo
    original_name <- ifelse(is_url(file), basename(file), basename(file))

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

    # Verificar si contiene la cadena de texto clave
    if (!any(grepl("COMUNICADO N°", texto_limpio))) {
      stop("El archivo '", file, "' no tiene título.")
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

        comunicado <- stringr::str_extract(texto_limpio, "COMUNICADO\\s*N[°º]\\s*\\d+(?:\\s*[-–]\\s*\\d+)?(?:-[A-Z]+)?")

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
                nombre_archivo = original_name,
                comunicado = comunicado,
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
            nombre_archivo = original_name,
            comunicado = comunicado,
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
              nombre_archivo = original_name,
              comunicado = comunicado,
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



#' Formatear y filtrar datos de comunicados
#'
#' Esta función formatea correctamente los datos extraídos de comunicados,
#' convirtiendo las fechas a formato POSIXct y permitiendo filtrar por rango de fechas.
#'
#' @param datos Un data frame con la estructura generada por extrae_data_comunicados.
#' @param fecha_min Fecha mínima para filtrar (opcional). Puede ser una cadena en formato
#'        "YYYY-MM-DD" o un objeto POSIXct/Date.
#' @param fecha_max Fecha máxima para filtrar (opcional). Puede ser una cadena en formato
#'        "YYYY-MM-DD" o un objeto POSIXct/Date.
#'
#' @return Un data frame con todas las columnas correctamente formateadas y filtrado según los
#'         parámetros especificados.
#'
#' @examples
#' # Formatear sin filtrar
#'
#'
#' pdf_urls <- c("https://consultasenlinea.produce.gob.pe/produce/descarga/comunicados/dgsfs/1542_comunicado1.pdf",
#' "https://consultasenlinea.produce.gob.pe/produce/descarga/comunicados/dgsfs/1478_comunicado1.pdf",
#' "https://consultasenlinea.produce.gob.pe/produce/descarga/comunicados/dgsfs/1468_comunicado1.pdf")
#'
#' resultados <- extrae_data_comunicados(pdf_urls)
#'
#' datos_formateados <- formatear_datos_comunicados(resultados)
#'
#' # Formatear y filtrar por rango de fechas
#' datos_filtrados <- formatear_datos_comunicados(
#'   resultados,
#'   fecha_min = "2024-11-01",
#'   fecha_max = "2024-12-31"
#' )
#'
#' @export
formatear_datos_comunicados <- function(datos, fecha_min = NULL, fecha_max = NULL) {
  # Verificar si el dataframe tiene la estructura esperada
  columnas_requeridas <- c(
    "FechaHoraInicio", "FechaHoraFin",
    "LatitudInicio", "LatitudFin"
  )

  if (!all(columnas_requeridas %in% names(datos))) {
    stop("El dataframe no tiene la estructura esperada. Debe contener las columnas: ",
         paste(columnas_requeridas, collapse = ", "))
  }

  # Clonar el dataframe para no modificar el original
  datos_fmt <- datos

  # Convertir columnas de fecha usando la función convertir_a_fecha
  for (col in c("FechaHoraInicio", "FechaHoraFin")) {
    if (col %in% names(datos_fmt)) {
      # Si ya es POSIXct, no hacer nada
      if (inherits(datos_fmt[[col]], "POSIXct")) {
        next
      }

      # Convertir fechas usando la función convertir_a_fecha
      datos_fmt[[col]] <- convertir_a_fecha(datos_fmt[[col]], tipo = "datetime")
    }
  }

  # Convertir coordenadas de latitud y longitud si no están en formato decimal
  for (col in c("LatitudInicio", "LatitudFin", "LongitudInicio", "LongitudFin")) {
    if (col %in% names(datos_fmt)) {
      # Verificar si la columna contiene coordenadas en formato DMS
      if (any(grepl("[°'\"]", datos_fmt[[col]], ignore.case = TRUE))) {
        # Convertir a decimal usando la función dms_a_decimal
        datos_fmt[[col]] <- sapply(datos_fmt[[col]], function(x) {
          if (is.na(x) || x == "") return(NA)
          tryCatch(Tivy::dms_a_decimal(x), error = function(e) NA)
        })
      }
    }
  }

  # Asegurarse de que las columnas numéricas sean numéricas
  for (col in c("MillasNauticasInicio", "MillasNauticasFin")) {
    if (col %in% names(datos_fmt)) {
      datos_fmt[[col]] <- as.numeric(datos_fmt[[col]])
    }
  }

  # Filtrar por rango de fechas si se especifican
  if (!is.null(fecha_min) || !is.null(fecha_max)) {
    # Convertir fecha_min a POSIXct si es necesario
    if (!is.null(fecha_min)) {
      if (is.character(fecha_min)) {
        fecha_min <- convertir_a_fecha(fecha_min, tipo = "datetime")
      } else if (inherits(fecha_min, "Date")) {
        fecha_min <- as.POSIXct(fecha_min)
      }
    }

    # Convertir fecha_max a POSIXct si es necesario
    if (!is.null(fecha_max)) {
      if (is.character(fecha_max)) {
        fecha_max <- convertir_a_fecha(fecha_max, tipo = "datetime")
      } else if (inherits(fecha_max, "Date")) {
        fecha_max <- as.POSIXct(fecha_max)
      }
    }

    # Aplicar filtros
    if (!is.null(fecha_min) && !is.null(fecha_max)) {
      datos_fmt <- datos_fmt[datos_fmt$FechaHoraInicio >= fecha_min &
                               datos_fmt$FechaHoraInicio <= fecha_max, ]
    } else if (!is.null(fecha_min)) {
      datos_fmt <- datos_fmt[datos_fmt$FechaHoraInicio >= fecha_min, ]
    } else if (!is.null(fecha_max)) {
      datos_fmt <- datos_fmt[datos_fmt$FechaHoraInicio <= fecha_max, ]
    }
  }

  # Verificar que el dataframe tenga filas después del filtrado
  if (nrow(datos_fmt) == 0) {
    warning("No hay datos que cumplan con los criterios de filtrado.")
  }

  return(datos_fmt)
}
