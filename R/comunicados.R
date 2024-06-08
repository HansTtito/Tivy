
extrae_data_comunicados = function(vector_pdf_names){

  todos_resultados <- list()

  # Procesar cada archivo PDF en pdf_files
  for (file in vector_pdf_names) {
    # Leer el texto del archivo PDF
    texto <- pdf_text(file)

    # Limpiar el texto
    texto_limpio <- str_replace_all(texto, "\\s+", " ")

    # Separar el texto en bloques
    bloques <- str_split(texto_limpio, "DISPONER LA SUSPENSIÓN PREVENTIVA DE LA ACTIVIDAD EXTRACTIVA")[[1]]

    # Inicializar lista para almacenar los resultados de cada archivo PDF
    resultados <- list()

    # Recorrer cada bloque (ignorando el primer bloque que no contiene información relevante)
    for (i in 2:length(bloques)) {
      bloque <- bloques[i]

      # Extraer las fechas y horas de inicio y fin
      pattern_fechas <- "(\\d{2}:\\d{2} horas del \\d{2} de \\w+ de \\d{4})"
      fechas <- gsub("horas del ", "", str_extract_all(bloque, pattern_fechas)[[1]])
      fechas_final <- as.POSIXct(fechas, format = "%H:%M %d de %B de %Y", tz = "America/Lima")

      # Extraer las coordenadas de latitud y longitud
      pattern_coordenadas_lat <- "\\d{1,2}°\\d{1,2}['’][NS]"
      pattern_coordenadas_lon <- "\\d{1,2}°\\d{1,2}['’][WE]"
      data_posiciones_lat <- str_extract_all(bloque, pattern_coordenadas_lat)[[1]]
      data_posiciones_lon <- str_extract_all(bloque, pattern_coordenadas_lon)[[1]]

      # Verificar si hay longitudes presentes
      if (length(data_posiciones_lon) == 0) {
        # Extraer las millas náuticas si no hay longitudes
        # pattern_millas <- "(?<=dentro de las )\\d+"
        pattern_millas <- "(?:entre las )?(\\d+) y (\\d+) millas náuticas|dentro de las (\\d+) millas náuticas"

        # Obtener todos los textos que contienen millas náuticas
        textos_millas <- str_extract_all(bloque, pattern_millas)[[1]]

        # Inicializar vectores para almacenar las millas de inicio y fin
        millas_inicio <- numeric()
        millas_fin <- numeric()

        # Procesar cada texto de millas náuticas
        for (texto_millas in textos_millas) {
          # Extraer los números de millas náuticas del texto actual
          numeros_millas <- as.numeric(str_extract_all(texto_millas, "\\d+")[[1]])

          # Verificar si se extrajeron números de millas náuticas
          if (length(numeros_millas) > 0) {
            # Si hay un solo número, se asigna a millas_fin
            if (length(numeros_millas) == 1) {
              millas_inicio <- c(millas_inicio, NA)
              millas_fin <- c(millas_fin, numeros_millas)
            } else {
              # Ordenar los números para obtener las millas iniciales y finales en pares
              numeros_millas_ordenados <- sort(numeros_millas)

              # Llenar los vectores con los pares de millas
              for (i in seq(1, length(numeros_millas_ordenados), by = 2)) {
                millas_inicio <- c(millas_inicio, numeros_millas_ordenados[i])
                millas_fin <- c(millas_fin, numeros_millas_ordenados[i + 1])
              }
            }
          } else {
            # Si no se encontraron números, se asignan NA a ambas variables
            millas_inicio <- c(millas_inicio, NA)
            millas_fin <- c(millas_fin, NA)
          }
        }

        # Combinar fechas y posiciones en un data.frame
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

          # Agregar el resultado a la lista
          resultados <- append(resultados, list(df))
        }
      } else {
        # Combinar fechas y posiciones en un data.frame
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

          # Agregar el resultado a la lista
          resultados <- append(resultados, list(df))
        }
      }
    }

    # Combinar todos los data.frames en uno solo para este archivo PDF
    resultados_finales <- do.call(rbind, resultados)

    # Agregar el resultado a la lista de todos los resultados
    todos_resultados <- append(todos_resultados, list(resultados_finales))
  }

  # Combinar los resultados de todos los archivos PDF en un solo data.frame
  todos_resultados_finales <- do.call(rbind, todos_resultados)

  return(todos_resultados_finales)

}
