#' Convertir latitud o longitud a grados decimales
#'
#' @description
#' Convierte coordenadas expresadas en formato grados, minutos y segundos (DMS) o grados y minutos (DM) a grados decimales.
#' Por defecto, se asume que las coordenadas están en el hemisferio sur (latitudes negativas).
#'
#' @param coordenadas Vector de caracteres. Cada elemento debe estar en formatos como:
#'   - Con símbolos: `"G° M' S\""`, `"G° M'"`, `"17°26'S"`
#'   - Sin símbolos: `"G M S"`, `"G M"`, `"17 26 S"`
#'   - El hemisferio puede estar incluido en la coordenada
#' @param hemisferio Caracter `"N"`, `"S"`, `"E"`, `"W"` o `"O"` que indica el hemisferio correspondiente
#' cuando no está especificado en la coordenada. `"S"` y `"W"`/`"O"` generan valores negativos. Default: `"S"`.
#'
#' @return Un vector numérico con las coordenadas convertidas a grados decimales.
#' @export
#'
#' @examples
#' # Convertir coordenadas del sur (formato completo)
#' dms_a_decimal(c("39° 48' 36\""), hemisferio = "S")
#'
#' # Convertir coordenadas del oeste (formato completo sin símbolos)
#' dms_a_decimal(c("73 15 0"), hemisferio = "O")
#'
#' # Convertir coordenadas con solo grados y minutos
#' dms_a_decimal(c("39° 48'"), hemisferio = "S")
#' dms_a_decimal(c("73 15"), hemisferio = "W")
#'
#' # Coordenadas con hemisferio incluido
#' dms_a_decimal(c("17°26'S"))
#' dms_a_decimal(c("73°15'W"))
#' dms_a_decimal(c("39 48 N"))
#'
#' # En un dataframe
#' # dms_a_decimal(calas$Longitud.Fin)
#'
#' @importFrom stringr str_split str_count str_detect str_extract
dms_a_decimal <- function(coordenadas, hemisferio = "S") {
  # Validación de entrada
  if (!is.character(coordenadas)) {
    stop("Las coordenadas deben ser una cadena de texto o un vector de caracteres.")
  }
  if (!hemisferio %in% c("N", "S", "E", "W", "O")) {
    stop("El hemisferio debe ser uno de: 'N', 'S', 'E', 'W' o 'O'.")
  }

  # Procesar cada coordenada
  resultados <- vapply(coordenadas, function(coord) {
    tryCatch({
      # Detectar si la coordenada incluye el hemisferio
      hemisferio_local <- hemisferio
      coord_original <- coord

      # Buscar indicadores de hemisferio (N, S, E, W, O)
      patron_hemisferio <- "[NSEW]|O"
      hemisferio_encontrado <- regmatches(coord, regexpr(patron_hemisferio, coord))

      if (length(hemisferio_encontrado) > 0 && hemisferio_encontrado != "") {
        hemisferio_local <- hemisferio_encontrado
        # Eliminar el hemisferio de la coordenada
        coord <- gsub(patron_hemisferio, "", coord)
      }

      # Determinar el signo según el hemisferio
      signo <- ifelse(hemisferio_local %in% c("S", "W", "O"), -1, 1)

      # Limpiar y normalizar la coordenada
      # Reemplazar grados, minutos y segundos con espacios
      coord_limpia <- gsub("[°'\"]", " ", coord)
      # Eliminar espacios múltiples
      coord_limpia <- gsub("\\s+", " ", coord_limpia)
      # Eliminar espacios al inicio y final
      coord_limpia <- trimws(coord_limpia)

      # Dividir en componentes
      componentes <- unlist(strsplit(coord_limpia, " "))
      # Filtrar componentes no numéricos
      componentes_num <- componentes[grepl("^[0-9]+(\\.[0-9]+)?$", componentes)]
      partes <- as.numeric(componentes_num)

      # Calcular según el número de componentes
      if (length(partes) == 3) {
        # Formato completo: grados, minutos, segundos
        decimal <- signo * (partes[1] + partes[2] / 60 + partes[3] / 3600)
      } else if (length(partes) == 2) {
        # Formato parcial: solo grados y minutos
        decimal <- signo * (partes[1] + partes[2] / 60)
      } else if (length(partes) == 1) {
        # Solo grados
        decimal <- signo * partes[1]
      } else {
        warning(paste("Formato no reconocido para la coordenada:", coord_original))
        return(NA_real_)
      }

      return(decimal)
    }, error = function(e) {
      warning(paste("Error al procesar la coordenada", coord, ":", e$message))
      return(NA_real_)
    })
  }, FUN.VALUE = numeric(1))

  # Asegurarse de que el resultado no tenga nombres
  names(resultados) <- NULL

  return(resultados)
}



#' Distancia a la costa vectorizado
#'
#' @description
#' Estima la distancia entre un conjunto de puntos (lon, lat) y una línea de costa definida por coordenadas.
#' Se puede ejecutar de forma secuencial o en paralelo, y retornar también los índices de los puntos costeros más cercanos.
#'
#' @param lon Vector numérico con las longitudes de los puntos de interés.
#' @param lat Vector numérico con las latitudes de los puntos de interés.
#' @param linea_costa Data frame que representa la línea de costa, debe contener columnas llamadas `'Long'` y `'Lat'`.
#' @param devolver_indices Lógico. Si es `TRUE`, devuelve también los índices de los puntos de la línea de costa más cercanos. Default `FALSE`.
#' @param tipo_distancia Tipo de distancia geográfica a usar: `"haversine"`, `"euclidean"`, `"grid"` .
#' @param unidad Unidad de medida para la distancia: `"mn"` (millas náuticas), `"km"`, etc.
#' @param ventana Ventana de búsqueda en grados alrededor del punto para limitar los cálculos y mejorar eficiencia. Default `1`.
#' @param paralelo Lógico. Si `TRUE`, realiza el cálculo en paralelo utilizando múltiples núcleos. Default `FALSE`.
#' @param nucleos Número de núcleos a usar para procesamiento paralelo. Default `4`.
#'
#' @return Si `devolver_indices = FALSE`, devuelve un vector numérico con las distancias a la costa para cada punto.
#'         Si `devolver_indices = TRUE`, devuelve una lista con:
#'         \itemize{
#'           \item \code{distancia}: vector numérico con las distancias a la costa
#'           \item \code{indice}: vector de índices del punto más cercano en la línea de costa
#'         }
#'
#' @export
#' @examples
#' \dontrun{
#' data_calas <- processing_calas(data_calas = calas)
#' distancia_costa(
#'   lon = data_calas$lon_final,
#'   lat = data_calas$lat_final,
#'   linea_costa = Shoreline_Peru,
#'   tipo_distancia = "haversine",
#'   unidad = "mn",
#'   paralelo = TRUE,
#'   nucleos = 2
#' )
#' }
#'
#' @importFrom future plan multisession
#' @importFrom future.apply future_lapply
distancia_costa <- function(lon, lat, linea_costa,
                            devolver_indices = FALSE,
                            tipo_distancia = "haversine",
                            unidad = "mn",
                            ventana = 1,
                            paralelo = FALSE,
                            nucleos = 4) {
  # Verificar estructura de datos
  if (!all(c("Long", "Lat") %in% colnames(linea_costa))) {
    stop("linea_costa debe contener columnas 'Long' y 'Lat'")
  }

  # Manejo de valores NA
  validos <- !is.na(lon) & !is.na(lat)
  lon_validos <- lon[validos]
  lat_validos <- lat[validos]

  # Salida temprana si no hay datos válidos
  if (length(lon_validos) == 0) return(rep(NA, length(validos)))

  # Dividir puntos en lotes
  n_puntos <- length(lon_validos)
  tamanio_lote <- ceiling(n_puntos / nucleos)
  lotes_indices <- split(seq_len(n_puntos), ceiling(seq_len(n_puntos) / tamanio_lote))

  # Cargar librería future.apply si se va a usar paralelo
  if (paralelo) {
    if (!requireNamespace("future.apply", quietly = TRUE)) {
      warning("Paquete 'future.apply' no encontrado. Usando procesamiento secuencial.")
      paralelo <- FALSE
    } else {
      future::plan(future::multisession, workers = nucleos)
    }
  }

  # Ejecutar en paralelo o secuencialmente
  apply_fun <- if (paralelo) future.apply::future_lapply else lapply

  resultados_lotes <- apply_fun(lotes_indices, function(indices_lote) {
    calcular_distancias_vectorizado(
      lon_validos[indices_lote],
      lat_validos[indices_lote],
      linea_costa$Long,
      linea_costa$Lat,
      tipo_distancia,
      ventana,
      unidad
    )
  })

  # Combinar resultados
  distancias <- unlist(lapply(resultados_lotes, `[[`, "distancias"))
  indices <- unlist(lapply(resultados_lotes, `[[`, "indices"))

  # Resultados con NAs en las posiciones originales
  resultado_final <- rep(NA_real_, length(validos))
  resultado_final[validos] <- distancias

  if (devolver_indices) {
    indices_final <- rep(NA_integer_, length(validos))
    indices_final[validos] <- indices
    return(list(distancia = resultado_final, indice = indices_final))
  } else {
    return(resultado_final)
  }
}



#' Puntos en tierra
#' @description
#' Función que permite estimar los puntos que están a la derecha o izquierda de la línea de costa
#'
#' @param lon Un vector con las longitudes
#' @param lat Un vector con las latitudes
#' @param linea_costa Un data frame con las coordenadas de la costa, el orden debe ser: lon, lat
#' @return Un vector con las distancias a la costa
#' @export
#' @rdname puntos_tierra
#' @examples
#' data_calas = processing_calas(data_calas = calas)
#' puntos_tierra(x_punto = data_calas$lon_final, y_punto = data_calas$lat_final, linea_costa = Shoreline_Peru)
#' @importFrom future plan multisession sequential
#' @importFrom future.apply future_lapply
puntos_tierra <- function(x_punto, y_punto, linea_costa, paralelo = FALSE, nucleos = 4) {
  # Asegurarse de que linea_costa tenga los nombres de columnas correctos
  if (!"Long" %in% colnames(linea_costa) || !"Lat" %in% colnames(linea_costa)) {
    stop("linea_costa debe contener columnas 'Long' y 'Lat'")
  }

  # Limpiar datos de entrada
  idx_validos <- !is.na(x_punto) & !is.na(y_punto)
  x_validos <- x_punto[idx_validos]
  y_validos <- y_punto[idx_validos]

  # Preparar vectores de resultados del mismo tamaño que los originales
  resultados <- rep(NA, length(x_punto))

  # Si no hay puntos válidos, devolver resultado vacío
  if (length(x_validos) == 0) {
    return(resultados)
  }

  # Constantes
  grados2rad <- pi / 180

  # Convertir a radianes
  lon_rad <- x_validos * grados2rad
  lat_rad <- y_validos * grados2rad
  shore_lon_rad <- linea_costa$Long * grados2rad
  shore_lat_rad <- linea_costa$Lat * grados2rad

  # Función para encontrar el índice del punto costero más cercano
  find_closest_shore <- function(i) {
    # Fórmula del gran círculo para calcular distancias
    xy_rad <- sin(lat_rad[i]) * sin(shore_lat_rad)
    yx_rad <- cos(lat_rad[i]) * cos(shore_lat_rad) * cos(shore_lon_rad - lon_rad[i])
    dist_rad <- acos(pmin(pmax(xy_rad + yx_rad, -1), 1))

    # Devolver índice del punto más cercano
    return(which.min(dist_rad))
  }

  if (paralelo && requireNamespace("future", quietly = TRUE) &&
      requireNamespace("future.apply", quietly = TRUE)) {
    # Configurar paralelización
    future::plan(future::multisession, workers = nucleos)

    # Calcular índices de los puntos más cercanos en paralelo
    closest_indices <- future.apply::future_lapply(
      1:length(x_validos),
      function(i) find_closest_shore(i),
      future.seed = TRUE
    )

    # Restaurar plan secuencial
    future::plan(future::sequential)

    # Desempaquetar resultado
    closest_indices <- unlist(closest_indices)
  } else {
    # Versión secuencial
    closest_indices <- sapply(1:length(x_validos), find_closest_shore)
  }

  # Determinar si cada punto está en tierra o mar
  for (i in 1:length(x_validos)) {
    idx_costa <- closest_indices[i]

    # Para la costa peruana (océano al oeste), determinar posición relativa
    # Si el punto está al este de la costa (longitud mayor), está en tierra
    resultados[idx_validos][i] <- ifelse(
      x_validos[i] > linea_costa$Long[idx_costa],
      "tierra",
      "mar"
    )
  }

  return(resultados)
}
