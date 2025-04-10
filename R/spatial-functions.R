#' Convertir latitud y longitud a grados decimales
#' @description
#' Función que permite convertir longitud o latitude en grados a decimales.
#'
#' @param coordenadas Un vector con las longitudes o latitudes a convertir
#' @return Un vector con las longitudes o latitudes convertidas a decimales
#' @export
#' @rdname dms_a_decimal
#' @examples
#' dms_a_decimal(calas$Longitud.Fin)
dms_a_decimal <- function(coordenadas, hemisferio = "S") {
  # Validación de entrada
  if (!is.character(coordenadas)) {
    stop("Las coordenadas deben ser una cadena de texto")
  }

  # Determinar el signo según el hemisferio
  signo <- ifelse(hemisferio %in% c("S", "W", "O"), -1, 1)

  # Intentar convertir usando tryCatch para manejar errores
  resultados <- tryCatch({
    # Dividir la cadena en componentes
    componentes <- str_split_fixed(coordenadas, " ", n = 3)

    # Eliminar caracteres no numéricos y convertir a numérico
    valores_numericos <- apply(componentes, 2, function(x) {
      # Eliminar °, ', " y otros caracteres no numéricos
      numeros <- gsub(pattern = "[°'\"]", replacement = "", x = x)
      # Convertir a numérico
      as.numeric(numeros)
    })

    # Calcular el valor decimal
    decimal <- signo * (valores_numericos[, 1] +
                          valores_numericos[, 2] / 60 +
                          valores_numericos[, 3] / 3600)

    return(decimal)
  }, error = function(e) {
    warning("Error al procesar las coordenadas: ", e$message)
    return(NA)
  })

  return(resultados)
}


#' Distancia a la Costa vectorizado
#' @description
#' Función que permite estimar la distancia entre puntos hacia una lista de puntos
#'
#' @param lon Un vector con las longitudes
#' @param lat Un vector con las latitudes
#' @param linea_costa Un data frame con las coordenadas de la costa, el orden debe ser: lon, lat
#' @return Un vector con las distancias a la costa
#' @export
#' @rdname distancia_costa
#' @examples
#' data_calas = processing_calas(data_calas = calas)
#' distancia_costa(lon = data_calas$lon_final, lat = data_calas$lat_final, linea_costa = Shoreline_Peru)
# spatial-functions.R
distancia_costa <- function(lon, lat, linea_costa,
                            devolver_indices = FALSE,
                            tipo_distancia = "haversine",
                            unidad = "mn",
                            ventana = 2,
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
    Tivy:::calcular_distancias_vectorizado(
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
