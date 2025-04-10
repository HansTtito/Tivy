# Función para calcular distancia usando la fórmula de Haversine modificada
# (con elipsoide WGS84)
calcular_distancia_haversine_wgs84 <- function(lon1, lat1, lon2, lat2, unidad) {
  # Convertir grados a radianes (verificando si ya están en radianes)
  if (max(abs(lon1), abs(lat1), abs(lon2), abs(lat2)) > 2*pi) {
    lon1 <- lon1 * pi/180
    lat1 <- lat1 * pi/180
    lon2 <- lon2 * pi/180
    lat2 <- lat2 * pi/180
  }

  # Radio ecuatorial y polar de la Tierra según WGS84
  a <- 6378137  # Radio ecuatorial en metros
  b <- 6356752.3142  # Radio polar en metros

  # Factor de conversión según unidad
  factor <- ifelse(unidad == "mn", 0.000539957, 0.001)  # a millas náuticas o km

  # Radio medio en la latitud dada
  R <- sqrt(((a^2 * cos(lat1))^2 + (b^2 * sin(lat1))^2) /
              ((a * cos(lat1))^2 + (b * sin(lat1))^2))

  # Vectorizar cálculo de distancias (sin bucle for)
  distancias <- vapply(seq_along(lon2), function(i) {
    dlon <- lon2[i] - lon1
    dlat <- lat2[i] - lat1
    a_calc <- sin(dlat/2)^2 + cos(lat1) * cos(lat2[i]) * sin(dlon/2)^2
    c <- 2 * atan2(sqrt(a_calc), sqrt(1-a_calc))
    R * c * factor
  }, numeric(1))

  # Encontrar el mínimo y su índice
  min_idx <- which.min(distancias)
  min_dist <- distancias[min_idx]

  # Devolver tanto la distancia mínima como su índice
  return(list(distancia = min_dist, indice = min_idx))
}

# Función para calcular distancia de Manhattan (city block)
calcular_distancia_manhattan <- function(lon1, lat1, lon2, lat2, unidad) {
  # Verificar si convertir a radianes
  if (max(abs(lon1), abs(lat1), abs(lon2), abs(lat2)) > 2*pi) {
    lon1 <- lon1 * pi/180
    lat1 <- lat1 * pi/180
    lon2 <- lon2 * pi/180
    lat2 <- lat2 * pi/180
  }

  R <- ifelse(unidad == "mn", 3440, 6371)  # Millas náuticas o kilómetros

  # Vectorizar cálculo
  distancias <- vapply(seq_along(lon2), function(i) {
    # Calcular distancia en dirección x (longitud)
    dx <- abs(lon2[i] - lon1) * cos((lat1 + lat2[i])/2) * R
    # Calcular distancia en dirección y (latitud)
    dy <- abs(lat2[i] - lat1) * R
    dx + dy
  }, numeric(1))

  # Encontrar el mínimo y su índice
  min_idx <- which.min(distancias)
  min_dist <- distancias[min_idx]

  return(list(distancia = min_dist, indice = min_idx))
}

# Función para calcular distancia de cuadrícula (grid) con resolución específica
calcular_distancia_grid <- function(lon1, lat1, lon2, lat2, resolucion = 0.25, unidad = "km") {
  # Redondear a la resolución de cuadrícula más cercana
  lon1_grid <- round(lon1 / resolucion) * resolucion
  lat1_grid <- round(lat1 / resolucion) * resolucion

  # Vectorizar cálculo
  distancias <- vapply(seq_along(lon2), function(i) {
    lon2_grid <- round(lon2[i] / resolucion) * resolucion
    lat2_grid <- round(lat2[i] / resolucion) * resolucion

    # Convertir a radianes si es necesario
    if (max(abs(lon1_grid), abs(lat1_grid), abs(lon2_grid), abs(lat2_grid)) > 2*pi) {
      lon1_grid <- lon1_grid * pi/180
      lat1_grid <- lat1_grid * pi/180
      lon2_grid <- lon2_grid * pi/180
      lat2_grid <- lat2_grid * pi/180
    }

    R <- ifelse(unidad == "mn", 3440, 6371)  # Millas náuticas o kilómetros

    dlat <- lat2_grid - lat1_grid
    dlon <- lon2_grid - lon1_grid
    a <- sin(dlat/2)^2 + cos(lat1_grid) * cos(lat2_grid) * sin(dlon/2)^2
    c <- 2 * atan2(sqrt(a), sqrt(1-a))
    R * c
  }, numeric(1))

  # Encontrar el mínimo y su índice
  min_idx <- which.min(distancias)
  min_dist <- distancias[min_idx]

  return(list(distancia = min_dist, indice = min_idx))
}


# Función principal: Vectorización por lotes para cada punto
calcular_distancias_vectorizado <- function(lon_punto, lat_punto, costa_lon, costa_lat, tipo_distancia, ventana, unidad) {
  n_puntos <- length(lon_punto)
  distancias_finales <- numeric(n_puntos)
  indices_finales <- integer(n_puntos)

  # Procesamiento por punto, pero con cálculos vectorizados internos
  for (i in seq_len(n_puntos)) {
    # Filtrado espacial por ventana geográfica (opcional pero eficiente)
    if (ventana > 0) {
      idx_filtro <-
        costa_lat >= (lat_punto[i] - ventana) &
        costa_lat <= (lat_punto[i] + ventana)

      # Si no hay puntos en la ventana, usar toda la costa
      if (sum(idx_filtro) < 2) {
        idx_filtro <- rep(TRUE, length(costa_lat))
      }

      costa_lon_filtrada <- costa_lon[idx_filtro]
      costa_lat_filtrada <- costa_lat[idx_filtro]
    } else {
      # Sin filtrado espacial
      costa_lon_filtrada <- costa_lon
      costa_lat_filtrada <- costa_lat
    }

    # Cálculo vectorizado según el método elegido
    resultado <- switch(
      tipo_distancia,
      "haversine" = Tivy:::calcular_distancia_haversine_wgs84(
        lon_punto[i], lat_punto[i], costa_lon_filtrada, costa_lat_filtrada, unidad
      ),
      "manhattan" = Tivy:::calcular_distancia_manhattan(
        lon_punto[i], lat_punto[i], costa_lon_filtrada, costa_lat_filtrada, unidad
      ),
      "grid" = Tivy:::calcular_distancia_grid(
        lon_punto[i], lat_punto[i], costa_lon_filtrada, costa_lat_filtrada, unidad
      ),
      stop("Tipo de distancia no válido")
    )

    # Almacenar resultados
    distancias_finales[i] <- resultado$distancia

    # Convertir índice local a global si se usó filtrado
    if (ventana > 0) {
      indices_originales <- which(idx_filtro)
      indices_finales[i] <- indices_originales[resultado$indice]
    } else {
      indices_finales[i] <- resultado$indice
    }
  }

  return(list(distancias = distancias_finales, indices = indices_finales))
}
