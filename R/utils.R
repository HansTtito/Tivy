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



# Función auxiliar para crear polígonos a partir de puntos
crear_poligonos <- function(datos,
                            lat_ini_col, lat_fin_col,
                            lon_ini_col, lon_fin_col,
                            millas_ini_col = NULL, millas_fin_col = NULL) {

  # Inicializar lista para polígonos
  poligonos <- list()

  # Crear polígonos para cada fila
  for (i in 1:nrow(datos)) {
    lat_min <- min(datos[[lat_ini_col]][i], datos[[lat_fin_col]][i])
    lat_max <- max(datos[[lat_ini_col]][i], datos[[lat_fin_col]][i])
    lon_min <- min(datos[[lon_ini_col]][i], datos[[lon_fin_col]][i])
    lon_max <- max(datos[[lon_ini_col]][i], datos[[lon_fin_col]][i])

    # Si tenemos millas náuticas, ajustar longitudes
    if (!is.null(millas_ini_col) && !is.null(millas_fin_col)) {
      # Convertir millas a grados de longitud (aproximación)
      # 1 minuto de longitud en ecuador ~ 1 milla náutica
      # Ajustar según latitud
      cos_lat <- cos(mean(c(lat_min, lat_max)) * pi/180)
      millas_min <- datos[[millas_ini_col]][i]
      millas_max <- datos[[millas_fin_col]][i]

      # Convertir millas a grados (aproximado)
      lon_offset_min <- millas_min / 60 / cos_lat
      lon_offset_max <- millas_max / 60 / cos_lat

      # Ajustar longitudes
      lon_min <- lon_min - lon_offset_min
      lon_max <- lon_min - lon_offset_max
    }

    # Crear coordenadas del polígono (rectángulo)
    coords <- rbind(
      c(lon_min, lat_min),  # Esquina inferior izquierda
      c(lon_max, lat_min),  # Esquina inferior derecha
      c(lon_max, lat_max),  # Esquina superior derecha
      c(lon_min, lat_max),  # Esquina superior izquierda
      c(lon_min, lat_min)   # Cerrar el polígono
    )

    # Crear geometría de polígono
    poligono <- sf::st_polygon(list(coords))

    # Añadir metadatos
    atributos <- datos[i, ]

    # Crear objeto sf
    poligono_sf <- sf::st_sf(
      geometry = sf::st_sfc(poligono, crs = 4326),
      id = i,
      fecha_inicio = if ("FechaHoraInicio" %in% names(datos)) datos$FechaHoraInicio[i] else NA,
      fecha_fin = if ("FechaHoraFin" %in% names(datos)) datos$FechaHoraFin[i] else NA,
      nombre_archivo = if ("nombre_archivo" %in% names(datos)) datos$nombre_archivo[i] else NA,
      comunicado = if ("comunicado" %in% names(datos)) datos$comunicado[i] else paste("Polígono", i)
    )

    poligonos[[i]] <- poligono_sf
  }

  # Combinar todos los polígonos en un solo objeto sf
  poligonos_sf <- do.call(rbind, poligonos)
  return(poligonos_sf)
}

# Función auxiliar para calcular la longitud aproximada de la costa a una latitud dada
calcular_longitud_costa <- function(costa, latitud) {
  # Encontrar los puntos de costa más cercanos a la latitud dada
  idx_cercanos <- which(abs(costa$Lat - latitud) < 0.1)

  if (length(idx_cercanos) == 0) {
    warning("No se encontraron puntos costeros cercanos a la latitud ", latitud, ". Usando aproximación.")
    return(-75)  # Valor aproximado para la costa peruana
  }

  # Calcular la longitud promedio de estos puntos
  longitud_costa <- mean(costa$Long[idx_cercanos])
  return(longitud_costa)
}


# Función para gráfico estático con ggplot2
graficar_estatico <- function(poligonos, costa, titulo, colores, mostrar_leyenda = TRUE,
                              etiquetas = NULL, agregar_grid = TRUE, tema = ggplot2::theme_minimal()) {

  p <- ggplot2::ggplot()

  # Línea de costa
  p <- p + ggplot2::geom_path(data = costa, ggplot2::aes(x = Long, y = Lat),
                              color = "darkgrey", linewidth = 0.5)

  if(is.null(colores)){
    colores <- RColorBrewer::brewer.pal(n = length(poligonos), name = "Set3")
  }

  # Construir data.frame para todos los polígonos juntos
  lista_df <- list()
  for (i in seq_along(poligonos)) {
    poligono <- poligonos[[i]]
    color_idx <- (i - 1) %% length(colores) + 1

    etiqueta <- if (!is.null(etiquetas) && length(etiquetas) >= i) {
      etiquetas[i]
    } else {
      poligono$comunicado
    }

    coords_df <- as.data.frame(poligono$coords)
    colnames(coords_df) <- c("Long", "Lat")
    coords_df$grupo <- etiqueta

    coords_df$id <- poligono$id

    lista_df[[i]] <- coords_df
  }

  df_poligonos <- do.call(rbind, lista_df)

  # Polígonos con relleno por comunicado
  p <- p + ggplot2::geom_polygon(
    data = df_poligonos,
    ggplot2::aes(x = Long, y = Lat, fill = grupo, group = id),
    alpha = 0.7,
    color = "black"
  )

  # Colores
  p <- p + ggplot2::scale_fill_manual(values = colores, name = "Comunicados")

  # Etiquetas y tema
  p <- p + ggplot2::labs(
    title = titulo,
    x = "Longitud",
    y = "Latitud",
    caption = "Fuente: PRODUCE"
  ) + tema

  # Cuadrícula
  if (agregar_grid) {
    p <- p + ggplot2::coord_quickmap() +
      ggplot2::scale_x_continuous(breaks = seq(-82, -70, by = 2)) +
      ggplot2::scale_y_continuous(breaks = seq(-20, -8, by = 2)) +
      ggplot2::theme(panel.grid = ggplot2::element_line(color = "lightgrey", linetype = "dashed"))
  } else {
    p <- p + ggplot2::coord_quickmap()
  }

  # Leyenda
  if (!mostrar_leyenda) {
    p <- p + ggplot2::theme(legend.position = "none")
  } else {
    p <- p + ggplot2::theme(
      legend.position = "right",
      legend.background = ggplot2::element_rect(fill = "white", color = "grey80"),
      legend.margin = ggplot2::margin(5, 5, 5, 5)
    )
  }

  return(p)
}


# Función para gráfico interactivo con leaflet
graficar_interactivo <- function(poligonos, costa, titulo, colores, mostrar_leyenda = TRUE,
                                 etiquetas = NULL, capas_base = TRUE, minimap = TRUE) {

  if(is.null(colores)){
    colores <- RColorBrewer::brewer.pal(n = length(poligonos), name = "Set3")
  }

  # Crear el mapa base
  mapa <- leaflet::leaflet() %>%
    leaflet::addTiles(group = "OpenStreetMap")

  # Añadir capas base adicionales si se solicita
  if (capas_base) {
    mapa <- mapa %>%
      leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap, group = "Océano") %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group = "Simple") %>%
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Satélite") %>%
      leaflet::addLayersControl(
        baseGroups = c("OpenStreetMap", "Océano", "Simple", "Satélite"),
        position = "topright"
      )
  }

  # Añadir título
  if (!is.null(titulo)) {
    mapa <- mapa %>%
      leaflet::addControl(
        html = paste0("<h4>", titulo, "</h4>"),
        position = "topleft"
      )
  }

  # Añadir la línea de costa
  mapa <- mapa %>%
    leaflet::addPolylines(
      data = costa,
      lng = ~Long,
      lat = ~Lat,
      color = "darkgrey",
      weight = 1.5,
      opacity = 0.8,
      group = "Línea de Costa"
    )

  # Crear grupo de capas para la leyenda
  overlay_groups <- c()

  # Añadir cada polígono
  for (i in seq_along(poligonos)) {
    poligono <- poligonos[[i]]
    color_idx <- (i - 1) %% length(colores) + 1

    # Si hay etiquetas, usarlas para la leyenda
    etiqueta <- if (!is.null(etiquetas) && length(etiquetas) >= i) {
      etiquetas[i]
    } else {
      # Extraer nombre del comunicado como etiqueta por defecto
      poligono$comunicado
    }

    # Guardar para la leyenda
    overlay_groups <- c(overlay_groups, etiqueta)

    # Preparar popup más detallado
    popup_content <- paste0(
      "<div style='max-width: 300px;'>",
      "<h4>", etiqueta, "</h4>",
      "<strong>Comunicado: </strong>", poligono$comunicado, "<br>",
      "<strong>Fecha inicio: </strong>", format(poligono$fecha_inicio, "%d/%m/%Y %H:%M"), "<br>",
      "<strong>Fecha fin: </strong>", format(poligono$fecha_fin, "%d/%m/%Y %H:%M"), "<br>",
      "<strong>Lat Ini: </strong>", poligono$Lat_Ini, "<br>",
      "<strong>Lon Ini: </strong>", poligono$Long_Ini, "<br>",
      "<strong>Lat Fin: </strong>", poligono$Lat_Fin, "<br>",
      "<strong>Lon Fin: </strong>", poligono$Long_Fin, "<br>",
      "<strong>Millas Nauticas Inicio: </strong>", poligono$MillasNauticasInicio, "<br>",
      "<strong>Millas Nauticas Fin: </strong>", poligono$MillasNauticasFin, "<br>",
      "</div>"
    )

    # Convertir coordenadas al formato que espera leaflet
    leaflet_coords <- list(
      lng = poligono$coords[, 1],
      lat = poligono$coords[, 2]
    )

    mapa <- mapa %>%
      leaflet::addPolygons(
        lng = leaflet_coords$lng,
        lat = leaflet_coords$lat,
        fillColor = colores[color_idx],
        fillOpacity = 0.7,
        color = "black",
        weight = 1,
        popup = popup_content,
        group = etiqueta,
        label = etiqueta,
        highlightOptions = leaflet::highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.8,
          bringToFront = TRUE
        )
      )
  }

  # Añadir control de capas para los polígonos si se solicita leyenda
  if (mostrar_leyenda && length(overlay_groups) > 0) {
    mapa <- mapa %>%
      leaflet::addLayersControl(
        baseGroups = if (capas_base) c("OpenStreetMap", "Océano", "Simple", "Satélite") else "OpenStreetMap",
        overlayGroups = overlay_groups,
        position = "topright",
        options = leaflet::layersControlOptions(collapsed = FALSE)
      )
  }

  # Añadir escala
  mapa <- mapa %>%
    leaflet::addScaleBar(position = "bottomleft", options = leaflet::scaleBarOptions(imperial = FALSE))

  # Añadir minimapa si se solicita
  if (minimap && requireNamespace("leaflet", quietly = TRUE)) {
    mapa <- mapa %>%
      leaflet::addMiniMap(
        tiles = leaflet::providers$CartoDB.Positron,
        toggleDisplay = TRUE,
        position = "bottomright"
      )
  }

  # Calcular los límites del mapa
  all_longs <- unlist(lapply(poligonos, function(p) p$coords[, 1]))
  all_lats <- unlist(lapply(poligonos, function(p) p$coords[, 2]))

  # Ajustar vista a los límites de los datos (con un margen)
  mapa <- mapa %>%
    leaflet::fitBounds(
      lng1 = min(all_longs) - 0.5,
      lat1 = min(all_lats) - 0.5,
      lng2 = max(all_longs) + 0.5,
      lat2 = max(all_lats) + 0.5
    )

  return(mapa)
}
