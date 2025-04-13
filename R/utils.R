#' Calcular distancia usando la fórmula de Haversine con el elipsoide WGS84
#'
#' @param lon1 Longitud del punto de origen (en grados o radianes).
#' @param lat1 Latitud del punto de origen (en grados o radianes).
#' @param lon2 Vector de longitudes destino (en grados o radianes).
#' @param lat2 Vector de latitudes destino (en grados o radianes).
#' @param unidad Unidad de salida: "mn" para millas náuticas, "km" para kilómetros.
#'
#' @return Lista con la distancia mínima y el índice correspondiente.
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



#' Calcular distancia tipo Manhattan (bloques de ciudad) entre coordenadas
#'
#' @param lon1 Longitud del punto de origen.
#' @param lat1 Latitud del punto de origen.
#' @param lon2 Vector de longitudes destino.
#' @param lat2 Vector de latitudes destino.
#' @param unidad Unidad de distancia: "mn" o "km".
#'
#' @return Lista con la distancia mínima y el índice correspondiente.
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



#' Calcular distancia de cuadrícula con resolución fija
#'
#' @param lon1 Longitud del punto base.
#' @param lat1 Latitud del punto base.
#' @param lon2 Vector de longitudes de destino.
#' @param lat2 Vector de latitudes de destino.
#' @param resolucion Resolución de la cuadrícula (por defecto 0.25 grados).
#' @param unidad Unidad de distancia: "mn" o "km".
#'
#' @return Lista con distancia mínima y su índice.
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



#' Calcular distancias entre múltiples puntos y una línea costera (vectorizado)
#'
#' @param lon_punto Vector de longitudes de puntos a comparar.
#' @param lat_punto Vector de latitudes de puntos a comparar.
#' @param costa_lon Vector de longitudes de la costa.
#' @param costa_lat Vector de latitudes de la costa.
#' @param tipo_distancia Tipo de distancia: "haversine", "manhattan" o "grid".
#' @param ventana Tamaño de la ventana (en grados) para filtrar la costa. Si es 0, se usa toda la costa.
#' @param unidad Unidad de distancia: "mn" o "km".
#'
#' @return Lista con vectores de distancias mínimas e índices correspondientes.
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


#' Calcular la longitud aproximada de la costa en una latitud específica
#'
#' Esta función encuentra la longitud aproximada de una línea costera en una latitud dada,
#' buscando puntos costeros cercanos y calculando su longitud promedio.
#'
#' @param costa Un dataframe que contiene datos de la línea costera con al menos dos columnas:
#'        'Lat' (latitud) y 'Long' (longitud) en grados decimales.
#' @param latitud Valor numérico que representa la latitud en grados decimales para la cual
#'        se desea encontrar la longitud de la costa.
#'
#' @return Un valor numérico que representa la longitud promedio en grados decimales de los
#'        puntos costeros cercanos a la latitud especificada. Si no se encuentran puntos cercanos,
#'        devuelve -75 como aproximación para la costa peruana y muestra una advertencia.
#'
#' @details
#' La función busca puntos costeros dentro de un rango de 0.1 grados de la latitud
#' especificada. Este umbral puede necesitar ajustes dependiendo de la densidad de puntos en el dataset.
#'
#' @note
#' - Requiere que el dataframe de entrada tenga la estructura correcta con columnas 'Lat' y 'Long'.
#' - El valor predeterminado de -75 para la costa peruana solo es una aproximación general y
#'   puede no ser adecuado para otras regiones.
#' - Esta función no realiza interpolaciones y puede dar resultados inexactos en costas con
#'   formas muy irregulares o bahías profundas.
#' - Funciona mejor cuando los datos costeros tienen una distribución uniforme de puntos.
calcular_longitud_costa <- function(costa, latitud) {
  # Verificar que el dataframe costa contenga las columnas necesarias
  if (!all(c("Lat", "Long") %in% names(costa))) {
    stop("El dataframe 'costa' debe contener las columnas 'Lat' y 'Long'")
  }

  # Verificar que latitud sea un valor numérico
  if (!is.numeric(latitud)) {
    stop("El parámetro 'latitud' debe ser un valor numérico")
  }

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


#' Generar gráfico estático de polígonos en un mapa
#'
#' @description
#' Crea un gráfico estático utilizando `ggplot2` que muestra polígonos geográficos sobre una base de línea de costa.
#'
#' @param poligonos Lista de polígonos. Cada polígono debe contener una matriz `coords` con columnas de longitud y latitud.
#' @param costa Data frame con la línea de costa, con columnas `Long` y `Lat`.
#' @param titulo Título principal del gráfico.
#' @param colores Vector de colores para rellenar los polígonos. Si es `NULL`, se asignan colores automáticamente.
#' @param mostrar_leyenda Lógico. Si `TRUE`, se muestra la leyenda.
#' @param etiquetas Vector de etiquetas para los polígonos (opcional).
#' @param agregar_grid Lógico. Si `TRUE`, agrega una cuadrícula geográfica al gráfico.
#' @param tema Tema de `ggplot2` a utilizar. Por defecto, `theme_minimal()`.
#'
#' @return Un objeto `ggplot` listo para ser graficado.
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


#' Generar gráfico interactivo de polígonos con leaflet
#'
#' @description
#' Crea un mapa interactivo utilizando `leaflet`, mostrando polígonos con información emergente (popup).
#'
#' @param poligonos Lista de polígonos. Cada uno debe tener campos como `coords`, `comunicado`, fechas y coordenadas.
#' @param costa Data frame con la línea de costa (columnas `Long` y `Lat`).
#' @param titulo Título a mostrar en la parte superior del mapa.
#' @param colores Vector de colores. Si `NULL`, se asignan automáticamente con `RColorBrewer::Set3`.
#' @param mostrar_leyenda Lógico. Si `TRUE`, se muestra el control de capas (leyenda).
#' @param etiquetas Vector opcional de nombres para mostrar en la leyenda y etiquetas del mapa.
#' @param capas_base Lógico. Si `TRUE`, se incluyen capas base como mapas satelitales y oceánicos.
#' @param minimap Lógico. Si `TRUE`, se muestra un minimapa en la esquina inferior derecha.
#'
#' @return Un objeto `leaflet` con el mapa interactivo.
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



#' Preparar polígonos a partir de datos de coordenadas
#'
#' @description
#' Función auxiliar para procesar datos y preparar polígonos.
#'
#' @param datos Un data frame con coordenadas.
#' @param costa Un data frame con la línea de costa.
#'
#' @return Una lista de polígonos para visualización.
#' @keywords internal
preparar_poligonos <- function(datos, costa) {
  # Validación de parámetros
  if (missing(datos)) {
    stop("El parámetro 'datos' es obligatorio.")
  }

  if (!is.data.frame(datos)) {
    stop("'datos' debe ser un data.frame.")
  }

  if (!is.data.frame(costa) || !all(c("Long", "Lat") %in% names(costa))) {
    stop("'costa' debe ser un data.frame con columnas 'Long' y 'Lat'.")
  }

  # Verificar si hay coordenadas para trabajar
  if (nrow(datos) == 0) {
    stop("El data frame 'datos' no contiene filas.")
  }

  # Preparar los datos para la visualización
  datos_preparados <- datos

  # Convertir coordenadas de texto a numéricas si es necesario
  if (any(c("LatitudInicio", "LatitudFin") %in% names(datos))) {
    if (!"lat_ini" %in% names(datos)) {
      datos_preparados$lat_ini <- Tivy::dms_a_decimal(datos$LatitudInicio)
    }
    if (!"lat_fin" %in% names(datos)) {
      datos_preparados$lat_fin <- Tivy::dms_a_decimal(datos$LatitudFin)
    }
  }

  if (any(c("LongitudInicio", "LongitudFin") %in% names(datos))) {
    if (!"lon_ini" %in% names(datos)) {
      datos_preparados$lon_ini <- Tivy::dms_a_decimal(datos$LongitudInicio)
    }
    if (!"lon_fin" %in% names(datos)) {
      datos_preparados$lon_fin <- Tivy::dms_a_decimal(datos$LongitudFin)
    }
  }

  # Añadir columnas para longitudes específicas por esquina (inicializadas como NA)
  datos_preparados$lon_ini_norte <- NA
  datos_preparados$lon_fin_norte <- NA
  datos_preparados$lon_ini_sur <- NA
  datos_preparados$lon_fin_sur <- NA

  # Manejar casos donde tenemos millas náuticas en lugar de longitudes
  filas_con_millas <- !is.na(datos$MillasNauticasInicio) & !is.na(datos$MillasNauticasFin) &
    (is.na(datos_preparados$lon_ini) | is.na(datos_preparados$lon_fin))

  if (any(filas_con_millas)) {
    for (i in which(filas_con_millas)) {
      # Obtener latitudes decimales
      lat_ini_dec <- datos_preparados$lat_ini[i]
      lat_fin_dec <- datos_preparados$lat_fin[i]

      # Calcular longitudes para cada esquina del polígono
      # Para latitud inicial (límite norte)
      lon_costa_lat_ini <- Tivy:::calcular_longitud_costa(costa, lat_ini_dec)
      # Para latitud final (límite sur)
      lon_costa_lat_fin <- Tivy:::calcular_longitud_costa(costa, lat_fin_dec)

      # Calcular offsets en grados basados en millas náuticas
      millas_ini <- datos$MillasNauticasInicio[i]
      millas_fin <- datos$MillasNauticasFin[i]

      # Factor de conversión ajustado por cada latitud
      factor_lat_ini <- cos(lat_ini_dec * pi/180)
      factor_lat_fin <- cos(lat_fin_dec * pi/180)

      # Convertir millas a grados para cada latitud
      offset_ini_lat_ini <- millas_ini / 60 / factor_lat_ini
      offset_fin_lat_ini <- millas_fin / 60 / factor_lat_ini
      offset_ini_lat_fin <- millas_ini / 60 / factor_lat_fin
      offset_fin_lat_fin <- millas_fin / 60 / factor_lat_fin

      # Almacenar las 4 longitudes específicas (una para cada esquina)
      datos_preparados$lon_ini_norte[i] <- lon_costa_lat_ini - offset_fin_lat_ini  # Esquina noroeste (más lejos)
      datos_preparados$lon_fin_norte[i] <- lon_costa_lat_ini - offset_ini_lat_ini  # Esquina noreste (más cerca)
      datos_preparados$lon_ini_sur[i] <- lon_costa_lat_fin - offset_fin_lat_fin    # Esquina suroeste (más lejos)
      datos_preparados$lon_fin_sur[i] <- lon_costa_lat_fin - offset_ini_lat_fin    # Esquina sureste (más cerca)

      # Marcar las longitudes originales como NA para indicar que usamos longitudes específicas
      datos_preparados$lon_ini[i] <- NA
      datos_preparados$lon_fin[i] <- NA
    }
  }

  # Crear lista para almacenar polígonos
  poligonos <- list()

  # Crear un polígono para cada fila
  for (i in 1:nrow(datos_preparados)) {
    # Verificar si tenemos longitudes específicas para esquinas (caso millas náuticas)
    if (!is.na(datos_preparados$lat_ini[i]) && !is.na(datos_preparados$lat_fin[i]) &&
        is.na(datos_preparados$lon_ini[i]) && !is.na(datos_preparados$lon_ini_norte[i])) {

      # Crear coordenadas del polígono con longitudes específicas para cada esquina
      coords <- rbind(
        c(datos_preparados$lon_ini_norte[i], datos_preparados$lat_ini[i]),  # Esquina noroeste
        c(datos_preparados$lon_fin_norte[i], datos_preparados$lat_ini[i]),  # Esquina noreste
        c(datos_preparados$lon_fin_sur[i], datos_preparados$lat_fin[i]),    # Esquina sureste
        c(datos_preparados$lon_ini_sur[i], datos_preparados$lat_fin[i]),    # Esquina suroeste
        c(datos_preparados$lon_ini_norte[i], datos_preparados$lat_ini[i])   # Cerrar el polígono
      )

    } else if (!is.na(datos_preparados$lat_ini[i]) && !is.na(datos_preparados$lat_fin[i]) &&
               !is.na(datos_preparados$lon_ini[i]) && !is.na(datos_preparados$lon_fin[i])) {

      # Caso normal: crear polígono rectangular con las mismas longitudes por lado
      coords <- rbind(
        c(datos_preparados$lon_ini[i], datos_preparados$lat_ini[i]),  # Esquina noroeste
        c(datos_preparados$lon_fin[i], datos_preparados$lat_ini[i]),  # Esquina noreste
        c(datos_preparados$lon_fin[i], datos_preparados$lat_fin[i]),  # Esquina sureste
        c(datos_preparados$lon_ini[i], datos_preparados$lat_fin[i]),  # Esquina suroeste
        c(datos_preparados$lon_ini[i], datos_preparados$lat_ini[i])   # Cerrar el polígono
      )

    } else {
      warning("Fila ", i, " contiene valores NA después del procesamiento. Se omitirá.")
      next
    }

    # Crear objeto polígono
    poligono <- list(
      coords = coords,
      id = i,
      fecha_inicio = if ("FechaHoraInicio" %in% names(datos)) datos$FechaHoraInicio[i] else NA,
      fecha_fin = if ("FechaHoraFin" %in% names(datos)) datos$FechaHoraFin[i] else NA,
      nombre_archivo = if ("nombre_archivo" %in% names(datos)) datos$nombre_archivo[i] else NA,
      Long_Ini = if ("LongitudInicio" %in% names(datos)) datos$LongitudInicio[i] else NA,
      Lat_Ini = if ("LatitudInicio" %in% names(datos)) datos$LatitudInicio[i] else NA,
      Long_Fin = if ("LongitudFin" %in% names(datos)) datos$LongitudFin[i] else NA,
      Lat_Fin = if ("LatitudFin" %in% names(datos)) datos$LatitudFin[i] else NA,
      MillasNauticasInicio = if ("MillasNauticasInicio" %in% names(datos)) datos$MillasNauticasInicio[i] else NA,
      MillasNauticasFin = if ("MillasNauticasFin" %in% names(datos)) datos$MillasNauticasFin[i] else NA,
      comunicado = if ("comunicado" %in% names(datos)) datos$comunicado[i] else paste("Polígono", i)
    )

    poligonos[[i]] <- poligono
  }

  # Filtrar polígonos NA
  poligonos <- poligonos[!sapply(poligonos, is.null)]

  if (length(poligonos) == 0) {
    stop("No se pudieron crear polígonos válidos con los datos proporcionados.")
  }

  return(poligonos)
}

