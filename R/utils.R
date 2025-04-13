#' Calcular distancia usando la fórmula de Haversine con el elipsoide WGS84
#'
#' @param lon1 Longitud del punto de origen (en grados o radianes).
#' @param lat1 Latitud del punto de origen (en grados o radianes).
#' @param lon2 Vector de longitudes destino (en grados o radianes).
#' @param lat2 Vector de latitudes destino (en grados o radianes).
#' @param unidad Unidad de salida: "mn" para millas náuticas, "km" para kilómetros.
#'
#' @return Lista con la distancia mínima y el índice correspondiente.
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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



#' Encuentra la línea paralela más cercana a una distancia en millas náuticas
#'
#' @description
#' Esta función busca dentro de una lista de líneas paralelas a la costa la que mejor
#' se aproxima a una distancia específica en millas náuticas.
#'
#' @param millas Valor numérico que indica la distancia en millas náuticas que se desea encontrar.
#' @param paralelas_costa Lista de data frames con líneas paralelas a diferentes distancias.
#'        Cada elemento debe tener un nombre que indique la distancia (ej. "l5", "l10").
#'
#' @return Una lista con dos elementos:
#' \itemize{
#'   \item df: El data frame con la línea paralela más cercana a la distancia solicitada.
#'   \item milla_real: El valor numérico de la milla realmente encontrada.
#' }
#'
#' @examples
#' # Asumiendo que paralelas_costa_peru es una lista de líneas a distintas distancias
#' resultado <- encontrar_linea_paralela(7, paralelas_costa_peru)
#' linea <- resultado$df
#' milla_encontrada <- resultado$milla_real
#'
#' @keywords internal
encontrar_linea_paralela <- function(millas, paralelas_costa) {
  # Extraer los números de millas de los nombres de las listas
  millas_disponibles <- as.numeric(gsub("l", "", names(paralelas_costa)))

  # Encontrar la milla más cercana
  idx_cercano <- which.min(abs(millas_disponibles - millas))
  milla_cercana <- millas_disponibles[idx_cercano]

  # Devolver el dataframe correspondiente
  return(list(
    df = paralelas_costa[[paste0("l", milla_cercana)]],
    milla_real = milla_cercana
  ))
}


#' Interpola un punto para una latitud específica en una línea paralela
#'
#' @description
#' Esta función calcula la longitud correspondiente a una latitud específica en una línea
#' paralela a la costa, interpolando entre los puntos existentes cuando sea necesario.
#'
#' @param linea Data frame que representa una línea paralela a la costa.
#' @param latitud Valor numérico de la latitud para la cual se desea encontrar la longitud.
#' @param nombre_lat Nombre de la columna que contiene las latitudes en el data frame.
#' @param nombre_lon Nombre de la columna que contiene las longitudes en el data frame.
#'
#' @return Un vector con dos elementos:
#' \itemize{
#'   \item lon: Longitud interpolada para la latitud dada.
#'   \item lat: La misma latitud proporcionada como entrada.
#' }
#'
#' @details
#' La función ordena la línea por latitud, busca los puntos que encierran la latitud
#' deseada y realiza una interpolación lineal. En caso de no encontrar puntos adecuados
#' para interpolar, devuelve el punto más cercano.
#'
#' @examples
#' # Asumiendo que linea_5_millas es un data frame con columnas 'lat' y 'lon'
#' punto <- interpolar_punto(linea_5_millas, -12.5, "lat", "lon")
#' longitud <- punto["lon"]
#'
#' @keywords internal
interpolar_punto <- function(linea, latitud, nombre_lat, nombre_lon) {
  # Ordenar por latitud para asegurar interpolación correcta
  linea <- linea[order(linea[[nombre_lat]]), ]

  # Encontrar los puntos que encierran la latitud deseada
  idx_inf <- max(which(linea[[nombre_lat]] <= latitud), na.rm = TRUE)
  idx_sup <- min(which(linea[[nombre_lat]] >= latitud), na.rm = TRUE)

  # Manejar casos especiales
  if (length(idx_inf) == 0 || is.infinite(idx_inf)) {
    # Si no hay puntos por debajo, usar el punto más cercano
    idx_cercano <- which.min(abs(linea[[nombre_lat]] - latitud))
    return(c(
      lon = linea[[nombre_lon]][idx_cercano],
      lat = linea[[nombre_lat]][idx_cercano]
    ))
  }

  if (length(idx_sup) == 0 || is.infinite(idx_sup)) {
    # Si no hay puntos por encima, usar el punto más cercano
    idx_cercano <- which.min(abs(linea[[nombre_lat]] - latitud))
    return(c(
      lon = linea[[nombre_lon]][idx_cercano],
      lat = linea[[nombre_lat]][idx_cercano]
    ))
  }

  # Si la latitud coincide exactamente con un punto, usarlo directamente
  if (idx_inf == idx_sup) {
    return(c(
      lon = linea[[nombre_lon]][idx_inf],
      lat = linea[[nombre_lat]][idx_inf]
    ))
  }

  # Interpolar entre los dos puntos
  lat_inf <- linea[[nombre_lat]][idx_inf]
  lat_sup <- linea[[nombre_lat]][idx_sup]
  lon_inf <- linea[[nombre_lon]][idx_inf]
  lon_sup <- linea[[nombre_lon]][idx_sup]

  # Calcular proporción para interpolación
  prop <- (latitud - lat_inf) / (lat_sup - lat_inf)
  longitud <- lon_inf + prop * (lon_sup - lon_inf)

  return(c(
    lon = longitud,
    lat = latitud
  ))
}



#' Extrae todos los puntos de una línea paralela entre dos latitudes
#'
#' @description
#' Esta función obtiene todos los puntos de una línea paralela a la costa que se encuentran
#' entre dos latitudes dadas, incluyendo puntos interpolados en los límites exactos.
#'
#' @param linea Data frame que representa una línea paralela a la costa.
#' @param lat_min Valor numérico de la latitud mínima (más al sur).
#' @param lat_max Valor numérico de la latitud máxima (más al norte).
#' @param nombre_lat Nombre de la columna que contiene las latitudes en el data frame.
#' @param nombre_lon Nombre de la columna que contiene las longitudes en el data frame.
#'
#' @return Una matriz de puntos con columnas correspondientes a longitud y latitud.
#'
#' @details
#' La función primero filtra los puntos que están dentro del rango de latitudes.
#' Si es necesario, interpola puntos adicionales para los límites exactos de
#' lat_min y lat_max. Si no hay suficientes puntos en el rango, crea un segmento
#' recto entre los puntos interpolados en los límites.
#'
#' @examples
#' # Asumiendo que linea_10_millas es un data frame con columnas 'lat' y 'lon'
#' puntos <- extraer_puntos_entre_latitudes(linea_10_millas, -14.5, -12.0, "lat", "lon")
#'
#' @keywords internal
extraer_puntos_entre_latitudes <- function(linea, lat_min, lat_max, nombre_lat, nombre_lon) {
  # Ordenar por latitud
  linea <- linea[order(linea[[nombre_lat]]), ]

  # Filtrar puntos dentro del rango de latitudes
  puntos_filtrados <- linea[linea[[nombre_lat]] >= lat_min & linea[[nombre_lat]] <= lat_max, ]

  # Si no hay suficientes puntos dentro del rango, tendremos que interpolar
  if (nrow(puntos_filtrados) < 2) {
    # Interpolar puntos en los límites
    punto_min <- Tivy:::interpolar_punto(linea, lat_min, nombre_lat, nombre_lon)
    punto_max <- Tivy:::interpolar_punto(linea, lat_max, nombre_lat, nombre_lon)

    # Crear matriz de puntos
    puntos <- rbind(
      punto_min,
      punto_max
    )
    colnames(puntos) <- c(nombre_lon, nombre_lat)

    return(puntos)
  }

  # Verificar si necesitamos añadir puntos interpolados en los límites exactos
  primer_punto <- puntos_filtrados[1, ]
  ultimo_punto <- puntos_filtrados[nrow(puntos_filtrados), ]

  puntos_limite <- NULL

  # Si el primer punto no está exactamente en lat_min, interpolar
  if (abs(primer_punto[[nombre_lat]] - lat_min) > 0.0001) {
    punto_min <- Tivy:::interpolar_punto(linea, lat_min, nombre_lat, nombre_lon)
    puntos_limite <- rbind(puntos_limite, punto_min)
  }

  # Puntos del medio (los filtrados)
  puntos_medio <- as.matrix(puntos_filtrados[, c(nombre_lon, nombre_lat)])

  # Si el último punto no está exactamente en lat_max, interpolar
  if (abs(ultimo_punto[[nombre_lat]] - lat_max) > 0.0001) {
    punto_max <- Tivy:::interpolar_punto(linea, lat_max, nombre_lat, nombre_lon)
    puntos_limite <- rbind(puntos_limite, punto_max)
  }

  # Combinar todos los puntos
  if (!is.null(puntos_limite)) {
    colnames(puntos_limite) <- c(nombre_lon, nombre_lat)

    # Determinar qué puntos van antes y cuáles después
    puntos_antes <- puntos_limite[puntos_limite[, 2] < primer_punto[[nombre_lat]], , drop = FALSE]
    puntos_despues <- puntos_limite[puntos_limite[, 2] > ultimo_punto[[nombre_lat]], , drop = FALSE]

    puntos <- rbind(
      puntos_antes,
      puntos_medio,
      puntos_despues
    )
  } else {
    puntos <- puntos_medio
  }

  return(puntos)
}


#' Preparar polígonos a partir de datos de coordenadas
#'
#' @description
#' Función auxiliar para procesar datos y preparar polígonos usando líneas paralelas
#' preexistentes. Los bordes este y oeste del polígono seguirán la forma de las líneas
#' paralelas a la costa a las millas náuticas especificadas, mientras que los bordes
#' norte y sur serán líneas rectas a las latitudes dadas.
#'
#' @param datos Un data frame con coordenadas.
#' @param costa Un data frame con la línea de costa.
#' @param paralelas_costa Lista de data frames con líneas paralelas a diferentes distancias de la costa.
#'        Cada elemento de la lista debe tener un nombre que indique la distancia (ej. "l5", "l10").
#' @param nombres_columnas Un vector con nombres de columnas en los data frames de paralelas_costa.
#'        Debe contener: c("lat", "lon", "dc") donde "dc" es la distancia a la costa.
#'
#' @return Una lista de polígonos para visualización.
#' @keywords internal
preparar_poligonos <- function(datos, costa, paralelas_costa = NULL, nombres_columnas = c("lat", "lon", "dc")) {
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

  # Validar paralelas_costa si se proporcionó
  if (!is.null(paralelas_costa)) {
    if (!is.list(paralelas_costa)) {
      stop("'paralelas_costa' debe ser una lista de data.frames.")
    }

    # Validar que cada elemento de la lista es un data.frame con las columnas necesarias
    for (nombre in names(paralelas_costa)) {
      df <- paralelas_costa[[nombre]]
      if (!is.data.frame(df)) {
        warning("El elemento '", nombre, "' en paralelas_costa no es un data.frame.")
        next
      }
      if (!all(nombres_columnas %in% names(df))) {
        warning("El elemento '", nombre, "' en paralelas_costa no tiene todas las columnas requeridas: ",
                paste(nombres_columnas, collapse = ", "))
      }
    }

    # Verificar que tenemos al menos un elemento válido
    elementos_validos <- sapply(names(paralelas_costa), function(nombre) {
      df <- paralelas_costa[[nombre]]
      is.data.frame(df) && all(nombres_columnas %in% names(df))
    })

    if (!any(elementos_validos)) {
      warning("Ningún elemento en paralelas_costa tiene el formato correcto. Se usará el método de aproximación original.")
      paralelas_costa <- NULL
    }
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

  # Crear lista para almacenar polígonos
  poligonos <- list()

  # Procesar cada fila para crear polígonos
  for (i in 1:nrow(datos_preparados)) {
    # Verificar si tenemos millas náuticas en lugar de longitudes
    if (!is.na(datos$MillasNauticasInicio[i]) && !is.na(datos$MillasNauticasFin[i]) &&
        (is.na(datos_preparados$lon_ini[i]) || is.na(datos_preparados$lon_fin[i]))) {

      # Obtener latitudes decimales y asegurarse de que están en orden correcto
      lat_norte <- max(datos_preparados$lat_ini[i], datos_preparados$lat_fin[i])
      lat_sur <- min(datos_preparados$lat_ini[i], datos_preparados$lat_fin[i])

      # Obtener millas
      millas_ini <- datos$MillasNauticasInicio[i]  # Más cerca de la costa
      millas_fin <- datos$MillasNauticasFin[i]     # Más lejos de la costa

      # Considerar usar paralelas_costa preexistentes
      if (!is.null(paralelas_costa)) {
        nombre_lat <- nombres_columnas[1] # Columna de latitud
        nombre_lon <- nombres_columnas[2] # Columna de longitud

        # Encontrar líneas paralelas para las millas inicial y final
        linea_ini_info <- Tivy:::encontrar_linea_paralela(millas_ini, paralelas_costa)
        linea_fin_info <- Tivy:::encontrar_linea_paralela(millas_fin, paralelas_costa)

        linea_ini <- linea_ini_info$df  # Línea más cercana a la costa
        linea_fin <- linea_fin_info$df  # Línea más alejada de la costa

        # Si encontramos líneas para ambas millas
        if (!is.null(linea_ini) && !is.null(linea_fin) &&
            nrow(linea_ini) > 0 && nrow(linea_fin) > 0) {

          # Interpolar puntos exactos en las esquinas
          punto_ne <- Tivy:::interpolar_punto(linea_ini, lat_norte, nombre_lat, nombre_lon)
          punto_se <- Tivy:::interpolar_punto(linea_ini, lat_sur, nombre_lat, nombre_lon)
          punto_no <- Tivy:::interpolar_punto(linea_fin, lat_norte, nombre_lat, nombre_lon)
          punto_so <- Tivy:::interpolar_punto(linea_fin, lat_sur, nombre_lat, nombre_lon)

          # Extraer puntos de la línea oeste (más alejada) entre las latitudes
          puntos_oeste <- Tivy:::extraer_puntos_entre_latitudes(
            linea_fin, lat_sur, lat_norte, nombre_lat, nombre_lon
          )

          # Extraer puntos de la línea este (más cercana) entre las latitudes
          puntos_este <- Tivy:::extraer_puntos_entre_latitudes(
            linea_ini, lat_sur, lat_norte, nombre_lat, nombre_lon
          )

          # Crear coordenadas para el polígono completo
          # 1. Comenzamos en la esquina noreste y avanzamos hacia el norte por el borde este
          # 2. Luego vamos de norte a sur por el borde oeste
          # 3. Volvemos al punto inicial para cerrar el polígono

          # Ordenar los puntos oeste de norte a sur
          puntos_oeste <- puntos_oeste[order(puntos_oeste[, 2], decreasing = TRUE), ]

          # Ordenar los puntos este de sur a norte
          puntos_este <- puntos_este[order(puntos_este[, 2]), ]

          # Combinar todos los puntos para formar el polígono
          coords <- rbind(
            puntos_este,          # Borde este (de sur a norte)
            puntos_oeste,         # Borde oeste (de norte a sur)
            puntos_este[1, ]      # Cerrar el polígono volviendo al primer punto
          )

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
            MillasNauticasInicio = millas_ini,
            MillasNauticasFin = millas_fin,
            comunicado = if ("comunicado" %in% names(datos)) datos$comunicado[i] else paste("Polígono", i),
            milla_ini_real = linea_ini_info$milla_real,
            milla_fin_real = linea_fin_info$milla_real,
            metodo = "detallado"
          )

          poligonos[[i]] <- poligono
          next  # Continuar con la siguiente iteración
        }
      }

      # Si llegamos aquí, es porque no pudimos crear un polígono detallado con líneas paralelas
      # Caemos de nuevo al método original con aproximación
      lon_costa_lat_norte <- Tivy:::calcular_longitud_costa(costa, lat_norte)
      lon_costa_lat_sur <- Tivy:::calcular_longitud_costa(costa, lat_sur)

      # Factor de conversión ajustado por cada latitud
      factor_lat_norte <- cos(lat_norte * pi/180)
      factor_lat_sur <- cos(lat_sur * pi/180)

      # Convertir millas a grados para cada latitud
      offset_ini_lat_norte <- millas_ini / 60 / factor_lat_norte
      offset_fin_lat_norte <- millas_fin / 60 / factor_lat_norte
      offset_ini_lat_sur <- millas_ini / 60 / factor_lat_sur
      offset_fin_lat_sur <- millas_fin / 60 / factor_lat_sur

      # Crear coordenadas del polígono con 4 esquinas (método original)
      coords <- rbind(
        c(lon_costa_lat_norte - offset_ini_lat_norte, lat_norte),  # Esquina noreste
        c(lon_costa_lat_norte - offset_fin_lat_norte, lat_norte),  # Esquina noroeste
        c(lon_costa_lat_sur - offset_fin_lat_sur, lat_sur),        # Esquina suroeste
        c(lon_costa_lat_sur - offset_ini_lat_sur, lat_sur),        # Esquina sureste
        c(lon_costa_lat_norte - offset_ini_lat_norte, lat_norte)   # Cerrar el polígono
      )

      # Crear objeto polígono (método original)
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
        MillasNauticasInicio = millas_ini,
        MillasNauticasFin = millas_fin,
        comunicado = if ("comunicado" %in% names(datos)) datos$comunicado[i] else paste("Polígono", i),
        metodo = "aproximado"
      )

      poligonos[[i]] <- poligono

    } else if (!is.na(datos_preparados$lat_ini[i]) && !is.na(datos_preparados$lat_fin[i]) &&
               !is.na(datos_preparados$lon_ini[i]) && !is.na(datos_preparados$lon_fin[i])) {

      # Caso con coordenadas explícitas: crear polígono rectangular
      coords <- rbind(
        c(datos_preparados$lon_ini[i], datos_preparados$lat_ini[i]),  # Esquina noroeste
        c(datos_preparados$lon_fin[i], datos_preparados$lat_ini[i]),  # Esquina noreste
        c(datos_preparados$lon_fin[i], datos_preparados$lat_fin[i]),  # Esquina sureste
        c(datos_preparados$lon_ini[i], datos_preparados$lat_fin[i]),  # Esquina suroeste
        c(datos_preparados$lon_ini[i], datos_preparados$lat_ini[i])   # Cerrar el polígono
      )

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
        comunicado = if ("comunicado" %in% names(datos)) datos$comunicado[i] else paste("Polígono", i),
        metodo = "explicito"
      )

      poligonos[[i]] <- poligono
    } else {
      warning("Fila ", i, " no tiene suficientes datos para crear un polígono válido.")
    }
  }

  # Filtrar polígonos NA
  poligonos <- poligonos[!sapply(poligonos, is.null)]

  if (length(poligonos) == 0) {
    stop("No se pudieron crear polígonos válidos con los datos proporcionados.")
  }

  return(poligonos)
}
