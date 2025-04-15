

#' Graficar polígonos de zonas de suspensión pesquera con ggplot2
#'
#' @description
#' Crea visualizaciones estáticas de polígonos para zonas de suspensión pesquera
#' utilizando ggplot2. Devuelve un objeto ggplot2 que puede ser modificado posteriormente.
#'
#' @param datos Un data frame con coordenadas de latitud y longitud de inicio y fin,
#'        que pueden estar en formato de texto (como "12°30'S") o decimales.
#' @param costa Un data frame que contiene la línea de costa para visualización.
#'        Debe tener columnas 'Long' y 'Lat'. Default `Tivy::linea_costa_peru`.
#' @param paralelas Un data frame que contiene las líneas paralelas a la costa. Default `Tivy::paralelas_costa_peru`.
#' @param titulo Título para el gráfico. Default NULL
#' @param colores Vector de colores para los polígonos. Default NULL
#' @param mostrar_leyenda Lógico. Si es TRUE, muestra la leyenda. Default FALSE.
#' @param etiquetas Vector de textos para etiquetar cada polígono en la leyenda.
#' @param agregar_grid Lógico. Si es TRUE, agrega cuadrícula al gráfico. Default FALSE.
#'
#' @return Un objeto ggplot2 que puede ser modificado con capas adicionales.
#' @import ggplot2
#'
#' @examples
#' # Crear gráfico básico
#' resultados <- extrae_data_comunicados(c("comunicado1.pdf"))
#' g <- graficar_poligonos_ggplot(datos = resultados, costa = Tivy::linea_costa_peru)
#'
#' # Personalizar posteriormente
#' g +
#'   ggplot2::xlim(-80, -70) +
#'   ggplot2::theme_minimal() +
#'   ggplot2::labs(title = "Título personalizado")
#'
#' @export
graficar_poligonos_ggplot <- function(datos,
                                      costa = Tivy::linea_costa_peru,
                                      paralelas = Tivy::paralelas_costa_peru,
                                      titulo = NULL,
                                      colores = NULL,
                                      mostrar_leyenda = FALSE,
                                      etiquetas = NULL,
                                      agregar_grid = FALSE) {

  # Preparar los polígonos (todo el procesamiento de datos es el mismo)
  poligonos <- preparar_poligonos(datos, costa, paralelas,)

  # Crear visualización estática con ggplot2
  return(Tivy:::graficar_estatico(
    poligonos,
    costa = costa,
    titulo = titulo,
    colores = colores,
    mostrar_leyenda = mostrar_leyenda,
    etiquetas = etiquetas,
    agregar_grid = agregar_grid
  ))
}

#' Graficar polígonos de zonas de suspensión pesquera con leaflet
#'
#' @description
#' Crea visualizaciones interactivas de polígonos para zonas de suspensión pesquera
#' utilizando leaflet. Devuelve un objeto leaflet que puede ser modificado posteriormente.
#'
#' @param datos Un data frame con coordenadas de latitud y longitud de inicio y fin,
#'        que pueden estar en formato de texto (como "12°30'S") o decimales.
#' @param costa Un data frame que contiene la línea de costa para visualización.
#'        Debe tener columnas 'Long' y 'Lat'. Default `Tivy::linea_costa_peru`.
#' @param paralelas Un data frame que contiene las líneas paralelas a la costa. Default `Tivy::paralelas_costa_peru`.
#' @param titulo Título para el gráfico. Default NULL
#' @param colores Vector de colores para los polígonos. Default NULL
#' @param mostrar_leyenda Lógico. Si es TRUE, muestra la leyenda. Default FALSE.
#' @param etiquetas Vector de textos para etiquetar cada polígono en la leyenda.
#' @param capas_base Lógico. Si es TRUE, agrega múltiples mapas base. Default FALSE.
#' @param minimap Lógico. Si es TRUE, agrega un minimapa al mapa interactivo. Default FALSE
#'
#' @return Un objeto leaflet que puede ser modificado con capas adicionales.
#' @import leaflet
#'
#' @examples
#' # Crear mapa básico
#' resultados <- extrae_data_comunicados(c("comunicado1.pdf"))
#' m <- graficar_poligonos_leaflet(datos = resultados, costa = Tivy::linea_costa_peru)
#'
#' # Personalizar posteriormente
#' m %>%
#'   leaflet::addMarkers(lng = -77.1, lat = -12.0, popup = "Lima") %>%
#'   leaflet::addCircleMarkers(lng = -76.3, lat = -13.4, radius = 5, color = "red")
#'
#' @export
graficar_poligonos_leaflet <- function(datos,
                                       costa = Tivy::linea_costa_peru,
                                       paralelas = Tivy::paralelas_costa_peru,
                                       titulo = NULL,
                                       colores = NULL,
                                       mostrar_leyenda = FALSE,
                                       etiquetas = NULL,
                                       capas_base = FALSE,
                                       minimap = FALSE) {

  # Preparar los polígonos (todo el procesamiento de datos es el mismo)
  poligonos <- preparar_poligonos(datos, costa, paralelas)

  # Crear visualización interactiva con leaflet
  return(Tivy:::graficar_interactivo(
    poligonos, costa, titulo, colores,
    mostrar_leyenda = mostrar_leyenda,
    etiquetas = etiquetas,
    capas_base = capas_base,
    minimap = minimap
  ))
}


#' Graficar polígonos de suspensión pesquera
#'
#' Esta función permite graficar polígonos de suspensión pesquera utilizando
#' un mapa base de la costa peruana. Dependiendo del parámetro `tipo`, puede
#' generar un gráfico estático con `ggplot2` o un gráfico interactivo con `leaflet`.
#'
#' @param datos Lista de polígonos con coordenadas y metadatos (como comunicado).
#' @param costa Data frame con la línea de costa a graficar. Default `Tivy::linea_costa_peru`.
#' @param paralelas Un data frame que contiene las líneas paralelas a la costa. Default `Tivy::paralelas_costa_peru`.
#' @param tipo Tipo de gráfico a generar: `"estatico"` para ggplot2 o `"interactivo"` para leaflet.
#' @param titulo Título del gráfico.
#' @param colores Vector de colores a usar para diferenciar los polígonos (por comunicado u otra categoría).
#' @param mostrar_leyenda Lógico. Si `TRUE`, se muestra la leyenda. Por defecto es `FALSE`.
#' @param etiquetas Vector opcional de etiquetas personalizadas para cada polígono. Si no se especifica, se usa el nombre del comunicado.
#' @param agregar_grid Lógico. Si `TRUE`, agrega una cuadrícula de referencia al gráfico estático.
#' @param capas_base Lógico. Si `TRUE`, agrega capas base (como satélite o topografía) en el mapa interactivo.
#' @param minimap Lógico. Si `TRUE`, agrega un minimapa en la esquina del mapa interactivo.
#'
#' @return Un objeto de clase `ggplot` si `tipo = "estatico"` o un objeto `leaflet` si `tipo = "interactivo"`.
#'
#' @examples
#' # Crear mapa básico
#' resultados <- extrae_data_comunicados(c("comunicado1.pdf", "comunicado2.pdf"))
#' graficar_poligonos(datos = resultados, costa = Tivy::linea_costa_peru)
#'
#' @export
graficar_poligonos <- function(datos,
                               costa = Tivy::linea_costa_peru,
                               paralelas = Tivy::paralelas_costa_peru,
                               tipo = "estatico",
                               titulo = "Zonas de suspensión pesquera",
                               colores = NULL,
                               mostrar_leyenda = FALSE,
                               etiquetas = NULL,
                               agregar_grid = FALSE,
                               capas_base = FALSE,
                               minimap = FALSE) {

  if (tipo == "estatico") {
    return(graficar_poligonos_ggplot(
      datos = datos,
      costa = costa,
      paralelas = paralelas,
      titulo = titulo,
      colores = colores,
      mostrar_leyenda = mostrar_leyenda,
      etiquetas = etiquetas,
      agregar_grid = agregar_grid
    ))
  } else {
    return(graficar_poligonos_leaflet(
      datos = datos,
      costa = costa,
      paralelas = paralelas,
      titulo = titulo,
      colores = colores,
      mostrar_leyenda = mostrar_leyenda,
      etiquetas = etiquetas,
      capas_base = capas_base,
      minimap = minimap
    ))
  }
}



#' Graficar porcentajes de juveniles (en número y peso)
#'
#' Genera visualizaciones para explorar los porcentajes de juveniles en función
#' de una variable seleccionada (por ejemplo, fecha o embarcación). Los datos deben
#' provenir de la función `juveniles_por_grupo`, que resume la proporción de juveniles
#' por grupo según talla y captura.
#'
#' La función permite elegir entre diferentes tipos de gráficos (barras, líneas, puntos,
#' mixtos o boxplots) y personalizar opciones como colores, ordenamiento y facetado.
#' También es posible agregar una línea de referencia correspondiente al límite legal
#' de juveniles.
#'
#' @param datos_juveniles Data frame con resultados de `juveniles_por_grupo`.
#' @param var_x Nombre de la variable a utilizar en el eje X del gráfico (por ejemplo, "fecha_unica").
#' @param fill_var (Opcional) Variable para agrupar y colorear las barras o líneas (por ejemplo, "embarcacion").
#' @param tipo_grafico Tipo de gráfico a mostrar: "barras" (por defecto), "lineas", "puntos", "mixto", o "boxplot".
#' @param titulo (Opcional) Título principal del gráfico.
#' @param limite_juv (Opcional) Valor umbral que representa el límite legal de porcentaje de juveniles; se añade como línea horizontal.
#' @param ordenar_por Criterio para ordenar el eje X: "x" (por defecto, orden según var_x), "numero", "peso" o "total".
#' @param paleta (Opcional) Vector de colores personalizados.
#' @param facet_var (Opcional) Variable para dividir el gráfico en subgráficos (facets).
#' @param ncol Número de columnas para el facetado (por defecto 2).
#'
#' @return Un objeto `ggplot` que representa el gráfico generado. Puede personalizarse o imprimirse directamente.
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_bar geom_line geom_point geom_boxplot
#'             facet_wrap scale_fill_manual scale_color_manual labs theme_minimal
#' @importFrom dplyr arrange mutate
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' # Procesamiento de datos
#' data_calas <- procesar_calas(data_calas = calas_bitacora)
#' data_faenas <- procesar_faenas(data_faenas = faenas_bitacora)
#' calas_tallas <- procesar_tallas(data_tallas = tallas_bitacora)
#' data_tallasfaenas <- merge(x = data_faenas, y = calas_tallas, by = "codigo_faena")
#' data_total <- merge_tallas_faenas_calas(data_calas = data_calas, data_tallas_faenas = data_tallasfaenas)
#'
#' datos_final <- agregar_variables(data_total)
#' tallas_cols <- c("8", "8.5", "9", "9.5", "10", "10.5", "11", "11.5", "12",
#'                  "12.5", "13", "13.5", "14", "14.5", "15")
#' resultado <- ponderar_tallas_df(df = datos_final, tallas_cols = tallas_cols,
#'                                 captura_col = "catch_ANCHOVETA", a = 0.0001, b = 2.984)
#' resultado$fecha_unica <- convertir_a_fecha(resultado$fecha_inicio_cala, tipo = "date")
#' resultado_juveniles <- juveniles_por_grupo(data = resultado,
#'                                            group_cols = c("fecha_unica"),
#'                                            cols_tallas = tallas_cols)
#'
#' # Gráfico de barras de porcentajes de juveniles por fecha
#' graficar_juveniles(
#'   datos_juveniles = resultado_juveniles,
#'   var_x = "fecha_unica",
#'   limite_juv = 12
#' )
graficar_juveniles <- function(datos_juveniles, var_x, fill_var = NULL,
                               tipo_grafico = "barras", titulo = NULL,
                               limite_juv = NULL, ordenar_por = "x",
                               paleta = NULL, facet_var = NULL, ncol = 2) {

  # Validación de parámetros
  if (!is.data.frame(datos_juveniles))
    stop("datos_juveniles debe ser un data.frame")

  if (!var_x %in% colnames(datos_juveniles))
    stop(paste("La variable", var_x, "no existe en el data.frame"))

  if (!is.null(fill_var) && !fill_var %in% colnames(datos_juveniles))
    stop(paste("La variable", fill_var, "no existe en el data.frame"))

  if (!is.null(facet_var) && !facet_var %in% colnames(datos_juveniles))
    stop(paste("La variable", facet_var, "no existe en el data.frame"))

  if (!tipo_grafico %in% c("barras", "lineas", "puntos", "mixto"))
    stop("tipo_grafico debe ser uno de: 'barras', 'lineas', 'puntos' o 'mixto'")

  if (!ordenar_por %in% c("x", "numero", "peso"))
    stop("ordenar_por debe ser uno de: 'x', 'numero', 'peso'")

  # Preparación de datos
  # Convertir a formato largo para facilitar la visualización
  datos_long <- datos_juveniles %>%
    tidyr::pivot_longer(
      cols = c("porc_juv_numero", "porc_juv_peso"),
      names_to = "tipo",
      values_to = "porcentaje"
    ) %>%
    dplyr::mutate(
      tipo = factor(
        tipo,
        levels = c("porc_juv_numero", "porc_juv_peso"),
        labels = c("En número", "En peso")
      )
    )

  # Verificar si la variable X es de tipo fecha y convertirla si es necesario
  if (is.character(datos_long[[var_x]]) &&
      (grepl("fecha", var_x, ignore.case = TRUE) ||
       grepl("date", var_x, ignore.case = TRUE))) {
    # Intentar convertir a Date solo si parece una fecha en formato de texto
    if (any(grepl("-|/", datos_long[[var_x]]))) {
      # Intentar varios formatos comunes de fecha
      fecha_convertida <- convertir_a_fecha(datos_long[[var_x]], tipo = "date")
      if (!is.null(fecha_convertida)) {
        datos_long[[var_x]] <- fecha_convertida
      }
    }
  }

  # Ordenar datos según el parámetro ordenar_por
  if (ordenar_por == "numero") {
    orden <- datos_juveniles %>%
      dplyr::arrange(dplyr::desc(porc_juv_numero)) %>%
      dplyr::pull(var_x)
    datos_long[[var_x]] <- factor(datos_long[[var_x]], levels = unique(orden))
  } else if (ordenar_por == "peso") {
    orden <- datos_juveniles %>%
      dplyr::arrange(dplyr::desc(porc_juv_peso)) %>%
      dplyr::pull(var_x)
    datos_long[[var_x]] <- factor(datos_long[[var_x]], levels = unique(orden))
  }

  # Crear base del gráfico
  if (is.null(fill_var)) {
    p <- ggplot2::ggplot(datos_long, ggplot2::aes(x = .data[[var_x]], y = porcentaje, fill = tipo))
  } else {
    p <- ggplot2::ggplot(datos_long, ggplot2::aes(x = .data[[var_x]], y = porcentaje,
                                                  fill = .data[[fill_var]], color = .data[[fill_var]]))
  }

  # Añadir geoms según el tipo de gráfico
  if (tipo_grafico == "barras") {
    if (is.null(fill_var)) {
      p <- p + ggplot2::geom_bar(stat = "identity", position = "dodge")
    } else {
      p <- p + ggplot2::geom_bar(stat = "identity", position = "dodge") +
        ggplot2::facet_wrap(~ tipo, ncol = ncol)
    }
  } else if (tipo_grafico == "lineas") {
    p <- p + ggplot2::geom_line(ggplot2::aes(group = .data[[if (is.null(fill_var)) "tipo" else fill_var]]),
                                size = 1) +
      ggplot2::geom_point(size = 2)

    if (is.null(fill_var)) {
      p <- p + ggplot2::facet_wrap(~ tipo, ncol = ncol)
    }
  } else if (tipo_grafico == "puntos") {
    p <- p + ggplot2::geom_point(size = 3, alpha = 0.7)

    if (is.null(fill_var)) {
      p <- p + ggplot2::facet_wrap(~ tipo, ncol = ncol)
    }
  } else if (tipo_grafico == "mixto") {
    p <- p + ggplot2::geom_bar(stat = "identity", alpha = 0.5, position = "dodge") +
      ggplot2::geom_point(size = 2, position = ggplot2::position_dodge(width = 0.9))

    if (is.null(fill_var)) {
      p <- p + ggplot2::facet_wrap(~ tipo, ncol = ncol)
    }
  }

  # Añadir línea de límite legal de juveniles si se proporciona
  if (!is.null(limite_juv)) {

    # Calcular punto medio del eje x según el tipo de variable
    if (inherits(datos_long[[var_x]], "Date") || is.numeric(datos_long[[var_x]])) {
      punto_x <- mean(datos_long[[var_x]], na.rm = TRUE)
    } else {
      # Para factores o caracteres, obtener el nivel del medio
      niveles_unicos <- unique(as.character(datos_long[[var_x]]))
      punto_x <- niveles_unicos[ceiling(length(niveles_unicos) / 2)]
    }

    p <- p + ggplot2::geom_hline(yintercept = limite_juv, linetype = "dashed",
                                 color = "red", linewidth = 1) +
      ggplot2::annotate("text", x = punto_x, y = limite_juv, label = paste("Límite:", limite_juv, "%"),
                        hjust = 1.1, vjust = -0.5, color = "black")
  }

  # Añadir paleta de colores si se proporciona
  if (!is.null(paleta)) {
    if (is.null(fill_var)) {
      p <- p + ggplot2::scale_fill_manual(values = paleta)
    } else {
      p <- p + ggplot2::scale_fill_manual(values = paleta) +
        ggplot2::scale_color_manual(values = paleta)
    }
  }

  # Añadir facetado adicional si se proporciona
  if (!is.null(facet_var)) {
    if (is.null(fill_var)) {
      p <- p + ggplot2::facet_wrap(~ tipo + .data[[facet_var]], ncol = ncol)
    } else {
      p <- p + ggplot2::facet_wrap(~ .data[[facet_var]], ncol = ncol)
    }
  }

  # Añadir título y etiquetas
  if (is.null(titulo)) {
    titulo <- paste("Porcentaje de juveniles por", var_x)
  }

  p <- p + ggplot2::labs(
    title = titulo,
    y = "Porcentaje de juveniles (%)",
    x = var_x,
    fill = if (is.null(fill_var)) "Tipo" else fill_var,
    color = if (is.null(fill_var)) NULL else fill_var
  ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )

  return(p)
}



#' Crear dashboard visual para el análisis de juveniles
#'
#' Genera un conjunto de visualizaciones en formato dashboard que resumen
#' los resultados obtenidos a partir de la función `juveniles_por_grupo`.
#' Este dashboard permite explorar la proporción de juveniles según diferentes
#' criterios agrupados, incluyendo comparaciones por número y peso,
#' así como tendencias temporales.
#'
#' @param datos_juveniles Data frame generado por `juveniles_por_grupo`, que
#' contiene los porcentajes de juveniles por grupo.
#' @param var_x Nombre de la variable a utilizar en el eje X de los gráficos
#' (por ejemplo, "fecha_unica" para visualización temporal).
#' @param limite_juv (Opcional) Valor umbral para el porcentaje legal de
#' juveniles. Si se proporciona, se añade como línea de referencia en los gráficos.
#' @param paleta (Opcional) Vector de colores personalizado para los gráficos.
#' @param ordenar_comparacion Lógico. Indica si el gráfico de comparación de
#' porcentajes debe ser ordenado de mayor a menor (por defecto es FALSE).
#'
#' @return Una lista de objetos `ggplot` que representan distintos aspectos del
#' análisis de juveniles, lista para ser integrada en un dashboard.
#' @export
#' @importFrom patchwork wrap_plots
#'
#' @examples
#' # Ejemplo de uso:
#' data_calas <- procesar_calas(data_calas = calas_bitacora)
#' data_faenas <- procesar_faenas(data_faenas = faenas_bitacora)
#' calas_tallas <- procesar_tallas(data_tallas = tallas_bitacora)
#'
#' data_tallasfaenas <- merge(x = data_faenas, y = calas_tallas, by = "codigo_faena")
#' data_total <- merge_tallas_faenas_calas(data_calas = data_calas, data_tallas_faenas = data_tallasfaenas)
#'
#' datos_final <- agregar_variables(data_total)
#' tallas_cols <- c("8", "8.5", "9", "9.5", "10", "10.5", "11", "11.5", "12",
#'                  "12.5", "13", "13.5", "14", "14.5", "15")
#' resultado <- ponderar_tallas_df(df = datos_final, tallas_cols = tallas_cols,
#'                                 captura_col = "catch_ANCHOVETA", a = 0.0001, b = 2.984)
#' resultado$fecha_unica <- convertir_a_fecha(resultado$fecha_inicio_cala, tipo = "date")
#' resultado_juveniles <- juveniles_por_grupo(data = resultado,
#'                                            group_cols = c("fecha_unica"),
#'                                            cols_tallas = tallas_cols)
#'
#' dashboard <- dashboard_juveniles(
#'   datos_juveniles = resultado_juveniles,
#'   var_x = "fecha_unica"
#' )
dashboard_juveniles <- function(datos_juveniles, var_x = NULL, limite_juv = NULL,
                                paleta = NULL, ordenar_comparacion = FALSE) {
  # Validación de parámetros
  if (!is.data.frame(datos_juveniles))
    stop("datos_juveniles debe ser un data.frame")

  # Si no se proporciona var_x, usar la primera columna
  if (is.null(var_x)) {
    var_x <- colnames(datos_juveniles)[1]
  }

  # Crear paleta por defecto si no se proporciona
  if (is.null(paleta)) {
    paleta <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD",
                "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF")
  }

  # Detectar si hay una columna de fecha
  cols_fecha <- grep("fecha|date", colnames(datos_juveniles), ignore.case = TRUE)
  fecha_var <- if (length(cols_fecha) > 0) colnames(datos_juveniles)[cols_fecha[1]] else NULL

  # Crear diferentes visualizaciones
  # 1. Gráfico de comparación por variable principal
  p1 <- Tivy::graficar_juveniles(
    datos_juveniles = datos_juveniles,
    var_x = var_x,
    tipo_grafico = "barras",
    titulo = paste("Comparación de juveniles por", var_x),
    limite_juv = limite_juv,
    ordenar_por = if(ordenar_comparacion) "peso" else "x",
    paleta = c("#3498DB", "#E74C3C")
  )

  # 2. Gráfico de serie temporal si existe una columna de fecha
  p2 <- NULL
  if (!is.null(fecha_var)) {
    p2 <- Tivy::graficar_juveniles(
      datos_juveniles = datos_juveniles,
      var_x = fecha_var,
      fill_var = if (var_x != fecha_var) var_x else NULL,
      tipo_grafico = "lineas",
      titulo = "Evolución temporal del porcentaje de juveniles",
      limite_juv = limite_juv,
      paleta = paleta
    )
  }

  # 3. Boxplot para distribución
  p3 <- Tivy::graficar_juveniles(
    datos_juveniles = datos_juveniles,
    var_x = var_x,
    tipo_grafico = "mixto",
    titulo = paste("Distribución de juveniles por", var_x),
    limite_juv = limite_juv,
    paleta = c("#3498DB", "#E74C3C")
  )

  # 4. Gráfico de dispersión Total vs Porcentaje
  datos_long <- datos_juveniles %>%
    tidyr::pivot_longer(
      cols = c("porc_juv_numero", "porc_juv_peso"),
      names_to = "tipo",
      values_to = "porcentaje"
    ) %>%
    dplyr::mutate(
      tipo = factor(
        tipo,
        levels = c("porc_juv_numero", "porc_juv_peso"),
        labels = c("En número", "En peso")
      ),
      total = ifelse(tipo == "En número", total_numero, total_peso)
    )

  p4 <- ggplot2::ggplot(datos_long, ggplot2::aes(x = .data[[var_x]], y = porcentaje, color = tipo)) +
    ggplot2::geom_point(size = 3, alpha = 0.7) +
    ggplot2::facet_wrap(~ tipo, scales = "free_x") +
    ggplot2::geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +
    ggplot2::labs(
      title = "Relación entre total y porcentaje de juveniles",
      x = "Total (número/peso)",
      y = "Porcentaje de juveniles (%)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c("#3498DB", "#E74C3C"))


  # Retornar la lista de gráficos
  result <- list(
    comparacion = p1,
    serie_temporal = p2,
    mixto = p3,
    relacion = p4
  )

  # Si patchwork está disponible, crear un dashboard combinado
  if (requireNamespace("patchwork", quietly = TRUE)) {
    graficos <- list(p1, p3, p4)
    if (!is.null(p2)) graficos <- c(graficos, list(p2))

    # Filtrar gráficos no nulos
    graficos <- graficos[!sapply(graficos, is.null)]

    # Combinar gráficos
    result$dashboard <- patchwork::wrap_plots(graficos, ncol = 2)
  }

  return(result)
}
