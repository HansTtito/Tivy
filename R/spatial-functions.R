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
  if (missing(coordenadas)) {
    stop("El parámetro 'coordenadas' es obligatorio.")
  }

  if (length(coordenadas) == 0) {
    warning("El vector de coordenadas está vacío.")
    return(numeric(0))
  }

  # Convertir a character si es factor
  if (is.factor(coordenadas)) {
    coordenadas <- as.character(coordenadas)
    warning("El vector de coordenadas ha sido convertido de factor a character.")
  }

  if (!is.character(coordenadas)) {
    if (is.numeric(coordenadas)) {
      warning("Las coordenadas proporcionadas ya son numéricas. Se devuelven sin cambios.")
      return(coordenadas)
    } else {
      stop("Las coordenadas deben ser una cadena de texto o un vector de caracteres.")
    }
  }

  if (!hemisferio %in% c("N", "S", "E", "W", "O")) {
    stop("El hemisferio debe ser uno de: 'N', 'S', 'E', 'W' o 'O'.")
  }

  # Procesar cada coordenada
  resultados <- vapply(coordenadas, function(coord) {
    # Manejar NA
    if (is.na(coord) || coord == "") {
      return(NA_real_)
    }

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

      # Verificar si hay componentes numéricos
      if (length(componentes_num) == 0) {
        warning(paste("No se encontraron componentes numéricos en la coordenada:", coord_original))
        return(NA_real_)
      }

      # Convertir a numérico con validación
      partes <- suppressWarnings(as.numeric(componentes_num))

      # Verificar si algún valor es NA después de la conversión
      if (any(is.na(partes))) {
        warning(paste("Error al convertir a numérico algún componente de la coordenada:", coord_original))
        return(NA_real_)
      }

      # Validar rangos de las partes
      if (length(partes) >= 1 && (is.na(partes[1]) || partes[1] < 0 || partes[1] > 180)) {
        warning(paste("Grados fuera de rango (0-180) en la coordenada:", coord_original))
      }

      if (length(partes) >= 2 && (is.na(partes[2]) || partes[2] < 0 || partes[2] >= 60)) {
        warning(paste("Minutos fuera de rango (0-59) en la coordenada:", coord_original))
      }

      if (length(partes) >= 3 && (is.na(partes[3]) || partes[3] < 0 || partes[3] >= 60)) {
        warning(paste("Segundos fuera de rango (0-59) en la coordenada:", coord_original))
      }

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

      # Validar el resultado final
      if (abs(decimal) > 180) {
        warning(paste("La coordenada decimal calculada está fuera de rango (-180 a 180):", decimal))
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
  # Validación de parámetros
  if (missing(lon) || missing(lat) || missing(linea_costa)) {
    stop("Los parámetros 'lon', 'lat' y 'linea_costa' son obligatorios.")
  }

  # Validar tipos de datos
  if (!is.numeric(lon)) stop("El parámetro 'lon' debe ser numérico.")
  if (!is.numeric(lat)) stop("El parámetro 'lat' debe ser numérico.")
  if (!is.data.frame(linea_costa)) stop("El parámetro 'linea_costa' debe ser un data.frame.")
  if (!is.logical(devolver_indices)) stop("El parámetro 'devolver_indices' debe ser lógico (TRUE/FALSE).")
  if (!is.character(tipo_distancia)) stop("El parámetro 'tipo_distancia' debe ser un texto.")
  if (!is.character(unidad)) stop("El parámetro 'unidad' debe ser un texto.")
  if (!is.numeric(ventana)) stop("El parámetro 'ventana' debe ser numérico.")
  if (!is.logical(paralelo)) stop("El parámetro 'paralelo' debe ser lógico (TRUE/FALSE).")
  if (!is.numeric(nucleos) || nucleos < 1) stop("El parámetro 'nucleos' debe ser un número entero positivo.")

  # Validar longitud de vectores
  if (length(lon) != length(lat)) {
    stop("Los vectores 'lon' y 'lat' deben tener la misma longitud.")
  }

  # Validar rangos de coordenadas
  if (any(abs(lon) > 180, na.rm = TRUE)) {
    warning("Se detectaron valores de longitud fuera del rango válido (-180 a 180).")
  }
  if (any(abs(lat) > 90, na.rm = TRUE)) {
    warning("Se detectaron valores de latitud fuera del rango válido (-90 a 90).")
  }

  # Validar tipo_distancia
  tipos_validos <- c("haversine", "euclidean", "grid")
  if (!tipo_distancia %in% tipos_validos) {
    stop("El parámetro 'tipo_distancia' debe ser uno de: ", paste(tipos_validos, collapse = ", "))
  }

  # Validar unidad
  unidades_conocidas <- c("mn", "km", "m", "mi")
  if (!unidad %in% unidades_conocidas) {
    warning("La unidad '", unidad, "' no es una de las unidades comunes: ", paste(unidades_conocidas, collapse = ", "))
  }

  # Verificar estructura de linea_costa
  if (!all(c("Long", "Lat") %in% colnames(linea_costa))) {
    stop("linea_costa debe contener columnas 'Long' y 'Lat'")
  }

  # Verificar que linea_costa tiene datos
  if (nrow(linea_costa) == 0) {
    stop("linea_costa está vacío")
  }

  # Verificar que linea_costa tiene coordenadas numéricas
  if (!is.numeric(linea_costa$Long) || !is.numeric(linea_costa$Lat)) {
    stop("Las columnas 'Long' y 'Lat' de linea_costa deben ser numéricas")
  }

  # Manejo de valores NA
  validos <- !is.na(lon) & !is.na(lat)
  if (sum(validos) == 0) {
    warning("Todos los puntos de entrada contienen valores NA")
    return(rep(NA, length(validos)))
  }

  lon_validos <- lon[validos]
  lat_validos <- lat[validos]

  # Dividir puntos en lotes
  n_puntos <- length(lon_validos)
  nucleos <- min(nucleos, n_puntos)  # Ajustar núcleos si hay menos puntos que núcleos
  tamanio_lote <- ceiling(n_puntos / nucleos)
  lotes_indices <- split(seq_len(n_puntos), ceiling(seq_len(n_puntos) / tamanio_lote))

  # Cargar librería future.apply si se va a usar paralelo
  if (paralelo) {
    if (!requireNamespace("future.apply", quietly = TRUE)) {
      warning("Paquete 'future.apply' no encontrado. Usando procesamiento secuencial.")
      paralelo <- FALSE
    } else {
      future::plan(future::multisession, workers = nucleos)
      on.exit(future::plan(future::sequential), add = TRUE)  # Asegurar que se restaure el plan secuencial
    }
  }

  # Ejecutar en paralelo o secuencialmente
  apply_fun <- if (paralelo) future.apply::future_lapply else lapply

  # Función de cálculo envuelta en tryCatch para manejar errores
  resultados_lotes <- tryCatch({
    apply_fun(lotes_indices, function(indices_lote) {
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
  }, error = function(e) {
    stop("Error en el cálculo de distancias: ", e$message)
  })

  # Combinar resultados
  tryCatch({
    distancias <- unlist(lapply(resultados_lotes, `[[`, "distancias"))
    indices <- unlist(lapply(resultados_lotes, `[[`, "indices"))

    # Verificar resultados
    if (length(distancias) != sum(validos)) {
      warning("El número de distancias calculadas no coincide con el número de puntos válidos.")
    }

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
  }, error = function(e) {
    warning("Error al procesar los resultados: ", e$message)
    return(rep(NA_real_, length(validos)))
  })
}



#' Puntos en tierra
#' @description
#' Función que permite estimar los puntos que están a la derecha o izquierda de la línea de costa
#'
#' @param x_punto Un vector con las longitudes
#' @param y_punto Un vector con las latitudes
#' @param linea_costa Un data frame con las coordenadas de la costa, el orden debe ser: lon, lat
#' @param paralelo Lógico. Si `TRUE`, realiza el cálculo en paralelo utilizando múltiples núcleos. Default `FALSE`.
#' @param nucleos Número de núcleos a usar para procesamiento paralelo. Default `4`.
#' @return Un vector con clasificación "tierra" o "mar" para cada punto.
#' @export
#' @rdname puntos_tierra
#' @examples
#' data_calas = processing_calas(data_calas = calas)
#' puntos_tierra(x_punto = data_calas$lon_final, y_punto = data_calas$lat_final, linea_costa = Shoreline_Peru)
#' @importFrom future plan multisession sequential
#' @importFrom future.apply future_lapply
puntos_tierra <- function(x_punto, y_punto, linea_costa, paralelo = FALSE, nucleos = 4) {
  # Validación de parámetros
  if (missing(x_punto) || missing(y_punto) || missing(linea_costa)) {
    stop("Los parámetros 'x_punto', 'y_punto' y 'linea_costa' son obligatorios.")
  }

  # Validar tipos de datos
  if (!is.numeric(x_punto)) stop("El parámetro 'x_punto' debe ser numérico.")
  if (!is.numeric(y_punto)) stop("El parámetro 'y_punto' debe ser numérico.")
  if (!is.data.frame(linea_costa)) stop("El parámetro 'linea_costa' debe ser un data.frame.")
  if (!is.logical(paralelo)) stop("El parámetro 'paralelo' debe ser lógico (TRUE/FALSE).")
  if (!is.numeric(nucleos) || nucleos < 1) stop("El parámetro 'nucleos' debe ser un número entero positivo.")

  # Validar longitud de vectores
  if (length(x_punto) != length(y_punto)) {
    stop("Los vectores 'x_punto' y 'y_punto' deben tener la misma longitud.")
  }

  # Validar rangos de coordenadas
  if (any(abs(x_punto) > 180, na.rm = TRUE)) {
    warning("Se detectaron valores de longitud fuera del rango válido (-180 a 180).")
  }
  if (any(abs(y_punto) > 90, na.rm = TRUE)) {
    warning("Se detectaron valores de latitud fuera del rango válido (-90 a 90).")
  }

  # Asegurarse de que linea_costa tenga los nombres de columnas correctos
  if (!"Long" %in% colnames(linea_costa) || !"Lat" %in% colnames(linea_costa)) {
    stop("linea_costa debe contener columnas 'Long' y 'Lat'")
  }

  # Verificar que linea_costa tiene datos
  if (nrow(linea_costa) == 0) {
    stop("linea_costa está vacío")
  }

  # Verificar que linea_costa tiene coordenadas numéricas
  if (!is.numeric(linea_costa$Long) || !is.numeric(linea_costa$Lat)) {
    stop("Las columnas 'Long' y 'Lat' de linea_costa deben ser numéricas")
  }

  # Limpiar datos de entrada
  idx_validos <- !is.na(x_punto) & !is.na(y_punto)
  if (sum(idx_validos) == 0) {
    warning("Todos los puntos de entrada contienen valores NA")
    return(rep(NA_character_, length(x_punto)))
  }

  x_validos <- x_punto[idx_validos]
  y_validos <- y_punto[idx_validos]

  # Preparar vectores de resultados del mismo tamaño que los originales
  resultados <- rep(NA_character_, length(x_punto))

  # Constantes
  grados2rad <- pi / 180

  # Convertir a radianes
  tryCatch({
    lon_rad <- x_validos * grados2rad
    lat_rad <- y_validos * grados2rad
    shore_lon_rad <- linea_costa$Long * grados2rad
    shore_lat_rad <- linea_costa$Lat * grados2rad
  }, error = function(e) {
    stop("Error al convertir coordenadas a radianes: ", e$message)
  })

  # Función para encontrar el índice del punto costero más cercano
  find_closest_shore <- function(i) {
    tryCatch({
      # Fórmula del gran círculo para calcular distancias
      xy_rad <- sin(lat_rad[i]) * sin(shore_lat_rad)
      yx_rad <- cos(lat_rad[i]) * cos(shore_lat_rad) * cos(shore_lon_rad - lon_rad[i])

      # Asegurar que los valores están en el rango [-1, 1] para evitar NaN en acos
      sum_rad <- xy_rad + yx_rad
      sum_rad[sum_rad < -1] <- -1
      sum_rad[sum_rad > 1] <- 1

      dist_rad <- acos(sum_rad)

      # Devolver índice del punto más cercano
      return(which.min(dist_rad))
    }, error = function(e) {
      warning("Error al calcular el punto costero más cercano para el índice ", i, ": ", e$message)
      return(NA_integer_)
    })
  }

  # Procesamiento paralelo o secuencial
  if (paralelo) {
    if (!requireNamespace("future", quietly = TRUE) ||
        !requireNamespace("future.apply", quietly = TRUE)) {
      warning("Paquetes 'future' y/o 'future.apply' no encontrados. Usando procesamiento secuencial.")
      paralelo <- FALSE
    }
  }

  # Ajustar núcleos si hay menos puntos que núcleos
  nucleos <- min(nucleos, length(x_validos))

  if (paralelo) {
    tryCatch({
      # Configurar paralelización
      future::plan(future::multisession, workers = nucleos)
      on.exit(future::plan(future::sequential), add = TRUE)  # Asegurar que se restaure el plan secuencial

      # Calcular índices de los puntos más cercanos en paralelo
      closest_indices <- future.apply::future_lapply(
        1:length(x_validos),
        function(i) find_closest_shore(i),
        future.seed = TRUE
      )

      # Desempaquetar resultado
      closest_indices <- unlist(closest_indices)
    }, error = function(e) {
      warning("Error en procesamiento paralelo: ", e$message, ". Cambiando a procesamiento secuencial.")
      closest_indices <- sapply(1:length(x_validos), find_closest_shore)
    })
  } else {
    # Versión secuencial
    closest_indices <- sapply(1:length(x_validos), find_closest_shore)
  }

  # Determinar si cada punto está en tierra o mar
  tryCatch({
    for (i in 1:length(x_validos)) {
      idx_costa <- closest_indices[i]

      # Saltear puntos con índice NA
      if (is.na(idx_costa)) {
        next
      }

      # Para la costa peruana (océano al oeste), determinar posición relativa
      # Si el punto está al este de la costa (longitud mayor), está en tierra
      resultados[idx_validos][i] <- ifelse(
        x_validos[i] > linea_costa$Long[idx_costa],
        "tierra",
        "mar"
      )
    }
  }, error = function(e) {
    warning("Error al determinar si los puntos están en tierra o mar: ", e$message)
  })

  return(resultados)
}



#' Graficar polígonos de zonas de suspensión pesquera con ggplot2
#'
#' @description
#' Crea visualizaciones estáticas de polígonos para zonas de suspensión pesquera
#' utilizando ggplot2. Devuelve un objeto ggplot2 que puede ser modificado posteriormente.
#'
#' @param datos Un data frame con coordenadas de latitud y longitud de inicio y fin,
#'        que pueden estar en formato de texto (como "12°30'S") o decimales.
#' @param costa Un data frame que contiene la línea de costa para visualización.
#'        Debe tener columnas 'Long' y 'Lat'. Por defecto utiliza Shoreline_Peru.
#' @param titulo Título para el gráfico.
#' @param colores Vector de colores para los polígonos.
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
#' g <- graficar_poligonos_ggplot(datos = resultados, costa = Shoreline_Peru)
#'
#' # Personalizar posteriormente
#' g +
#'   ggplot2::xlim(-80, -70) +
#'   ggplot2::theme_minimal() +
#'   ggplot2::labs(title = "Título personalizado")
#'
#' @export
graficar_poligonos_ggplot <- function(datos,
                                      costa = Tivy::Shoreline_Peru,
                                      titulo = "Zonas de suspensión pesquera",
                                      colores = NULL,
                                      mostrar_leyenda = FALSE,
                                      etiquetas = NULL,
                                      agregar_grid = FALSE) {

  # Preparar los polígonos (todo el procesamiento de datos es el mismo)
  poligonos <- preparar_poligonos(datos, costa)

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
#'        Debe tener columnas 'Long' y 'Lat'. Por defecto utiliza Shoreline_Peru.
#' @param titulo Título para el gráfico.
#' @param colores Vector de colores para los polígonos.
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
#' m <- graficar_poligonos_leaflet(datos = resultados, costa = Shoreline_Peru)
#'
#' # Personalizar posteriormente
#' m %>%
#'   leaflet::addMarkers(lng = -77.1, lat = -12.0, popup = "Lima") %>%
#'   leaflet::addCircleMarkers(lng = -76.3, lat = -13.4, radius = 5, color = "red")
#'
#' @export
graficar_poligonos_leaflet <- function(datos,
                                       costa = Tivy::Shoreline_Peru,
                                       titulo = "Zonas de suspensión pesquera",
                                       colores = NULL,
                                       mostrar_leyenda = FALSE,
                                       etiquetas = NULL,
                                       capas_base = FALSE,
                                       minimap = FALSE) {

  # Preparar los polígonos (todo el procesamiento de datos es el mismo)
  poligonos <- preparar_poligonos(datos, costa)

  # Crear visualización interactiva con leaflet
  return(Tivy:::graficar_interactivo(
    poligonos, costa, titulo, colores,
    mostrar_leyenda = mostrar_leyenda,
    etiquetas = etiquetas,
    capas_base = capas_base,
    minimap = minimap
  ))
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

# También mantener la función original para compatibilidad
#' @export
graficar_poligonos <- function(datos,
                               costa = Tivy::Shoreline_Peru,
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
      titulo = titulo,
      colores = colores,
      mostrar_leyenda = mostrar_leyenda,
      etiquetas = etiquetas,
      capas_base = capas_base,
      minimap = minimap
    ))
  }
}
