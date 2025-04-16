#' Convertir latitud o longitud a grados decimales
#'
#' @description
#' Convierte coordenadas expresadas en formato grados, minutos y segundos (DMS) o grados y minutos (DM) a grados decimales.
#' Por defecto, se asume que las coordenadas están en el hemisferio sur (latitudes negativas).
#' La función puede corregir automáticamente errores comunes como minutos o segundos mayores a 60.
#'
#' @param coordenadas Vector de caracteres. Cada elemento debe estar en formatos como:
#'   - Con símbolos: `"G° M' S\""`, `"G° M'"`, `"17°26'S"`
#'   - Sin símbolos: `"G M S"`, `"G M"`, `"17 26 S"`
#'   - El hemisferio puede estar incluido en la coordenada
#' @param hemisferio Caracter `"N"`, `"S"`, `"E"`, `"W"` o `"O"` que indica el hemisferio correspondiente
#' cuando no está especificado en la coordenada. `"S"` y `"W"`/`"O"` generan valores negativos. Default: `"S"`.
#' @param corregir_errores Lógico. Si es TRUE (valor por defecto), la función corrige automáticamente
#' valores fuera de rango, como minutos o segundos mayores a 60, convirtiéndolos apropiadamente
#' a la unidad superior.
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
#' # Corregir automáticamente valores fuera de rango
#' dms_a_decimal(c("39° 75' 36\""), corregir_errores = TRUE)  # Minutos > 60
#' dms_a_decimal(c("39° 48' 98\""), corregir_errores = TRUE)  # Segundos > 60
#'
#' # Desactivar corrección automática
#' dms_a_decimal(c("39° 75' 36\""), corregir_errores = FALSE)  # Generará advertencia
#'
#' # En un dataframe
#' # dms_a_decimal(calas_bitacora$Longitud.Fin)
#'
#' @importFrom stringr str_split str_count str_detect str_extract
dms_a_decimal <- function(coordenadas, hemisferio = "S", corregir_errores = TRUE) {
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

      # NUEVO: Corregir automáticamente valores fuera de rango
      if (corregir_errores && length(partes) >= 3) {
        # Corregir segundos >= 60
        if (!is.na(partes[3]) && partes[3] >= 60) {
          minutos_extra <- floor(partes[3] / 60)
          partes[3] <- partes[3] %% 60
          partes[2] <- partes[2] + minutos_extra
        }

        # Corregir minutos >= 60
        if (!is.na(partes[2]) && partes[2] >= 60) {
          grados_extra <- floor(partes[2] / 60)
          partes[2] <- partes[2] %% 60
          partes[1] <- partes[1] + grados_extra
        }
      } else {
        # Emitir warnings pero no corregir
        if (length(partes) >= 1 && (is.na(partes[1]) || partes[1] < 0 || partes[1] > 180)) {
          warning(paste("Grados fuera de rango (0-180) en la coordenada:", coord_original))
        }

        if (length(partes) >= 2 && (is.na(partes[2]) || partes[2] < 0 || partes[2] >= 60)) {
          warning(paste("Minutos fuera de rango (0-59) en la coordenada:", coord_original))
        }

        if (length(partes) >= 3 && (is.na(partes[3]) || partes[3] < 0 || partes[3] >= 60)) {
          warning(paste("Segundos fuera de rango (0-59) en la coordenada:", coord_original))
        }
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
#' @param linea_costa Data frame que representa la línea de costa, debe contener columnas llamadas `'Long'` y `'Lat'`. Default `Tivy::linea_costa_peru`.
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
#' data_calas <- procesar_calas(data_calas = calas_bitacora)
#' distancia_costa(
#'   lon = data_calas$lon_final,
#'   lat = data_calas$lat_final,
#'   linea_costa = Tivy::linea_costa_peru,
#'   tipo_distancia = "haversine",
#'   unidad = "mn",
#'   paralelo = TRUE,
#'   nucleos = 2
#' )
#' }
#'
#' @importFrom future plan multisession
#' @importFrom future.apply future_lapply
distancia_costa <- function(lon,
                            lat,
                            linea_costa = Tivy::linea_costa_peru,
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
        lon_punto = lon_validos[indices_lote],
        lat_punto = lat_validos[indices_lote],
        costa_lon = linea_costa$Long,
        costa_lat = linea_costa$Lat,
        tipo_distancia = tipo_distancia,
        ventana = ventana,
        unidad = unidad
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
#'
#' @description
#' Esta función clasifica un conjunto de coordenadas geográficas (longitud y latitud) como "tierra" o "mar" según su posición relativa a una línea de costa. Se considera que un punto está en tierra si su longitud es mayor a la de su punto más cercano en la línea de costa. Además, permite el cálculo en paralelo para mejorar el rendimiento en grandes volúmenes de datos.
#'
#' @param x_punto Vector numérico de longitudes (en grados decimales).
#' @param y_punto Vector numérico de latitudes (en grados decimales).
#' @param linea_costa `data.frame` con las coordenadas de la línea de costa. Debe tener columnas nombradas `Long` y `Lat`. Por defecto se puede usar `Tivy::linea_costa_peru`.
#' @param paralelo Lógico. Si `TRUE`, realiza el cálculo en paralelo utilizando múltiples núcleos. Por defecto es `FALSE`.
#' @param nucleos Número de núcleos a usar en caso de procesamiento paralelo. Por defecto es `4`.
#' @param tipo_distancia Tipo de distancia geodésica a usar en el cálculo: `"haversine"` (por defecto) u otras si la función interna lo permite.
#' @param ventana Ventana geográfica en grados para reducir el número de puntos de la línea de costa a considerar por cada punto. Por defecto es `0.5`.
#' @param unidad Unidad de medida para la distancia: `"km"` (por defecto) u otra si la función interna lo permite.
#'
#' @return Un vector de texto del mismo largo que `x_punto`, indicando si cada punto se encuentra en `"tierra"` o `"mar"`. Los valores `NA` se mantienen como `NA`.
#'
#' @details
#' Esta función usa internamente `calcular_distancias_vectorizado()` para identificar el punto más cercano en la línea de costa para cada coordenada. Si `paralelo = TRUE`, utiliza los paquetes `future` y `future.apply` para distribuir el trabajo entre varios núcleos.
#'
#' @examples
#' data_calas <- procesar_calas(data_calas = calas_bitacora)
#' resultado <- puntos_tierra(
#'   x_punto = data_calas$lon_final,
#'   y_punto = data_calas$lat_final,
#'   linea_costa = Tivy::linea_costa_peru
#' )
#' table(resultado)
#'
#' @export
puntos_tierra <- function(x_punto,
                          y_punto,
                          linea_costa = Tivy::linea_costa_peru,
                          paralelo = FALSE,
                          nucleos = 4,
                          tipo_distancia = "haversine",
                          ventana = 0.5,
                          unidad = "mn") {
  # --- Validaciones ---
  if (!is.numeric(x_punto) || !is.numeric(y_punto)) {
    stop("`x_punto` y `y_punto` deben ser vectores numéricos.")
  }

  if (length(x_punto) != length(y_punto)) {
    stop("`x_punto` y `y_punto` deben tener la misma longitud.")
  }

  if (!is.data.frame(linea_costa)) {
    stop("`linea_costa` debe ser un data.frame.")
  }

  if (!all(c("Long", "Lat") %in% names(linea_costa))) {
    stop("`linea_costa` debe contener columnas llamadas 'Long' y 'Lat'.")
  }

  if (!is.logical(paralelo)) {
    stop("`paralelo` debe ser TRUE o FALSE.")
  }

  if (!is.numeric(nucleos) || nucleos < 1) {
    stop("`nucleos` debe ser un número entero positivo.")
  }

  if (!tipo_distancia %in% c("haversine", "manhattan", "grid")) {
    stop("`tipo_distancia` debe ser uno de: 'haversine', 'manhattan', 'grid'.")
  }

  if (!is.numeric(ventana) || ventana < 0) {
    stop("`ventana` debe ser un número no negativo.")
  }

  if (!unidad %in% c("km", "mn")) {
    stop("`unidad` debe ser 'km' o 'mn'.")
  }

  # Convertir factores o cadenas a numéricos si es necesario
  x_punto <- as.numeric(x_punto)
  y_punto <- as.numeric(y_punto)
  linea_costa$Long <- as.numeric(linea_costa$Long)
  linea_costa$Lat <- as.numeric(linea_costa$Lat)

  # Eliminar coordenadas NA si existen (mantener NAs en salida)
  n <- length(x_punto)

  procesar_punto <- function(i) {
    if (is.na(x_punto[i]) || is.na(y_punto[i])) return(NA_character_)

    resultado <- calcular_distancias_vectorizado(
      lon_punto = x_punto[i],
      lat_punto = y_punto[i],
      costa_lon = linea_costa$Long,
      costa_lat = linea_costa$Lat,
      tipo_distancia = tipo_distancia,
      ventana = ventana,
      unidad = unidad
    )

    idx_costa <- resultado$indices[1]

    if (idx_costa >= 2 && idx_costa < nrow(linea_costa)) {
      p1 <- c(linea_costa$Long[idx_costa - 1], linea_costa$Lat[idx_costa - 1])
      p2 <- c(linea_costa$Long[idx_costa + 1], linea_costa$Lat[idx_costa + 1])
      p  <- c(x_punto[i], y_punto[i])

      direccion <- (p2[1] - p1[1]) * (p[2] - p1[2]) - (p2[2] - p1[2]) * (p[1] - p1[1])

      if (direccion > 0) return("mar") else return("tierra")
    }

    return("desconocido")
  }

  # Ejecutar en paralelo o no
  if (paralelo) {
    future::plan(future::multisession, workers = nucleos)
    resultado <- future.apply::future_lapply(seq_len(n), procesar_punto)
    future::plan(future::sequential)
  } else {
    resultado <- lapply(seq_len(n), procesar_punto)
  }

  return(unlist(resultado))
}


