#' Relación Talla - Peso
#'
#' Esta función estima el peso de un individuo a partir de su talla, utilizando
#' la fórmula general de la relación longitud-peso: \eqn{W = a \cdot L^b}, donde
#' \eqn{W} es el peso, \eqn{L} la longitud (talla), \eqn{a} y \eqn{b} los parámetros específicos.
#'
#' @param talla Un vector numérico que contiene las tallas de los individuos.
#' @param a Valor numérico del coeficiente de la relación longitud-peso.
#' @param b Valor numérico del exponente de la relación longitud-peso.
#' @return Un vector numérico con los pesos estimados.
#' @export
#' @examples
#'
#' tallas <- seq(5, 20, by = 0.5)
#' a <- 0.0001
#' b <- 2.984
#'
#' pesos <- talla_peso(talla = tallas, a = a, b = b)
#'
#' print(pesos)
talla_peso <- function(talla, a, b) {
  # Validación de parámetros
  if (!is.numeric(talla)) stop("El parámetro 'talla' debe ser numérico.")
  if (!is.numeric(a)) stop("El parámetro 'a' debe ser numérico.")
  if (!is.numeric(b)) stop("El parámetro 'b' debe ser numérico.")

  if (any(talla <= 0, na.rm = TRUE)) warning("Se detectaron valores de talla <= 0, que podrían producir resultados no válidos.")
  if (a <= 0) warning("El valor de 'a' es <= 0, lo que podría producir resultados biológicamente inverosímiles.")

  return(a * talla^b)
}



#' Ponderación de tallas según captura total
#'
#' Calcula una ponderación de las tallas muestreadas en función de la captura total
#' registrada. Esto permite escalar la frecuencia observada a la captura total.
#'
#' @param frecuencia Un vector numérico con la frecuencia de tallas observadas en el muestreo.
#' @param captura Valor numérico con la captura total (en kg o toneladas, según el caso).
#' @param tallas Un vector numérico con las tallas correspondientes a las frecuencias.
#' @param a Valor numérico del coeficiente de la relación longitud-peso.
#' @param b Valor numérico del exponente de la relación longitud-peso.
#' @return Un vector numérico con las frecuencias ponderadas.
#' @export
#' @examples
#'
#' frecuencia <- c(0, 1, 4, 8, 16, 12, 23, 34, 55, 35, 24, 15, 10, 6, 3, 2)
#'
#' tallas <- seq(5, 20, by = 1)
#'
#' captura <- 1000
#' a <- 0.0001
#' b <- 2.984
#'
#' ponderadas <- ponderacion(frecuencia, captura, tallas, a, b)
#'
#' print(head(ponderadas))
ponderacion <- function(frecuencia, captura, tallas, a, b, silenciar_warnings = FALSE) {
  # Validación de parámetros
  if (!is.numeric(frecuencia)) stop("El parámetro 'frecuencia' debe ser numérico.")
  if (!is.numeric(tallas)) stop("El parámetro 'tallas' debe ser numérico.")
  if (!is.numeric(captura)) stop("El parámetro 'captura' debe ser numérico.")
  if (!is.numeric(a)) stop("El parámetro 'a' debe ser numérico.")
  if (!is.numeric(b)) stop("El parámetro 'b' debe ser numérico.")
  if (length(frecuencia) != length(tallas)) {
    stop("Los vectores 'frecuencia' y 'tallas' deben tener la misma longitud.")
  }

  # Acumular mensajes de advertencia
  mensajes_warning <- character()

  # Validación de captura
  if (is.na(captura) || captura <= 0) {
    mensajes_warning <- c(mensajes_warning,
                          "El valor de 'captura' es NA o <= 0, se usará captura = 1.")
    captura <- 1
  }

  # Validación de tallas
  if (any(tallas <= 0, na.rm = TRUE)) {
    mensajes_warning <- c(mensajes_warning,
                          "Hay tallas <= 0, podrían producirse resultados no válidos.")
  }

  # Validación de frecuencia
  frecuencia[is.na(frecuencia)] <- 0
  if (sum(frecuencia, na.rm = TRUE) == 0) {
    mensajes_warning <- c(mensajes_warning,
                          "La suma de frecuencias es cero. Se devolverá un vector de ceros.")
    if (!silenciar_warnings && length(mensajes_warning) > 0)
      warning(paste(mensajes_warning, collapse = " | "))
    return(rep(0, length(tallas)))
  }

  # Cálculo de pesos
  peso <- talla_peso(talla = tallas, a = a, b = b) * frecuencia
  suma_peso <- sum(peso, na.rm = TRUE)
  if (suma_peso == 0) {
    mensajes_warning <- c(mensajes_warning,
                          "La suma de pesos es cero. Se devolverá un vector de ceros.")
    if (!silenciar_warnings && length(mensajes_warning) > 0)
      warning(paste(mensajes_warning, collapse = " | "))
    return(rep(0, length(tallas)))
  }

  # Mostrar advertencias acumuladas (si existen)
  if (!silenciar_warnings && length(mensajes_warning) > 0) {
    warning(paste(mensajes_warning, collapse = " | "))
  }

  # Cálculo final
  talla_ponderada <- (captura / suma_peso) * frecuencia
  return(talla_ponderada)
}


#' Ponderación de tallas en un data frame
#'
#' Esta función realiza la ponderación de frecuencias de tallas según la captura total
#' utilizando la relación longitud-peso. Permite procesamiento paralelo para conjuntos
#' de datos grandes.
#'
#' @param df Un data frame que contiene las columnas de tallas y captura.
#' @param tallas_cols Un vector de caracteres con los nombres de las columnas que representan las tallas.
#' @param captura_col Nombre de la columna que contiene los valores de captura.
#' @param a Valor numérico del coeficiente de la relación longitud-peso.
#' @param b Valor numérico del exponente de la relación longitud-peso.
#' @param paralelo Booleano indicando si se debe utilizar procesamiento paralelo.
#' @param num_cores Número de núcleos a utilizar (si paralelo=TRUE).
#' @param tam_bloque Tamaño de los bloques para procesamiento (si paralelo=TRUE).
#'
#' @return Un data frame con las columnas originales y las columnas de tallas ponderadas.
#'         Las columnas ponderadas tendrán el prefijo "pond_" seguido del nombre original.
#'
#' @examples
#'
#' data(calas_bitacora)
#' data(faenas_bitacora)
#' data(tallas_bitacora)
#'
#' data_calas <- procesar_calas(data_calas = calas_bitacora)
#' data_faenas <- procesar_faenas(data_faenas = faenas_bitacora)
#' calas_tallas <- procesar_tallas(data_tallas = tallas_bitacora)
#'
#' data_tallas_faenas <- merge(x = data_faenas, y = calas_tallas, by = 'codigo_faena')
#'
#' data_total <- merge_tallas_faenas_calas(data_calas = data_calas, data_tallas_faenas = data_tallas_faenas)
#'
#' tallas_columnas <- c("8", "8.5", "9", "9.5", "10", "10.5", "11", "11.5","12", "12.5", "13", "13.5", "14", "14.5", "15")
#'
#' # Procesamiento secuencial
#' resultado <- ponderar_tallas_df(df = data_total, tallas_cols = tallas_columnas, captura_col = "catch_ANCHOVETA", a= 0.0001, b = 2.984)
#'
#' print(head(resultado))
#'
#' # Procesamiento paralelo para conjuntos grandes
#' resultado_paralelo <- ponderar_tallas_df(
#'   df = data_total, tallas_cols = tallas_columnas, captura_col = "catch_ANCHOVETA", a = 0.0001, b = 2.984, paralelo = TRUE
#' )
#'
#' print(head(resultado_paralelo))
#'
#' @import parallel
#' @export
ponderar_tallas_df <- function(df, tallas_cols, captura_col, a, b,
                               paralelo = FALSE, num_cores = NULL, tam_bloque = 10000,
                               silenciar_warnings = TRUE) {
  # Validaciones iniciales
  if (!is.data.frame(df)) {
    stop("El primer argumento debe ser un data frame.")
  }
  if (!all(tallas_cols %in% names(df))) {
    stop("Algunas columnas de tallas no existen en el data frame.")
  }
  if (!(captura_col %in% names(df))) {
    stop("La columna de captura no existe en el data frame.")
  }

  # Si se solicita procesamiento paralelo
  if (paralelo) {
    # Verificar paquetes necesarios
    if (!requireNamespace("future", quietly = TRUE) ||
        !requireNamespace("future.apply", quietly = TRUE)) {
      stop("Paquetes future y future.apply necesarios para procesamiento paralelo. Instálalos.")
    }

    # Configurar procesamiento paralelo
    if (is.null(num_cores)) {
      num_cores <- max(1, parallel::detectCores() / 2)
    }
    future::plan(future::multisession, workers = num_cores)

    # Dividir en bloques
    num_filas <- nrow(df)
    indices_bloques <- split(1:num_filas,
                             ceiling(seq_along(1:num_filas) / tam_bloque))

    # Procesar bloques en paralelo
    resultados <- future.apply::future_lapply(
      indices_bloques,
      function(indices) {
        bloque <- df[indices, ]
        procesar_bloque(bloque, tallas_cols, captura_col, a, b, silenciar_warnings = TRUE)
      },
      future.seed = TRUE
    )

    # Combinar resultados
    resultado_final <- do.call(rbind, resultados)
    future::plan(future::sequential)

    # Mostrar resumen de warnings al final si no se silencian
    if (!silenciar_warnings) {
      # Aquí podríamos agregar un mensaje resumen si fuera necesario
      # pero como el procesamiento es paralelo, sería más complejo obtener conteos exactos
      message("Procesamiento paralelo completado. Algunas filas podrían tener valores NA o cero para tallas ponderadas.")
    }

    return(resultado_final)
  } else {
    # Procesamiento secuencial
    return(procesar_bloque(df, tallas_cols, captura_col, a, b, silenciar_warnings))
  }
}



#' Porcentaje de juveniles
#'
#' Estima el porcentaje de individuos considerados juveniles en una muestra
#' según una talla límite establecida.
#'
#' @param frecuencia Un vector numérico con la frecuencia de tallas muestreadas.
#' @param tallas Vector de tallas correspondiente a las frecuencias.
#' @param juvLim Talla límite (por defecto 12 cm) para clasificar como juvenil.
#' @return Porcentaje de juveniles en la muestra.
#' @export
#' @examples
#'
#' frecuencia <- c(0, 1, 4, 8, 16, 12, 23, 34, 55, 35, 24, 15, 10, 6, 3, 2)
#'
#' tallas <- seq(5, 20, by = 1)
#'
#' porc <- porc_juveniles(frecuencia, tallas, juvLim = 12)
#'
#' print(porc)
porc_juveniles <- function(frecuencia, tallas, juvLim = 12, silenciar_warnings = FALSE) {
  # Validación de parámetros
  if (!is.numeric(frecuencia)) stop("El parámetro 'frecuencia' debe ser numérico.")
  if (!is.numeric(tallas)) stop("El parámetro 'tallas' debe ser numérico.")
  if (!is.numeric(juvLim)) stop("El parámetro 'juvLim' debe ser numérico.")
  if (length(frecuencia) != length(tallas)) {
    stop("Los vectores 'frecuencia' y 'tallas' deben tener la misma longitud.")
  }
  if (juvLim <= 0 && !silenciar_warnings)
    warning("El valor de 'juvLim' es <= 0, lo que podría no ser biológicamente plausible.")

  total_frecuencia <- sum(frecuencia, na.rm = TRUE)
  if (total_frecuencia == 0) {
    if (!silenciar_warnings) warning("La suma de frecuencias es cero. Se devolverá NA.")
    return(NA_real_)
  }

  juv <- 100 * (sum(frecuencia[tallas < juvLim], na.rm = TRUE) / total_frecuencia)
  return(juv)
}



#' Talla mínima observada con frecuencia positiva
#'
#' @param frecuencia Un vector numérico con las frecuencias de tallas.
#' @param tallas Vector de tallas correspondiente.
#' @return Valor mínimo de talla con frecuencia mayor que cero.
#' @export
#' @examples
#' min_range(frecuencia = c(0,0,1,2,3), tallas = c(5,6,7,8,9))
min_range <- function(frecuencia, tallas) {
  # Validación de parámetros
  if (!is.numeric(frecuencia)) stop("El parámetro 'frecuencia' debe ser numérico.")
  if (!is.numeric(tallas)) stop("El parámetro 'tallas' debe ser numérico.")

  if (length(frecuencia) != length(tallas)) {
    stop("Los vectores 'frecuencia' y 'tallas' deben tener la misma longitud.")
  }

  # Verificar si hay frecuencias positivas
  if (all(frecuencia <= 0, na.rm = TRUE) || all(is.na(frecuencia))) {
    warning("No hay frecuencias positivas. Se devolverá NA.")
    return(NA_real_)
  }

  frecuencia[frecuencia <= 0] <- NA
  return(min(tallas[!is.na(frecuencia)], na.rm = TRUE))
}



#' Talla máxima observada con frecuencia positiva
#'
#' @param frecuencia Un vector numérico con las frecuencias de tallas.
#' @param tallas Vector de tallas correspondiente.
#' @return Valor máximo de talla con frecuencia mayor que cero.
#' @export
#' @examples
#' max_range(frecuencia = c(0,0,1,2,3), tallas = c(5,6,7,8,9))
max_range <- function(frecuencia, tallas) {
  # Validación de parámetros
  if (!is.numeric(frecuencia)) stop("El parámetro 'frecuencia' debe ser numérico.")
  if (!is.numeric(tallas)) stop("El parámetro 'tallas' debe ser numérico.")

  if (length(frecuencia) != length(tallas)) {
    stop("Los vectores 'frecuencia' y 'tallas' deben tener la misma longitud.")
  }

  # Verificar si hay frecuencias positivas
  if (all(frecuencia <= 0, na.rm = TRUE) || all(is.na(frecuencia))) {
    warning("No hay frecuencias positivas. Se devolverá NA.")
    return(NA_real_)
  }

  frecuencia[frecuencia <= 0] <- NA
  return(max(tallas[!is.na(frecuencia)], na.rm = TRUE))
}



#' Conversión de número de individuos a peso
#'
#' Convierte frecuencias numéricas de tallas en un data.frame a estimaciones de peso,
#' usando la relación talla-peso.
#'
#' @param data Data frame donde las columnas con nombres iguales a las tallas contienen frecuencias.
#' @param tallas Vector de tallas (que deben coincidir con los nombres de columnas del `data`).
#' @param a Valor numérico del coeficiente de la relación longitud-peso.
#' @param b Valor numérico del exponente de la relación longitud-peso.
#' @return Data frame con las mismas dimensiones pero expresadas en peso.
#' @export
#' @importFrom stats setNames
#' @examples
#'
#' data(calas_bitacora)
#' data(faenas_bitacora)
#' data(tallas_bitacora)
#'
#' # Procesar datos
#' data_calas <- procesar_calas(data_calas = calas_bitacora)
#' data_faenas <- procesar_faenas(data_faenas = faenas_bitacora)
#' calas_tallas <- procesar_tallas(data_tallas = tallas_bitacora)
#'
#' # Integrar datos
#' data_tallasfaenas <- merge(x = data_faenas, y = calas_tallas, by = 'codigo_faena')
#' data_total <- merge_tallas_faenas_calas(data_calas = data_calas,
#'                                        data_tallas_faenas = data_tallasfaenas)
#' datos_final <- agregar_variables(data_total)
#'
#' # Definir columnas de tallas
#' tallas_cols <- c("8", "8.5", "9", "9.5", "10", "10.5", "11", "11.5",
#'                  "12", "12.5", "13", "13.5", "14", "14.5", "15")
#'
#' # Ponderar tallas
#' resultado <- ponderar_tallas_df(df = datos_final,
#'                                tallas_cols = tallas_cols,
#'                                captura_col = "catch_ANCHOVETA",
#'                                a = 0.0001,
#'                                b = 2.984)
#'
#'numero_a_peso(data = resultado, tallas = paste0('pond_', tallas_cols), a = 0.0012, b = 3.1242)
numero_a_peso <- function(data, tallas, a, b) {
  # Validación de parámetros
  if (!is.data.frame(data)) stop("El parámetro 'data' debe ser un data.frame.")
  if (!is.numeric(a)) stop("El parámetro 'a' debe ser numérico.")
  if (!is.numeric(b)) stop("El parámetro 'b' debe ser numérico.")

  if (is.character(tallas)) {
    tallas_num <- extraer_valores_tallas(tallas)
  }

  # Validación de que las tallas estén presentes en el data.frame
  tallas_presentes <- tallas %in% colnames(data)
  if (!all(tallas_presentes)) {
    tallas_faltantes <- tallas[!tallas_presentes]
    stop("Las siguientes tallas no están presentes como columnas en el data.frame: ",
         paste(tallas_faltantes, collapse = ", "))
  }

  # Verificar que las columnas de tallas contengan valores numéricos
  for (talla_col in tallas) {
    if (!is.numeric(data[[talla_col]])) {
      data[[talla_col]] <- as.numeric(data[[talla_col]])
      warning("La columna '", talla_col, "' ha sido convertida a numérica.")
    }
  }

  # Cálculo de pesos
  tryCatch({
    pesos <- as.data.frame(t(apply(data[, tallas, drop = FALSE], 1, function(x) {
      talla_peso(talla = tallas_num, a = a, b = b) * x
    })))

    # Renombrar columnas con prefijo "peso_"
    colnames(pesos) <- paste0("peso_", tallas)

    # Añadir las columnas de peso al data original
    resultado <- cbind(data, pesos)

    return(resultado)
  }, error = function(e) {
    stop("Error al calcular pesos: ", e$message)
  })
}





#' Cálculo de porcentaje de juveniles por grupos
#'
#' @description
#' Calcula el porcentaje de juveniles por grupos especificados, tanto en número como
#' en peso. Utiliza un enfoque moderno con dplyr para procesar los datos y calcular
#' las proporciones de juveniles basándose en frecuencias de tallas.
#'
#' @param data Data frame con datos de frecuencias de tallas.
#' @param group_cols Vector de nombres de columnas para agrupar los datos.
#' @param cols_tallas Vector de nombres o índices de columnas que contienen las
#'   frecuencias de tallas. Pueden ser nombres con patrones como "pond_X", "talla_X" o "X".
#' @param juvLim Talla límite para considerar juveniles (por defecto 12 cm).
#' @param a Coeficiente de la relación longitud-peso (por defecto 0.0012).
#' @param b Exponente de la relación longitud-peso (por defecto 3.1242).
#' @param remove_empty Lógico. Si es TRUE (por defecto), elimina los grupos
#'   sin datos (total_numero = 0).
#'
#' @return Data frame con los siguientes campos:
#'   \itemize{
#'     \item Columnas de agrupación especificadas en group_cols
#'     \item porc_juv_numero: Porcentaje de juveniles en número
#'     \item porc_juv_peso: Porcentaje de juveniles en peso
#'     \item total_numero: Total de individuos en el grupo
#'     \item total_peso: Peso total en el grupo
#'   }
#'
#' @export
#' @importFrom dplyr group_by_at summarize across everything filter pick
#' @importFrom tidyr unnest
#'
#' @examples
#' # Cargar datos de ejemplo
#' data(calas_bitacora)
#' data(faenas_bitacora)
#' data(tallas_bitacora)
#'
#' # Procesar datos
#' data_calas <- procesar_calas(data_calas = calas_bitacora)
#' data_faenas <- procesar_faenas(data_faenas = faenas_bitacora)
#' calas_tallas <- procesar_tallas(data_tallas = tallas_bitacora)
#'
#' # Integrar datos
#' data_tallasfaenas <- merge(x = data_faenas, y = calas_tallas, by = 'codigo_faena')
#' data_total <- merge_tallas_faenas_calas(data_calas = data_calas,
#'                                        data_tallas_faenas = data_tallasfaenas)
#' datos_final <- agregar_variables(data_total)
#'
#' # Definir columnas de tallas
#' tallas_cols <- c("8", "8.5", "9", "9.5", "10", "10.5", "11", "11.5",
#'                  "12", "12.5", "13", "13.5", "14", "14.5", "15")
#'
#' # Ponderar tallas
#' resultado <- ponderar_tallas_df(df = datos_final,
#'                                tallas_cols = tallas_cols,
#'                                captura_col = "catch_ANCHOVETA",
#'                                a = 0.0001,
#'                                b = 2.984)
#'
#' # Añadir columna de fecha para agrupar
#' resultado$fecha_unica <- convertir_a_fecha(resultado$fecha_inicio_cala, tipo = "date")
#'
#' # Calcular juveniles por fecha
#' resultado_por_fecha <- juveniles_por_grupo(
#'   data = resultado,
#'   group_cols = "fecha_unica",
#'   cols_tallas = paste0("pond_", tallas_cols),
#'   juvLim = 12,
#'   a = 0.0012,
#'   b = 3.1242
#' )
#'
#' # Calcular juveniles por fecha y distancia a costa
#' resultado_fecha_dc <- juveniles_por_grupo(
#'   data = resultado,
#'   group_cols = c("fecha_unica", "dc_cat"),
#'   cols_tallas = paste0("pond_", tallas_cols),
#'   juvLim = 12,
#'   a = 0.0012,
#'   b = 3.1242
#' )
#'
#' # Ver resultados
#' head(resultado_por_fecha)
juveniles_por_grupo <- function(data, group_cols, cols_tallas, juvLim = 12, a = 0.0012, b = 3.1242,
                                remove_empty = TRUE) {
  # Validación de parámetros
  if (!is.data.frame(data)) stop("El parámetro 'data' debe ser un data.frame.")
  if (!all(group_cols %in% colnames(data)))
    stop("No todas las columnas de agrupación están en el data.frame.")

  # Determinar si cols_tallas contiene nombres o índices
  if (is.numeric(cols_tallas)) {
    # Si son índices numéricos, obtener los nombres correspondientes
    if (any(cols_tallas > ncol(data) | cols_tallas < 1))
      stop("Alguno de los índices en cols_tallas está fuera del rango del data.frame.")

    # Convertir los índices a nombres para trabajar uniformemente
    cols_nombres <- names(data)[cols_tallas]
  } else {
    # Si ya son nombres, verificar que existan en el data.frame
    if (!all(cols_tallas %in% colnames(data)))
      stop("No todas las columnas de tallas están en el data.frame.")

    cols_nombres <- cols_tallas
  }

  # Asegurar que las columnas de tallas sean numéricas
  data <- data %>%
    dplyr::mutate(dplyr::across(all_of(cols_nombres), ~as.numeric(.x)))

  # Extraer valores numéricos de tallas a partir de nombres de columnas
  # Solo si los nombres parecen contener información de tallas (e.g., "pond_8.5", "8", "talla_9")
  if (all(grepl("^(pond_)?([0-9]+(\\.[0-9]+)?)$|^talla_[0-9]+(\\.[0-9]+)?$", cols_nombres))) {
    # Extraer valores numéricos eliminando prefijos comunes
    valores_tallas <- as.numeric(gsub("^(pond_|talla_)?", "", cols_nombres))
  } else if (is.numeric(cols_tallas)) {
    # Si los cols_tallas eran originalmente numéricos y no parecen ser patrones de tallas,
    # usar los valores originales
    valores_tallas <- cols_tallas
  } else {
    # En caso contrario, intentar convertir directamente los nombres a numéricos
    valores_tallas <- suppressWarnings(as.numeric(cols_nombres))

    # Si la conversión no funciona (generando NAs), usar números secuenciales
    if (anyNA(valores_tallas)) {
      warning("No se pudieron determinar valores numéricos de tallas a partir de los nombres de columnas. Usando secuencia 1:n.")
      valores_tallas <- seq_along(cols_nombres)
    }
  }

  # Función para procesar cada grupo usando calcular_juveniles
  procesar_grupo <- function(df) {
    # Comprobar si el dataframe es vacío o todas las frecuencias son cero
    if (nrow(df) == 0) {
      return(data.frame(
        porc_juv_numero = NA_real_,
        porc_juv_peso = NA_real_,
        total_numero = 0,
        total_peso = 0
      ))
    }

    # Extraer y sumar frecuencias por talla
    frecuencias <- colSums(df[, cols_nombres, drop = FALSE], na.rm = TRUE)

    # Verificar si todas las frecuencias son cero
    if (all(frecuencias == 0)) {
      return(data.frame(
        porc_juv_numero = NA_real_,
        porc_juv_peso = NA_real_,
        total_numero = 0,
        total_peso = 0
      ))
    }

    # Llamar a la función externa para calcular juveniles
    calcular_juveniles(frecuencias, valores_tallas, juvLim, a, b)
  }

  # Si no hay columnas de agrupación, calcular para todo el conjunto
  if (length(group_cols) == 0) {
    return(procesar_grupo(data))
  }

  # Agrupar y calcular - versión actualizada sin usar cur_data()
  resultados <- data %>%
    dplyr::group_by(dplyr::across(all_of(group_cols))) %>%
    dplyr::summarize(
      resultado = list(
        {
          # Usar pick() para seleccionar las columnas de tallas
          df_grupo <- dplyr::pick(all_of(cols_nombres))
          # Añadir las filas para completar el data.frame
          df_grupo <- as.data.frame(df_grupo)
          procesar_grupo(df_grupo)
        }
      ),
      .groups = "drop"
    ) %>%
    tidyr::unnest(resultado)

  # Opcionalmente eliminar grupos sin datos
  if (remove_empty && any(resultados$total_numero == 0, na.rm = TRUE)) {
    resultados <- resultados %>%
      dplyr::filter(total_numero > 0)
  }

  return(resultados)
}
