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
#' tallas <- seq(5, 20, by = 0.5)
#' a <- 0.0001
#' b <- 2.984
#' pesos <- talla_peso(talla = tallas, a = a, b = b)
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
#' frecuencia <- c(0, 1, 4, 8, 16, 12, 23, 34, 55, 35, 24, 15, 10, 6, 3, 2)
#' tallas <- seq(5, 20, by = 1)
#' captura <- 1000
#' a <- 0.0001
#' b <- 2.984
#' ponderadas <- ponderacion(frecuencia, captura, tallas, a, b)
ponderacion <- function(frecuencia, captura, tallas, a, b) {
  # Validación de parámetros
  if (!is.numeric(frecuencia)) stop("El parámetro 'frecuencia' debe ser numérico.")
  if (!is.numeric(tallas)) stop("El parámetro 'tallas' debe ser numérico.")
  if (!is.numeric(captura)) stop("El parámetro 'captura' debe ser numérico.")
  if (!is.numeric(a)) stop("El parámetro 'a' debe ser numérico.")
  if (!is.numeric(b)) stop("El parámetro 'b' debe ser numérico.")
  if (length(frecuencia) != length(tallas)) {
    stop("Los vectores 'frecuencia' y 'tallas' deben tener la misma longitud.")
  }

  # Manejar NAs en captura
  if (is.na(captura) || captura <= 0) {
    warning("El valor de 'captura' es NA o <= 0, se utilizará captura = 1 para los cálculos.")
    captura <- 1
  }

  if (any(tallas <= 0, na.rm = TRUE)) warning("Se detectaron valores de talla <= 0, que podrían producir resultados no válidos.")
  if (sum(frecuencia, na.rm = TRUE) == 0) {
    warning("La suma de frecuencias es cero, se devolverá un vector de ceros.")
    return(rep(0, length(tallas)))
  }

  # Reemplazar NAs en frecuencia con ceros
  frecuencia[is.na(frecuencia)] <- 0

  peso <- talla_peso(talla = tallas, a = a, b = b) * frecuencia
  suma_peso <- sum(peso, na.rm = TRUE)
  if (suma_peso == 0) {
    warning("La suma de pesos es cero, se devolverá un vector de ceros.")
    return(rep(0, length(tallas)))
  }
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
#' data_calas <- procesar_calas(data_calas = calas_bitacora)
#' data_faenas <- procesar_faenas(data_faenas = faenas_bitacora)
#' calas_tallas <- procesar_tallas(data_tallas = tallas_bitacora)
#'
#' data_tallasfaenas <- merge(x = data_faenas, y = calas_tallas, by = 'codigo_faena')
#'
#' data_total <- merge_tallas_faenas_calas(data_calas = data_calas, data_tallas_faenas = data_tallasfaenas)
#'
#' tallas_columnas <- c("8", "8.5", "9", "9.5", "10", "10.5", "11", "11.5","12", "12.5", "13", "13.5", "14", "14.5", "15")
#' # Procesamiento secuencial
#' resultado <- ponderar_tallas_df(df = data_total, tallas_cols = tallas_columnas, captura_col = "catch_ANCHOVETA", a= 0.0001, b = 2.984)
#'
#' # Procesamiento paralelo para conjuntos grandes
#' resultado_paralelo <- ponderar_tallas_df(
#'   df = data_total, tallas_cols = tallas_columnas, captura_col = "catch_ANCHOVETA", a = 0.0001, b = 2.984, paralelo = TRUE
#' )
#'
#' @import parallel
#' @export
ponderar_tallas_df <- function(df, tallas_cols, captura_col, a, b,
                               paralelo = FALSE, num_cores = NULL, tam_bloque = 10000) {
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
        Tivy:::procesar_bloque(bloque, tallas_cols, captura_col, a, b)
      },
      future.seed = TRUE
    )

    # Combinar resultados
    resultado_final <- do.call(rbind, resultados)

    return(resultado_final)
  } else {
    # Procesamiento secuencial
    return(Tivy:::procesar_bloque(df, tallas_cols, captura_col, a, b))
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
#' frecuencia <- c(0, 1, 4, 8, 16, 12, 23, 34, 55, 35, 24, 15, 10, 6, 3, 2)
#' tallas <- seq(5, 20, by = 1)
#' porc <- porc_juveniles(frecuencia, tallas, juvLim = 12)
porc_juveniles <- function(frecuencia, tallas, juvLim = 12) {
  # Validación de parámetros
  if (!is.numeric(frecuencia)) stop("El parámetro 'frecuencia' debe ser numérico.")
  if (!is.numeric(tallas)) stop("El parámetro 'tallas' debe ser numérico.")
  if (!is.numeric(juvLim)) stop("El parámetro 'juvLim' debe ser numérico.")

  if (length(frecuencia) != length(tallas)) {
    stop("Los vectores 'frecuencia' y 'tallas' deben tener la misma longitud.")
  }

  if (juvLim <= 0) warning("El valor de 'juvLim' es <= 0, lo que podría no ser biológicamente plausible.")

  total_frecuencia <- sum(frecuencia, na.rm = TRUE)

  if (total_frecuencia == 0) {
    warning("La suma de frecuencias es cero. Se devolverá NA.")
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
#' min_range(c(0,0,1,2,3), c(5,6,7,8,9))
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
#' max_range(c(0,0,1,2,3), c(5,6,7,8,9))
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
#' data <- data.frame(id = 1:2, `8` = c(3,2), `9` = c(5,4), `10` = c(2,3))
#' tallas <- c(8,9,10)
#' numero_a_peso(data, tallas, a = 0.0012, b = 2.984)
numero_a_peso <- function(data, tallas, a, b) {
  # Validación de parámetros
  if (!is.data.frame(data)) stop("El parámetro 'data' debe ser un data.frame.")
  if (!is.numeric(tallas)) stop("El parámetro 'tallas' debe ser numérico.")
  if (!is.numeric(a)) stop("El parámetro 'a' debe ser numérico.")
  if (!is.numeric(b)) stop("El parámetro 'b' debe ser numérico.")

  # Validación de que las tallas estén presentes en el data.frame
  tallas_char <- as.character(tallas)
  tallas_presentes <- tallas_char %in% colnames(data)

  if (!all(tallas_presentes)) {
    tallas_faltantes <- tallas_char[!tallas_presentes]
    stop("Las siguientes tallas no están presentes como columnas en el data.frame: ",
         paste(tallas_faltantes, collapse = ", "))
  }

  # Verificar que las columnas de tallas contengan valores numéricos
  for (talla_col in tallas_char) {
    if (!is.numeric(data[[talla_col]])) {
      data[[talla_col]] <- as.numeric(data[[talla_col]])
      warning("La columna '", talla_col, "' ha sido convertida a numérica.")
    }
  }

  # Cálculo de pesos
  tryCatch({
    peso <- as.data.frame(t(apply(data[, tallas_char, drop = FALSE], 1, function(x) {
      talla_peso(talla = tallas, a = a, b = b) * x
    })))

    # Identificar columnas no numéricas (descriptivas)
    id <- setdiff(names(data), tallas_char)

    # Combinar columnas descriptivas con columnas de peso
    if (length(id) > 0) {
      peso <- cbind(data[, id, drop = FALSE], peso)
    }

    return(peso)
  }, error = function(e) {
    stop("Error al calcular pesos: ", e$message)
  })
}
