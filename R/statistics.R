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
  if (length(frecuencia) != length(tallas)) {
    stop("Los vectores 'frecuencia' y 'tallas' deben tener la misma longitud.")
  }

  if (sum(frecuencia, na.rm = TRUE) != 0) {
    peso <- talla_peso(talla = tallas, a = a, b = b) * frecuencia
    talla_ponderada <- (captura / sum(peso, na.rm = TRUE)) * frecuencia
  } else {
    talla_ponderada <- rep(0, length(tallas))
  }

  return(talla_ponderada)
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
  juv <- 100 * (sum(frecuencia[tallas < juvLim], na.rm = TRUE) / sum(frecuencia, na.rm = TRUE))
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
  frecuencia[frecuencia == 0] <- NA
  return(min(tallas[!is.na(frecuencia)]))
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
  frecuencia[frecuencia == 0] <- NA
  return(max(tallas[!is.na(frecuencia)]))
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
#' @examples
#' data <- data.frame(id = 1:2, `8` = c(3,2), `9` = c(5,4), `10` = c(2,3))
#' tallas <- c(8,9,10)
#' numero_a_peso(data, tallas, a = 0.0012, b = 2.984)
numero_a_peso <- function(data, tallas, a, b) {
  # Validación de columnas
  if (!all(as.character(tallas) %in% colnames(data))) {
    stop("Algunas tallas no están presentes como columnas en el data.frame.")
  }

  peso <- as.data.frame(t(apply(data[, as.character(tallas)], 1 , function(x) {
    talla_peso(talla = tallas, a = a, b = b) * x
  })))

  id <- setdiff(names(data), as.character(tallas))
  peso <- cbind(data[, id, drop = FALSE], peso)
  return(peso)
}
