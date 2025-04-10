#' Relación Talla - Peso
#' @description
#' Función que permite estimar el Peso en función de la talla
#'
#' @param talla Un vector con las tallas
#' @param a Un valor numérico de la relación longitud - peso
#' @param b Un valor numérico de la relación longitud - peso
#' @return Un vector con los pesos estimados
#' @export
#' @rdname Length_weight
#' @examples
#' tallas <- seq(from = 5, to = 20, by = 0.5)
#' a <- 0.0001
#' b <- 2.984
#' pesos <- talla_peso(talla = tallas, a = a, b = b)
talla_peso <- function(talla , a , b) {
  w = a * (talla^b)
  return(w)
}


#' Ponderación de la talla la captura
#' @description
#' Función para ponderar las tallas en función de la captura.
#'
#' @param frecuencia Un vector con las frecuencias de las tallas muestreadas en la captura
#' @param captura Un valor numérico que representa la captura
#' @param tallas Un vector numérico que representa las tallas muestreadas
#' @param a Un valor numérico de la relación longitud - peso
#' @param b Un valor numérico de la relación longitud - peso
#' @return Un vector con las tallas ponderadas a la captura
#' @export
#' @rdname ponderacion
#' @examples
#' frecuencia_tallas <- c(0, 0, 0, 0, 0, 0, 0, 0, 1, 4, 8, 16, 12, 23, 34, 55, 35, 24, 15, 10, 6, 3, 2, 0, 0, 0, 0, 0, 0, 0, 0)
#' captura <- 100
#' tallas <- seq(from = 5, to = 20, by = 0.5)
#' a <- 0.0001
#' b <- 2.984
#' tallas_ponderadas <- ponderacion(frecuencia = frecuencia_tallas, captura = captura, tallas = tallas, a = a, b = b)
ponderacion <- function(frecuencia, captura, tallas, a, b) {
  if (sum(frecuencia, na.rm = TRUE) != 0) {
    peso <- talla_peso(Length = tallas, a = a, b = b) * frecuencia
    talla_ponderada <- (captura / sum(peso, na.rm = TRUE)) * frecuencia

  } else {
    talla_ponderada <- rep(0, length(tallas))

  }

  return(talla_ponderada)

}


#' Porcentaje de Juveniles
#' @description
#' Función para estimar el porcentaje de juveniles
#'
#' @param frecuencia Un vector con las frecuencias de las tallas muestreadas en la captura
#' @param tallas Un vector numérico que representa las tallas muestreadas
#' @param juvLim Un valor numérico que representa la talla límite para considerar un individuo como juvenil
#' @return El porcentaje de juveniles estimado
#' @export
#' @rdname porc_juveniles
#' @examples
#' frecuencia <- c(0, 0, 0, 0, 0, 0, 0, 0, 1, 4, 8, 16, 12, 23, 34, 55, 35, 24, 15, 10, 6, 3, 2, 0, 0, 0, 0, 0, 0, 0, 0)
#' tallas <- seq(from = 5, to = 20, by = 0.5)
#' juveniles <- porc_juveniles(frecuencia = frecuencia, tallas = tallas, juvLim = 12)
porc_juveniles <- function(frecuencia, tallas, juvLim = 12) {
  juv <- 100 * (sum(frecuencia[tallas < juvLim], na.rm = TRUE) / sum(frecuencia, na.rm = TRUE))

  return(juv)

}


#' Máximo y Mínimo de la talla
#' @description
#' Funciones para determinar el máximo y mínimo de la talla
#'
#' @param frecuencia Un vector con las frecuencias de las tallas muestreadas en la captura
#' @param tallas Un vector numérico que representa las tallas muestreadas
#' @return La talla mínima muestreada
#' @export
#' @rdname max_min_functions
#' @examples
#' frecuencia <- c(0, 0, 0, 0, 0, 0, 0, 0, 1, 4, 8, 16, 12, 23, 34, 55, 35, 24, 15, 10, 6, 3, 2, 0, 0, 0, 0, 0, 0, 0, 0)
#' tallas <- seq(from = 5, to = 20, by = 0.5)
#' min_range(frecuencia = frecuencia, tallas = tallas)
min_range <- function(frecuencia, tallas) {
  data[data == 0] <- NA
  min_v <- min(as.numeric(tallas[!is.na(data)]))

  return(min_v)

}


#'
#' @param frecuencia Un vector con las frecuencias de las tallas muestreadas en la captura
#' @param tallas Un vector numérico que representa las tallas muestreadas
#' @export
#' @rdname max_min_functions
#' @examples
#' max_range(frecuencia = frecuencia, tallas = tallas)
max_range <- function(frecuencia, tallas) {
  data[data == 0] <- NA
  max_v <- max(as.numeric(tallas[!is.na(data)]))

  return(max_v)

}



#' numero_a_peso
#' @description
#' Función para convertir las tallas en número a peso
#'
#' @param data Un data frame con las frecuencias de las tallas en número
#' @param tallas Un vector numérico que representa las tallas muestreadas
#' @param a Un valor numérico de la relación longitud - peso
#' @param b Un valor numérico de la relación longitud - peso
#' @return El porcentaje de juveniles estimado
#' @export
#' @rdname numero_a_peso
#' @examples
#' data_tallas <- processing_tallas(data_tallas = tallas)
#' tallas <- seq(from = 8, to = 15, by = 0.5)
#' a <- 0.0012
#' b <- 2.984
#' data_peso <- numero_a_peso(data = data_tallas, tallas = tallas, a = a, b = b)
numero_a_peso <- function(data, tallas, a, b) {
  peso <- as.data.frame(t(apply(data[, as.character(tallas)], 1 , function(x)
    talla_peso(talla = tallas, a = a, b = b) * x)))
  id <- setdiff(names(data), as.character(tallas))
  peso <- cbind(data[, id], peso)
  return(peso)

}
