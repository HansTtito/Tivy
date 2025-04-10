#' Procesamiento de datos de tallas de las calas
#' @description
#' processing_tallas() Función para procesar datos de tallas de las calas pesqueras proveniente de las bitácoras de PRODUCE. Usa formato xlsx
#'
#' @param data_tallas Un data frame con los datos de tallas de las calas a procesar
#' @return Un data frame con los datos procesados
#' @export
#' @rdname processing_tallas
#' @examples
#' data_tallas_calas <- processing_tallas(data_tallas = faenas)
processing_tallas = function(data_tallas) {
  data_tallas <- data_tallas %>% dplyr::select(3, 4, 5, 8, 10)

  names(data_tallas) <- c("codigo_faena", "n_cala", "descripcion", "talla", "freq")

  data_tallas = data_tallas %>% dplyr::mutate(descripcion = str_trim(descripcion))

  data_tallas <- data_tallas %>% tidyr::spread(talla, freq)

  return(data_tallas)

}


#' @description
#' processing_tallas_2() Función para procesar datos de tallas de las calas pesqueras proveniente de las bitácoras de PRODUCE. Usa formato csv
#'
#' @param data_tallas Un data frame con los datos de tallas de las calas a procesar
#' @export
#' @rdname processing_tallas
processing_tallas_2 = function(data_tallas) {
  data_tallas = data_tallas[, -c(1, 6)]

  names(data_tallas) <- c("codigo_faena", "n_cala", "descripcion", "talla", "freq")

  data_tallas = data_tallas %>%
    dplyr::mutate(
      talla = as.numeric(talla),
      freq = as.numeric(freq),
      descripcion = str_trim(descripcion)
    )

  data_tallas <- data_tallas %>% tidyr::spread(talla, freq)

  return(data_tallas)

}

