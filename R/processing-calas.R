#' Procesamiento de datos de calas
#'
#' @description
#' processing_calas() Función para procesar datos de calas pesqueras proveniente de las bitácoras de PRODUCE. Usa formato xlsx
#'
#' @param data_calas Un data frame con los datos de calas a procesar
#' @return Un data frame con los datos procesados
#' @export
#' @rdname processing_calas
#' @examples
#' data_calas <- processing_calas(data_calas = calas)
processing_calas <- function(data_calas) {
  data_calas <- data_calas %>%
    dplyr::select(3, 4, 5, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18)

  names(data_calas) <- c(
    "codigo_faena",
    "n_cala",
    "fecha_inicio",
    "fecha_fin",
    "latitud_inicio",
    "longitud_inicio",
    "latitud_fin",
    "longitud_fin",
    "tipo_arte",
    "descripcion",
    "catch",
    "estado",
    "origen_cala",
    "fecha_registro"
  )

  data_calas <- data_calas %>% dplyr::mutate(
    descripcion = stringi::stri_trim(descripcion),
    lat_inicial = dms_a_decimal(latitud_inicio),
    lon_inicial = dms_a_decimal(longitud_inicio),
    lat_final = dms_a_decimal(latitud_fin),
    lon_final = dms_a_decimal(longitud_fin)
  )


  return(data_calas)

}

#' @description
#' processing_calas_2() Función para procesar datos de calas pesqueras proveniente de las bitácoras de PRODUCE. Usa formato csv
#'
#' @param data_calas Un data frame con los datos de calas a procesar
#' @export
#' @rdname processing_calas
processing_calas_2 <- function(data_calas) {
  data_calas <- data_calas[, -1]

  names(data_calas) <- c(
    "codigo_faena",
    "n_cala",
    "fecha_inicio",
    "fecha_fin",
    "latitud_inicio",
    "longitud_inicio",
    "latitud_fin",
    "longitud_fin",
    "tipo_arte",
    "descripcion",
    "catch",
    "estado",
    "origen_cala",
    "fecha_registro"
  )

  data_calas <- data_calas %>% dplyr::mutate(
    descripcion = stringi::stri_trim(descripcion),
    lat_inicial = dms_a_decimal(latitud_inicio),
    lon_inicial = dms_a_decimal(longitud_inicio),
    lat_final = dms_a_decimal(latitud_fin),
    lon_final = dms_a_decimal(longitud_fin)
  )

  return(data_calas)

}
