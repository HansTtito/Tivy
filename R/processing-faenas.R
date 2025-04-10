#' Procesamiento de datos de faenas
#'
#' @description
#' processing_faenas() Función para procesar datos de faenas pesqueras proveniente de las bitácoras de PRODUCE. Usa formato xlsx
#'
#' @param data_faenas Un data frame con los datos de faenas a procesar
#' @return Un data frame con los datos procesados
#' @export
#' @rdname processing_faenas
#' @examples
#' data_faenas <- processing_faenas(data_faenas = faenas)
processing_faenas = function(data_faenas) {
  data_faenas <- data_faenas %>% dplyr::select(11, 4, 3, 7, 9, 10)

  names(data_faenas) <- c("codigo_faena", "embarcacion", "armador", "matricula", 'fecha_inicio', 'fecha_fin')

  return(data_faenas)

}

#' @description
#' processing_faenas_2() Función para procesar datos de faenas pesqueras proveniente de las bitácoras de PRODUCE. Usa formato csv
#'
#' @param data_faenas Un data frame con los datos de faenas a procesar
#' @export
#' @rdname processing_faenas
processing_faenas_2 = function(data_faenas) {
  data_faenas <- data_faenas %>% dplyr::select(8, 3, 2, 4, 6, 7)

  names(data_faenas) <- c("codigo_faena", "embarcacion", "armador", "matricula")

  return(data_faenas)

}
