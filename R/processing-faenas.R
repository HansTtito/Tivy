#' Procesamiento de datos de faenas pesqueras, archivo sitrapesca de PRODUCE
#'
#' @description
#' Procesa datos de faenas pesqueras provenientes de las bitácoras de PRODUCE,
#' en formato CSV o XLSX. Devuelve un data frame limpio y estandarizado.
#'
#' @param data_faenas Data frame con los datos crudos de faenas.
#' @param formato Formato del archivo de entrada: "xlsx" (por defecto) o "csv".
#'
#' @return Un data frame con columnas estandarizadas: codigo_faena, embarcacion, armador, matricula, fecha_inicio (opcional), fecha_fin (opcional).
#' @export
#' @importFrom dplyr select %>%
#'
#' @examples
#'
#' data(faenas_bitacora)
#'
#' faenas = procesar_faenas(data_faenas = faenas_bitacora, formato = "xlsx")
#'
#' print(head(faenas))
procesar_faenas <- function(data_faenas, formato = "xlsx") {
  if (!formato %in% c("xlsx", "csv")) {
    stop("El parámetro 'formato' debe ser 'xlsx' o 'csv'.")
  }
  if (formato == "xlsx") {
    if (ncol(data_faenas) < 11) stop("Se esperan al menos 11 columnas en archivos XLSX.")
    data_faenas <- data_faenas %>% dplyr::select(11, 4, 3, 7, 9, 10)
    names(data_faenas) <- c("codigo_faena", "embarcacion", "armador", "matricula", "fecha_inicio", "fecha_fin")
  } else if (formato == "csv") {
    if (ncol(data_faenas) < 8) stop("Se esperan al menos 8 columnas en archivos CSV.")
    data_faenas <- data_faenas %>% dplyr::select(8, 3, 2, 4)
    names(data_faenas) <- c("codigo_faena", "embarcacion", "armador", "matricula")
  }
  return(data_faenas)
}
