#' Procesamiento de datos de calas pesqueras, archivo sitrapesca de PRODUCE
#'
#' @description
#' Procesa datos de calas pesqueras provenientes de las bitácoras de PRODUCE,
#' en formato CSV o XLSX. Devuelve un data frame limpio y estandarizado.
#'
#' @param data_calas Data frame con los datos crudos de calas.
#' @param formato Formato del archivo de entrada: "xlsx" (por defecto) o "csv".
#'
#' @return Un data frame con columnas estandarizadas y coordenadas en grados decimales.
#' @export
#' @importFrom dplyr select mutate %>%
#' @importFrom stringi stri_trim
#'
#' @examples
#' procesar_calas(data_calas = calas, formato = "xlsx")
procesar_calas <- function(data_calas, formato = "xlsx") {
  if (!formato %in% c("xlsx", "csv")) {
    stop("El parámetro 'formato' debe ser 'xlsx' o 'csv'.")
  }
  if (formato == "xlsx") {
    if (ncol(data_calas) < 18) stop("Se esperan al menos 18 columnas en archivos XLSX.")
    data_calas <- data_calas %>%
      dplyr::select(3, 4, 5, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18)
  } else if (formato == "csv") {
    data_calas <- data_calas[, -1]  # Eliminar primera columna extra
  }
  names(data_calas) <- c(
    "codigo_faena", "n_cala", "fecha_inicio", "fecha_fin",
    "latitud_inicio", "longitud_inicio", "latitud_fin", "longitud_fin",
    "tipo_arte", "descripcion", "catch", "estado", "origen_cala", "fecha_registro"
  )
  data_calas <- data_calas %>%
    dplyr::mutate(
      descripcion = stringi::stri_trim(descripcion),
      lat_inicial = Tivy::dms_a_decimal(latitud_inicio),
      lon_inicial = Tivy::dms_a_decimal(longitud_inicio),
      lat_final = Tivy::dms_a_decimal(latitud_fin),
      lon_final = Tivy::dms_a_decimal(longitud_fin)
    )
  return(data_calas)
}
