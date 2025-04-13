#' Procesamiento de datos de tallas de las calas, archivo sitrapesca de PRODUCE
#'
#' @description
#' Procesa datos de tallas provenientes de las bitácoras de PRODUCE en formato CSV o XLSX.
#' Limpia las columnas relevantes y transforma el formato long a formato wide (filas por cala, columnas por talla).
#'
#' @param data_tallas Data frame con los datos de tallas.
#' @param formato Formato de entrada: "xlsx" (por defecto) o "csv".
#'
#' @return Un data frame con tallas por cala (formato wide).
#' @export
#'
#' @examples
#' procesar_tallas(data_tallas = tallas_bitacora, formato = "xlsx")
#'
#' @importFrom dplyr select mutate %>%
#' @importFrom stringr str_trim
#' @importFrom tidyr pivot_wider
procesar_tallas <- function(data_tallas, formato = "xlsx") {
  if (!formato %in% c("xlsx", "csv")) {
    stop("El parámetro 'formato' debe ser 'xlsx' o 'csv'.")
  }
  # Seleccionar columnas según formato
  if (formato == "xlsx") {
    if (ncol(data_tallas) < 10) stop("Se esperan al menos 10 columnas en archivos XLSX.")
    data_tallas <- data_tallas %>% dplyr::select(3, 4, 5, 8, 10)
  } else if (formato == "csv") {
    if (ncol(data_tallas) < 6) stop("Se esperan al menos 6 columnas en archivos CSV.")
    data_tallas <- data_tallas[, -c(1, 6)]
  }
  # Asignar nombres
  names(data_tallas) <- c("codigo_faena", "n_cala", "descripcion", "talla", "freq")
  # Limpieza y conversión
  data_tallas <- data_tallas %>%
    dplyr::mutate(
      descripcion = stringr::str_trim(descripcion),
      talla = suppressWarnings(as.numeric(talla)),
      freq = suppressWarnings(as.numeric(freq))
    )
  # Guardar orden deseado de las tallas
  orden_tallas <- sort(unique(na.omit(data_tallas$talla)))
  # Transformar a formato wide
  data_tallas <- tidyr::pivot_wider(
    data_tallas,
    names_from = talla,
    values_from = freq,
    values_fill = list(freq = 0)
  )
  # Reordenar columnas de tallas
  columnas_fijas <- c("codigo_faena", "n_cala", "descripcion")
  columnas_tallas_ordenadas <- as.character(orden_tallas)
  data_tallas <- data_tallas[, c(columnas_fijas, columnas_tallas_ordenadas)]
  return(data_tallas)
}
