#' Procesamiento de datos de calas pesqueras, archivo sitrapesca de PRODUCE
#'
#' @description
#' Procesa datos de calas pesqueras provenientes de las bitácoras de PRODUCE,
#' en formato CSV o XLSX. Devuelve un data frame limpio y estandarizado con
#' coordenadas convertidas a grados decimales.
#'
#' @param data_calas Data frame con los datos crudos de calas.
#' @param formato Formato del archivo de entrada: "xlsx" (por defecto) o "csv".
#' @param corregir_coordenadas Lógico. Si es TRUE (valor por defecto), se corrigen
#'   posibles errores en las coordenadas durante la conversión de grados, minutos y
#'   segundos a grados decimales.
#'
#' @return Un data frame con las siguientes columnas estandarizadas:
#'   \itemize{
#'     \item codigo_faena: Identificador único de la faena de pesca
#'     \item n_cala: Número de cala dentro de la faena
#'     \item fecha_inicio: Fecha y hora de inicio de la cala
#'     \item fecha_fin: Fecha y hora de fin de la cala
#'     \item latitud_inicio, longitud_inicio: Coordenadas iniciales en formato original
#'     \item latitud_fin, longitud_fin: Coordenadas finales en formato original
#'     \item tipo_arte: Tipo de arte de pesca utilizado
#'     \item descripcion: Descripción adicional de la cala
#'     \item catch: Captura total registrada
#'     \item estado: Estado de la cala
#'     \item origen_cala: Origen de los datos de la cala
#'     \item fecha_registro: Fecha de registro en el sistema
#'     \item lat_inicial, lon_inicial: Coordenadas iniciales en grados decimales
#'     \item lat_final, lon_final: Coordenadas finales en grados decimales
#'   }
#' @export
#' @importFrom dplyr select mutate %>%
#' @importFrom stringi stri_trim
#'
#' @examples
#' # Cargar datos de ejemplo
#'
#' # Procesar datos con configuración predeterminada (corrigiendo coordenadas)
#' calas_procesadas <- procesar_calas(
#'   data_calas = calas_bitacora,
#'   formato = "xlsx"
#' )
#'
#' # Procesar datos sin corregir posibles errores en coordenadas
#' calas_sin_correccion <- procesar_calas(
#'   data_calas = calas_bitacora,
#'   formato = "xlsx",
#'   corregir_coordenadas = FALSE
#' )
#'
#' # Ver las primeras filas del resultado
#' head(calas_procesadas)
procesar_calas <- function(data_calas, formato = "xlsx", corregir_coordenadas = TRUE) {
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
      lat_inicial = dms_a_decimal(latitud_inicio, corregir_errores = corregir_coordenadas),
      lon_inicial = dms_a_decimal(longitud_inicio, hemisferio = "W", corregir_errores = corregir_coordenadas),
      lat_final = dms_a_decimal(latitud_fin, corregir_errores = corregir_coordenadas),
      lon_final = dms_a_decimal(longitud_fin, hemisferio = "W", corregir_errores = corregir_coordenadas)
    )
  return(data_calas)
}
