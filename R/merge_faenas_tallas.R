#' Mergeando datos de faenas, tallas y calas
#'
#' @description
#' `merge_tallas_faenas_calas()` es una función para unir los datos de faenas, tallas y calas, combinando las capturas por especie,
#' los rangos de tallas (mínimo y máximo) y la información espacial y temporal de cada cala.
#'
#' @param data_calas Un data frame procesado con `processing_calas()` o `processing_calas_2()`.
#' @param data_tallas_faenas Un data frame con los datos de tallas por faena y cala.
#'
#' @return Un data frame con los datos consolidados de faenas, tallas y calas.
#' @export
#'
#' @examples
#' data_calas <- procesar_calas(data_calas = calas, formato = "xlsx")
#' data_faenas <- procesar_faenas(data_faenas = faenas, formato = "xlsx")
#' data_tallas <- procesar_tallas(data_tallas = tallas, formato = "xlsx")
#' data_tallas_faenas <- merge(x = data_tallas, y = data_faenas, by = "codigo_faena", all = TRUE)
#' data_total <- merge_tallas_faenas_calas(data_calas = data_calas, data_tallas_faenas = data_tallas_faenas)
merge_tallas_faenas_calas <- function(data_calas, data_tallas_faenas) {
  # Renombrar fechas en calas si existen
  data_calas <- data_calas %>%
    dplyr::rename_with(~ gsub("fecha_inicio", "fecha_inicio_cala", .x), .cols = dplyr::matches("fecha_inicio")) %>%
    dplyr::rename_with(~ gsub("fecha_fin", "fecha_fin_cala", .x), .cols = dplyr::matches("fecha_fin"))

  # Renombrar fechas en faenas si existen
  data_tallas_faenas <- data_tallas_faenas %>%
    dplyr::rename_with(~ gsub("fecha_inicio", "fecha_inicio_faena", .x), .cols = dplyr::matches("fecha_inicio")) %>%
    dplyr::rename_with(~ gsub("fecha_fin", "fecha_fin_faena", .x), .cols = dplyr::matches("fecha_fin"))

  # Identificar columnas de tallas
  tallas <- grep(pattern = "^[1-9]", x = names(data_tallas_faenas), value = TRUE)

  if (length(tallas) == 0) {
    stop("No se encontraron columnas de tallas en 'data_tallas_faenas'.")
  }

  # ---- CAPTURAS POR ESPECIE ----
  catch_sps <- data_calas %>%
    dplyr::filter(!is.na(descripcion), descripcion != "") %>%
    dplyr::mutate(catch = as.numeric(catch)) %>%
    dplyr::group_by(codigo_faena, n_cala, descripcion) %>%
    dplyr::summarise(catch = sum(catch, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = descripcion, values_from = catch, names_prefix = "catch_")

  # ---- RANGOS MÍNIMOS Y MÁXIMOS ----
  data_tallas_faenas <- data_tallas_faenas %>%
    dplyr::filter(!is.na(descripcion), descripcion != "") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      min_rango = min_range(c_across(all_of(tallas)), as.numeric(tallas)),
      max_rango = max_range(c_across(all_of(tallas)), as.numeric(tallas))
    ) %>%
    dplyr::ungroup()

  min_sps <- data_tallas_faenas %>%
    dplyr::select(codigo_faena, n_cala, descripcion, min_rango) %>%
    tidyr::pivot_wider(names_from = descripcion, values_from = min_rango, names_prefix = "min_")

  max_sps <- data_tallas_faenas %>%
    dplyr::select(codigo_faena, n_cala, descripcion, max_rango) %>%
    tidyr::pivot_wider(names_from = descripcion, values_from = max_rango, names_prefix = "max_")

  min_max_sps <- merge(min_sps, max_sps, by = c("codigo_faena", "n_cala"), all = TRUE)

  # ---- UNIÓN DE TALLAS Y RANGOS ----
  tallas_total <- merge(
    data_tallas_faenas,
    min_max_sps,
    by = c("codigo_faena", "n_cala"),
    all = TRUE
  )

  # ---- UNIÓN DE TODO CON CALAS ----
  total_data <- merge(catch_sps, tallas_total, by = c("codigo_faena", "n_cala"), all = TRUE)

  final_data <- merge(
    data_calas %>% dplyr::select(-dplyr::matches("catch")),
    total_data,
    by = c("codigo_faena", "n_cala", "descripcion"),
    all = TRUE
  )

  # Eliminar duplicados si existen
  final_data <- final_data[!duplicated(final_data[, c("codigo_faena", "n_cala", "descripcion")]), ]

  return(final_data)
}
