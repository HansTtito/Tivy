#' Mergeando datos de faenas, tallas y calas
#'
#' @description
#' merge_tallas_faenas_calas() Función para juntar los archivos de faenas, tallas y calas
#'
#' @param data_calas Un data frame obtenido de processing_calas()
#' @param data_tallas_faenas Un data frame con los datos de tallas de las faenas a procesar
#' @return Un data frame con los datos consolidados de faenas, tallas y calas
#' @export
#' @rdname merge_tallas_faenas_calas
#' @examples
#' data_total <- merge_tallas_faenas_calas(data_calas = data_calas, data_tallas_faenas = data_tallas_faenas)
merge_tallas_faenas_calas = function(data_calas, data_tallas_faenas) {
  tallas = grep(
    pattern = "[1-9]",
    x = names(data_tallas_faenas),
    value = TRUE
  )

  catch_sps = data_calas %>%
    dplyr::filter(!is.na(descripcion), descripcion != "") %>%
    dplyr::mutate(catch = as.numeric(catch)) %>%
    dplyr::group_by(codigo_faena, n_cala, descripcion) %>%
    dplyr::reframe(catch = sum(catch)) %>%
    tidyr::spread(descripcion, catch, sep = "_")

  names(catch_sps) = gsub(pattern = "descripcion",
                          replacement = "catch",
                          x = names(catch_sps))

  min_sps = data_tallas_faenas %>%
    dplyr::filter(!is.na(descripcion), descripcion != "") %>%
    dplyr::mutate(min_rango = apply(data_tallas_faenas[tallas], 1, min_range)) %>%
    tidyr::spread(descripcion, min_rango, sep = "_") %>%
    dplyr::select("codigo_faena",
                  "n_cala",
                  dplyr::starts_with("descripcion_"))


  names(min_sps) = gsub(pattern = "descripcion",
                        replacement = "min",
                        x = names(min_sps))

  max_sps = data_tallas_faenas %>%
    dplyr::mutate(max_rango = apply(data_tallas_faenas[tallas], 1, max_range)) %>%
    tidyr::spread(descripcion, max_rango, sep = "_") %>%
    dplyr::select("codigo_faena",
                  "n_cala",
                  dplyr::starts_with("descripcion_"))

  names(max_sps) = gsub(pattern = "descripcion",
                        replacement = "max",
                        x = names(max_sps))

  min_max_sps = merge(min_sps,
                      max_sps,
                      by = c("codigo_faena", "n_cala"),
                      all = TRUE)

  tallas_total = merge(
    data_tallas_faenas,
    min_max_sps,
    by = c("codigo_faena", "n_cala"),
    all = TRUE
  )

  total_data  = merge(catch_sps,
                      tallas_total,
                      by = c("codigo_faena", "n_cala"),
                      all = TRUE)

  final_data = merge(
    data_calas %>% dplyr::select(-grep(
      pattern = "catch", x = names(data_calas)
    )),
    total_data,
    by = c("codigo_faena", "n_cala", "descripcion"),
    all = TRUE
  )

  final_data = final_data[!duplicated(final_data[, c("codigo_faena", "n_cala")]), ]

  return(final_data)

}


