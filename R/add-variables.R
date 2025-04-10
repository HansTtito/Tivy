#' agregando variables a la base de datos
#'
#' @description
#' agregar_variables() Función que permite agregar variables como la distancia a la costa, la cantidad de juveniles y la cantidad de muestras
#'
#' @param data Un data frame obtenido de merge_tallas_faenas_calas()
#' @return Un data frame con los datos consolidados de faenas, tallas y calas
#' @export
#' @rdname agregar_variables
#' @examples
#' data_total <- agregar_variables(data = data_total)
agregar_variables = function(data) {
  # data$n = 1:nrow(data)

  tallas = grep(pattern = "[1-9]",
                x = names(data),
                value = TRUE)

  data$juv = apply(data[, tallas],
                   1,
                   porc_juveniles,
                   tallas = as.numeric(tallas),
                   juvLim = 12)

  data$muestra = apply(data[, tallas], 1, sum, na.rm = TRUE)

  data$dc = distancia_costa_vectorizado(lon = data$lon_inicial, lat = data$lat_inicial)

  data = data %>%
    dplyr::mutate(
      dc_cat = dplyr::case_when(
        dc >= 5  & dc < 15 ~ "05-15 mn",
        dc >= 15 & dc < 30 ~ "15-30 mn",
        dc >= 30 & dc < 50 ~ "30-50 mn",
        dc >= 50 & dc < 100 ~ "50-100 mn"
      )
    )

  return(data)

}
