#' Agrega variables de juveniles, tama<c3><b1>o de muestra, distancia a la costa y su categor<c3><ad>a
#'
#' Esta funci<c3><b3>n agrega nuevas variables a un conjunto de datos, incluyendo la proporci<c3><b3>n de juveniles,
#' el total de individuos en la muestra, la distancia a la costa y la categor<c3><ad>a de distancia a la costa.
#'
#' @param data Un data frame que debe contener las coordenadas de latitud (`lat_inicial`) y longitud (`lon_inicial`),
#' as<c3><ad> como columnas con los tama<c3><b1>os de los individuos.
#' @param JuvLim L<c3><ad>mite de talla para considerar juveniles (default = 12). Si la talla es menor que este valor,
#' el individuo se considera juvenil.
#' @param tipo_distancia Tipo de c<c3><a1>lculo de distancia a la costa (opciones como "haversine", etc.),
#' por defecto es "haversine".
#' @param ventana Ventana para suavizar la l<c3><ad>nea de costa, el valor por defecto es 0.5.
#' @param unidad Unidad de distancia utilizada para la medici<c3><b3>n de la distancia a la costa
#' ("mn", "km", etc.), por defecto es "mn".
#'
#' @return Un data frame con las siguientes nuevas variables:
#' \itemize{
#'   \item `juv`: La proporci<c3><b3>n de juveniles en cada fila.
#'   \item `muestra`: El total de individuos en la muestra.
#'   \item `dc`: La distancia a la costa desde las coordenadas de latitud y longitud proporcionadas.
#'   \item `dc_cat`: La categor<c3><ad>a de distancia a la costa (por ejemplo, "05-15 mn", "15-30 mn", etc.).
#' }
#' @examples
#' \dontrun{
#' # Procesamiento de datos
#' data_calas <- procesar_calas(data_calas = calas_bitacora)
#' data_faenas <- procesar_faenas(data_faenas = faenas_bitacora)
#' calas_tallas <- procesar_tallas(data_tallas = tallas_bitacora)
#'
#' # Merge de calas, tallas y faenas
#' data_tallasfaenas <- merge(x = data_faenas, y = calas_tallas, by = 'codigo_faena')
#' data_total <- merge_tallas_faenas_calas(data_calas = data_calas, data_tallas_faenas = data_tallasfaenas)
#'
#' # Aplicaci<c3><b3>n de la funci<c3><b3>n
#' resultados <- agregar_variables(data_total)
#' }
#' @export
#' @importFrom dplyr mutate case_when %>%
agregar_variables <- function(data,
                              JuvLim = 12,
                              tipo_distancia = "haversine",
                              ventana = 0.5,
                              unidad = "mn") {

  stopifnot(is.data.frame(data))
  required_cols <- c("lon_inicial", "lat_inicial")
  if (!all(required_cols %in% names(data))) {
    stop("Faltan columnas requeridas: lon_inicial y/o lat_inicial")
  }

  tallas <- grep(pattern = "^[1-9][0-9]*$",
                 x = names(data),
                 value = TRUE)
  if (length(tallas) == 0) {
    stop("No se encontraron columnas de tallas con nombres numéricos.")
  }

  data[tallas] <- lapply(data[tallas], as.numeric)

  data$juv <- apply(data[, tallas, drop = FALSE],
                    1,
                    porc_juveniles,
                    tallas = as.numeric(tallas),
                    juvLim = JuvLim)

  data$muestra <- rowSums(data[, tallas], na.rm = TRUE)

  data$dc <- tryCatch(
    Tivy::distancia_costa(
      lon = data$lon_inicial,
      lat = data$lat_inicial,
      linea_costa = Tivy::linea_costa_peru,
      tipo_distancia = tipo_distancia,
      ventana = ventana,
      unidad = unidad
    ),
    error = function(e) {
      warning("Error en cálculo de distancia a costa: ", conditionMessage(e))
      return(rep(NA_real_, nrow(data)))
    }
  )

  data <- data %>%
    dplyr::mutate(
      dc_cat = dplyr::case_when(
        !is.na(dc) & dc >= 5  & dc < 15  ~ "05-15 mn",
        !is.na(dc) & dc >= 15 & dc < 30  ~ "15-30 mn",
        !is.na(dc) & dc >= 30 & dc < 50  ~ "30-50 mn",
        !is.na(dc) & dc >= 50 & dc < 100 ~ "50-100 mn",
        TRUE                             ~ NA_character_
      )
    )
  return(data)
}
