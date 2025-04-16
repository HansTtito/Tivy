#' Agrega variables de juveniles, tamaño de muestra, distancia a la costa y su categoría
#'
#' Esta función agrega nuevas variables a un conjunto de datos, incluyendo la proporción de juveniles,
#' el total de individuos en la muestra, la distancia a la costa y la categoría de distancia a la costa.
#'
#' @param data Un data frame que debe contener las coordenadas de latitud (`lat_inicial`) y longitud (`lon_inicial`),
#' así como columnas con los tamaños de los individuos.
#' @param JuvLim Límite de talla para considerar juveniles (default = 12). Si la talla es menor que este valor,
#' el individuo se considera juvenil.
#' @param tipo_distancia Tipo de cálculo de distancia a la costa (opciones como "haversine", etc.),
#' por defecto es "haversine".
#' @param ventana Ventana para suavizar la línea de costa, el valor por defecto es 0.5.
#' @param unidad Unidad de distancia utilizada para la medición de la distancia a la costa
#' ("mn", "km", etc.), por defecto es "mn".
#'
#' @return Un data frame con las siguientes nuevas variables:
#' \itemize{
#'   \item `juv`: La proporción de juveniles en cada fila.
#'   \item `muestra`: El total de individuos en la muestra.
#'   \item `dc`: La distancia a la costa desde las coordenadas de latitud y longitud proporcionadas.
#'   \item `dc_cat`: La categoría de distancia a la costa (por ejemplo, "05-15 mn", "15-30 mn", etc.).
#' }
#' @examples
#'
#' # Procesamiento de datos
#' data_calas <- procesar_calas(data_calas = calas_bitacora)
#' data_faenas <- procesar_faenas(data_faenas = faenas_bitacora)
#' calas_tallas <- procesar_tallas(data_tallas = tallas_bitacora)
#'
#' # Merge de calas, tallas y faenas
#' data_tallasfaenas <- merge(x = data_faenas, y = calas_tallas, by = 'codigo_faena')
#' data_total <- merge_tallas_faenas_calas(data_calas = data_calas, data_tallas_faenas = data_tallasfaenas)
#'
#' # Aplicación de la función
#' resultados <- agregar_variables(data = data_total)
#'
#' print(resultados)
#' @export
#' @importFrom dplyr mutate case_when %>%
agregar_variables <- function(data,
                              JuvLim = 12,
                              tipo_distancia = "haversine",
                              ventana = 0.5,
                              unidad = "mn") {

  stopifnot(is.data.frame(data))
  required_cols <- c("lon_inicial", "lat_inicial")

  # Verificación de columnas necesarias
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Faltan columnas requeridas: ", paste(missing_cols, collapse = ", "))
  }

  # Identificar columnas de tallas
  tallas <- grep(pattern = "^[1-9][0-9]*$",
                 x = names(data),
                 value = TRUE)

  # Si no se encuentran tallas, emitir un mensaje sin detener el flujo
  if (length(tallas) == 0) {
    warning("No se encontraron columnas de tallas con nombres numéricos. No se calculará proporción de juveniles.")
    return(data)
  }

  data[tallas] <- lapply(data[tallas], as.numeric)

  # Proporción de juveniles
  data$juv <- apply(data[, tallas, drop = FALSE],
                    1,
                    porc_juveniles,
                    tallas = as.numeric(tallas),
                    juvLim = JuvLim)

  # Total de individuos en la muestra
  data$muestra <- rowSums(data[, tallas], na.rm = TRUE)

  # Variable para controlar el warning de distancia a la costa
  distancia_warning_shown <- FALSE

  # Calcular distancia a la costa, suprimir warnings y mostrar uno solo
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
      # Si ya se ha mostrado el warning, no lo volvemos a mostrar
      if (!distancia_warning_shown) {
        warning("Error en cálculo de distancia a costa: ", conditionMessage(e))
        distancia_warning_shown <<- TRUE  # Marca el warning como mostrado
      }
      return(rep(NA_real_, nrow(data)))  # Retorna valores NA en caso de error
    }
  )

  # Crear categoría de distancia a la costa
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

