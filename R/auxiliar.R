#' Buscar columnas que contienen un patrón seguido de un número
#'
#' Identifica columnas en un dataframe cuyos nombres siguen el patrón
#' de un prefijo específico seguido de un número.
#'
#' @param data Un data frame donde buscar las columnas.
#' @param patron El patrón o prefijo a buscar (por defecto "pond_").
#' @param ordenar Indica si las columnas deben ordenarse numéricamente (por defecto TRUE).
#' @return Vector de caracteres con los nombres de las columnas que coinciden con el patrón.
#' @export
#' @examples
#' # Identificar todas las columnas pond_X en el dataframe
#'
#' data_calas <- procesar_calas(data_calas = calas_bitacora)
#' data_faenas <- procesar_faenas(data_faenas = faenas_bitacora)
#' calas_tallas <- procesar_tallas(data_tallas = tallas_bitacora)
#'
#' data_tallasfaenas <- merge(x = data_faenas, y = calas_tallas, by = 'codigo_faena')
#' data_total <- merge_tallas_faenas_calas(data_calas = data_calas, data_tallas_faenas = data_tallasfaenas)
#' datos_final <- agregar_variables(data_total)
#'
#' tallas_cols <- c("8", "8.5", "9", "9.5", "10", "10.5", "11", "11.5","12", "12.5", "13", "13.5", "14", "14.5", "15")
#' tallas_pond <- ponderar_tallas_df(df = datos_final, tallas_cols = tallas_cols, captura_col = "catch_ANCHOVETA", a= 0.0001, b = 2.984)
#'
#' cols_pond <- buscar_columnas_patron(data = tallas_pond, patron = "pond_")
#'
#' # Usar las columnas encontradas para cálculos
#' tallas_pond[, cols_pond]
buscar_columnas_patron <- function(data, patron = "pond_", ordenar = TRUE) {
  # Validación de parámetros
  if (!is.data.frame(data)) stop("El parámetro 'data' debe ser un data.frame.")
  if (!is.character(patron)) stop("El parámetro 'patron' debe ser una cadena de texto.")

  # Obtener todos los nombres de columnas
  nombres_columnas <- colnames(data)

  # Buscar columnas que contengan el patrón
  exp_reg <- paste0("^", patron, "[0-9]+(\\.[0-9]+)?$")
  columnas_coincidentes <- nombres_columnas[grep(exp_reg, nombres_columnas)]

  if (length(columnas_coincidentes) == 0) {
    warning("No se encontraron columnas que coincidan con el patrón: ", patron)
    return(character(0))
  }

  # Ordenar numéricamente si es necesario
  if (ordenar) {
    # Extraer los valores numéricos de los nombres de columnas
    valores_numericos <- as.numeric(gsub(patron, "", columnas_coincidentes))
    # Ordenar las columnas según estos valores
    columnas_coincidentes <- columnas_coincidentes[order(valores_numericos)]
  }

  return(columnas_coincidentes)
}



#' Extraer valores numéricos desde nombres de columnas
#'
#' Esta función extrae todos los valores numéricos (incluidos decimales) desde un vector
#' de nombres de columnas, eliminando cualquier carácter no numérico.
#'
#' @param nombres_columnas Vector de caracteres con los nombres de columnas.
#' @return Vector numérico con los valores de tallas extraídos.
#' @export
#' @examples
#' nombres <- c("pond_10.5", "talla_11", "12.5mm", "T14", "13-erróneo")
#' extraer_valores_tallas(nombres)
#' # Resultado: 10.5 11.0 12.5 14.0 13.0
extraer_valores_tallas <- function(nombres_columnas) {
  if (!is.character(nombres_columnas)) {
    stop("El parámetro 'nombres_columnas' debe ser un vector de caracteres.")
  }

  # Extrae la primera aparición de número en cada string (soporta decimales)
  valores <- as.numeric(stringr::str_extract(nombres_columnas, "\\d+\\.?\\d*"))

  if (any(is.na(valores))) {
    warning("Algunos nombres no contienen valores numéricos y fueron convertidos a NA.")
  }

  return(valores)
}


#' Obtener nombres y posiciones de columnas que coinciden con un patrón
#'
#' Devuelve tanto los nombres como las posiciones de columnas en un dataframe
#' que siguen el patrón especificado.
#'
#' @param data Un data frame donde buscar las columnas.
#' @param patron El patrón o prefijo a buscar (por defecto "pond_").
#' @param ordenar Indica si los resultados deben ordenarse numéricamente (por defecto TRUE).
#' @return Lista con dos elementos: "posiciones" y "nombres" de las columnas coincidentes.
#' @export
#' @examples
#' # Obtener tanto posiciones como nombres
#'
#' data_calas <- procesar_calas(data_calas = calas_bitacora)
#' data_faenas <- procesar_faenas(data_faenas = faenas_bitacora)
#' calas_tallas <- procesar_tallas(data_tallas = tallas_bitacora)
#'
#' data_tallasfaenas <- merge(x = data_faenas, y = calas_tallas, by = 'codigo_faena')
#' data_total <- merge_tallas_faenas_calas(data_calas = data_calas, data_tallas_faenas = data_tallasfaenas)
#' datos_final <- agregar_variables(data_total)
#'
#' tallas_cols <- c("8", "8.5", "9", "9.5", "10", "10.5", "11", "11.5","12", "12.5", "13", "13.5", "14", "14.5", "15")
#' tallas_pond <- ponderar_tallas_df(df = datos_final, tallas_cols = tallas_cols, captura_col = "catch_ANCHOVETA", a= 0.0001, b = 2.984)
#'
#' resultado <- info_columnas_patron(tallas_pond, "pond_")
#'
#' posiciones <- resultado$posiciones
#' print(posiciones)
#' nombres <- resultado$nombres
#' print(nombres)
#'
info_columnas_patron <- function(data, patron = "pond_", ordenar = TRUE) {
  # Validación de parámetros
  if (!is.data.frame(data)) stop("El parámetro 'data' debe ser un data.frame.")
  if (!is.character(patron)) stop("El parámetro 'patron' debe ser una cadena de texto.")

  # Obtener todos los nombres de columnas
  nombres_columnas <- colnames(data)

  # Buscar columnas que contengan el patrón
  exp_reg <- paste0("^", patron, "[0-9]+(\\.[0-9]+)?$")
  indices <- grep(exp_reg, nombres_columnas)

  if (length(indices) == 0) {
    warning("No se encontraron columnas que coincidan con el patrón: ", patron)
    return(list(posiciones = integer(0), nombres = character(0)))
  }

  columnas_coincidentes <- nombres_columnas[indices]

  # Ordenar numéricamente si es necesario
  if (ordenar) {
    # Extraer los valores numéricos de los nombres de columnas
    valores_numericos <- as.numeric(gsub(patron, "", columnas_coincidentes))
    # Ordenar según estos valores
    orden <- order(valores_numericos)
    indices <- indices[orden]
    columnas_coincidentes <- columnas_coincidentes[orden]
  }

  return(list(
    posiciones = indices,
    nombres = columnas_coincidentes
  ))
}



#' Calcula porcentajes de juveniles para un conjunto de frecuencias
#'
#' Función auxiliar que calcula porcentajes de juveniles tanto en número como en peso
#' a partir de un conjunto de frecuencias de tallas.
#'
#' @param frecuencias Vector numérico con las frecuencias por talla.
#' @param valores_tallas Vector numérico con los valores de las tallas correspondientes.
#' @param juvLim Talla límite para considerar juveniles (por defecto 12 cm).
#' @param a Coeficiente de la relación longitud-peso.
#' @param b Exponente de la relación longitud-peso.
#' @return Data frame con porcentajes y totales de juveniles.
#' @export
#' @examples
#' frecuencias <- c(10, 15, 25, 30, 20, 10)
#' valores_tallas <- c(8, 9, 10, 11, 12, 13)
#' calcular_juveniles(frecuencias, valores_tallas)
calcular_juveniles <- function(frecuencias, valores_tallas, juvLim = 12, a = 0.0012, b = 3.1242) {
  # Validación de parámetros
  if (!is.numeric(frecuencias)) stop("El parámetro 'frecuencias' debe ser numérico.")
  if (!is.numeric(valores_tallas)) stop("El parámetro 'valores_tallas' debe ser numérico.")
  if (length(frecuencias) != length(valores_tallas))
    stop("Los vectores 'frecuencias' y 'valores_tallas' deben tener la misma longitud.")

  # Verificar si hay datos
  total_numero <- sum(frecuencias, na.rm = TRUE)

  if (total_numero == 0) {
    # Si no hay datos, devolver NA sin warnings
    return(data.frame(
      porc_juv_numero = NA_real_,
      porc_juv_peso = NA_real_,
      total_numero = 0,
      total_peso = 0
    ))
  }

  # Calcular juveniles en número con la función existente
  # Usamos suppressWarnings para evitar warnings redundantes
  porc_juv_numero <- suppressWarnings(porc_juveniles(frecuencias, valores_tallas, juvLim))

  # Calcular pesos
  pesos <- talla_peso(valores_tallas, a, b) * frecuencias
  total_peso <- sum(pesos, na.rm = TRUE)

  # Calcular juveniles en peso con la misma función
  if (total_peso == 0) {
    porc_juv_peso <- NA_real_
  } else {
    porc_juv_peso <- suppressWarnings(porc_juveniles(pesos, valores_tallas, juvLim))
  }

  return(data.frame(
    porc_juv_numero = porc_juv_numero,
    porc_juv_peso = porc_juv_peso,
    total_numero = total_numero,
    total_peso = total_peso
  ))
}

