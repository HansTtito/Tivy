#' Convertir latitud o longitud a grados decimales
#'
#' @description
#' Convierte coordenadas expresadas en formato grados, minutos y segundos (DMS) o grados y minutos (DM) a grados decimales.
#' Por defecto, se asume que las coordenadas están en el hemisferio sur (latitudes negativas).
#'
#' @param coordenadas Vector de caracteres. Cada elemento debe estar en formatos como:
#'   - Con símbolos: `"G° M' S\""`, `"G° M'"`, `"17°26'S"`
#'   - Sin símbolos: `"G M S"`, `"G M"`, `"17 26 S"`
#'   - El hemisferio puede estar incluido en la coordenada
#' @param hemisferio Caracter `"N"`, `"S"`, `"E"`, `"W"` o `"O"` que indica el hemisferio correspondiente
#' cuando no está especificado en la coordenada. `"S"` y `"W"`/`"O"` generan valores negativos. Default: `"S"`.
#'
#' @return Un vector numérico con las coordenadas convertidas a grados decimales.
#' @export
#'
#' @examples
#' # Convertir coordenadas del sur (formato completo)
#' dms_a_decimal(c("39° 48' 36\""), hemisferio = "S")
#'
#' # Convertir coordenadas del oeste (formato completo sin símbolos)
#' dms_a_decimal(c("73 15 0"), hemisferio = "O")
#'
#' # Convertir coordenadas con solo grados y minutos
#' dms_a_decimal(c("39° 48'"), hemisferio = "S")
#' dms_a_decimal(c("73 15"), hemisferio = "W")
#'
#' # Coordenadas con hemisferio incluido
#' dms_a_decimal(c("17°26'S"))
#' dms_a_decimal(c("73°15'W"))
#' dms_a_decimal(c("39 48 N"))
#'
#' # En un dataframe
#' # dms_a_decimal(calas$Longitud.Fin)
#'
#' @importFrom stringr str_split str_count str_detect str_extract
dms_a_decimal <- function(coordenadas, hemisferio = "S") {
  # Validación de entrada
  if (!is.character(coordenadas)) {
    stop("Las coordenadas deben ser una cadena de texto o un vector de caracteres.")
  }
  if (!hemisferio %in% c("N", "S", "E", "W", "O")) {
    stop("El hemisferio debe ser uno de: 'N', 'S', 'E', 'W' o 'O'.")
  }

  # Procesar cada coordenada
  resultados <- sapply(coordenadas, function(coord) {
    tryCatch({
      # Detectar si la coordenada incluye el hemisferio
      hemisferio_local <- hemisferio
      coord_original <- coord

      # Buscar indicadores de hemisferio (N, S, E, W, O)
      patron_hemisferio <- "[NSEW]|O"
      hemisferio_encontrado <- regmatches(coord, regexpr(patron_hemisferio, coord))

      if (length(hemisferio_encontrado) > 0 && hemisferio_encontrado != "") {
        hemisferio_local <- hemisferio_encontrado
        # Eliminar el hemisferio de la coordenada
        coord <- gsub(patron_hemisferio, "", coord)
      }

      # Determinar el signo según el hemisferio
      signo <- ifelse(hemisferio_local %in% c("S", "W", "O"), -1, 1)

      # Limpiar y normalizar la coordenada
      # Reemplazar grados, minutos y segundos con espacios
      coord_limpia <- gsub("[°'\"]", " ", coord)
      # Eliminar espacios múltiples
      coord_limpia <- gsub("\\s+", " ", coord_limpia)
      # Eliminar espacios al inicio y final
      coord_limpia <- trimws(coord_limpia)

      # Dividir en componentes
      componentes <- unlist(strsplit(coord_limpia, " "))
      # Filtrar componentes no numéricos
      componentes_num <- componentes[grepl("^[0-9]+(\\.[0-9]+)?$", componentes)]
      partes <- as.numeric(componentes_num)

      # Calcular según el número de componentes
      if (length(partes) == 3) {
        # Formato completo: grados, minutos, segundos
        decimal <- signo * (partes[1] + partes[2] / 60 + partes[3] / 3600)
      } else if (length(partes) == 2) {
        # Formato parcial: solo grados y minutos
        decimal <- signo * (partes[1] + partes[2] / 60)
      } else if (length(partes) == 1) {
        # Solo grados
        decimal <- signo * partes[1]
      } else {
        warning(paste("Formato no reconocido para la coordenada:", coord_original))
        return(NA_real_)
      }

      return(decimal)
    }, error = function(e) {
      warning(paste("Error al procesar la coordenada", coord, ":", e$message))
      return(NA_real_)
    })
  })

  names(resultados) <- NULL

  return(resultados)
}
