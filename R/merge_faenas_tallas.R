#' Mergeando datos de faenas, tallas y calas
#'
#' @description
#' `merge_tallas_faenas_calas()` es una función para unir los datos de faenas, tallas y calas, combinando las capturas por especie,
#' los rangos de tallas (mínimo y máximo) y la información espacial y temporal de cada cala.
#'
#' @param data_calas Un data frame procesado con `procesar_calas()`.
#' @param data_tallas_faenas Un data frame con los datos de tallas por faena y cala.
#'
#' @return Un data frame con los datos consolidados de faenas, tallas y calas.
#' @export
#'
#' @examples
#' data_calas <- procesar_calas(data_calas = calas_bitacora, formato = "xlsx")
#' data_faenas <- procesar_faenas(data_faenas = faenas_bitacora, formato = "xlsx")
#' data_tallas <- procesar_tallas(data_tallas = tallas_bitacora, formato = "xlsx")
#' data_tallas_faenas <- merge(x = data_tallas, y = data_faenas, by = "codigo_faena", all = TRUE)
#' data_total <- merge_tallas_faenas_calas(data_calas = data_calas, data_tallas_faenas = data_tallas_faenas)
#'
#' @importFrom dplyr rename_with matches filter mutate group_by summarise ungroup select rowwise c_across all_of
#' @importFrom tidyr pivot_wider
merge_tallas_faenas_calas <- function(data_calas, data_tallas_faenas) {
  # Validación de parámetros
  if (missing(data_calas)) {
    stop("El parámetro 'data_calas' es obligatorio.")
  }
  if (missing(data_tallas_faenas)) {
    stop("El parámetro 'data_tallas_faenas' es obligatorio.")
  }

  if (!is.data.frame(data_calas)) {
    stop("'data_calas' debe ser un data.frame.")
  }
  if (!is.data.frame(data_tallas_faenas)) {
    stop("'data_tallas_faenas' debe ser un data.frame.")
  }

  # Verificar columnas requeridas
  required_calas <- c("codigo_faena", "n_cala", "descripcion")
  required_tallas <- c("codigo_faena", "n_cala", "descripcion")

  missing_calas <- required_calas[!required_calas %in% names(data_calas)]
  if (length(missing_calas) > 0) {
    stop("Las siguientes columnas requeridas no están presentes en 'data_calas': ",
         paste(missing_calas, collapse = ", "))
  }

  missing_tallas <- required_tallas[!required_tallas %in% names(data_tallas_faenas)]
  if (length(missing_tallas) > 0) {
    stop("Las siguientes columnas requeridas no están presentes en 'data_tallas_faenas': ",
         paste(missing_tallas, collapse = ", "))
  }

  # Verificar si hay datos
  if (nrow(data_calas) == 0) {
    warning("'data_calas' está vacío.")
  }
  if (nrow(data_tallas_faenas) == 0) {
    warning("'data_tallas_faenas' está vacío.")
  }

  # Verificar si 'catch' existe en data_calas
  if (!"catch" %in% names(data_calas)) {
    warning("La columna 'catch' no está presente en 'data_calas'. Las capturas por especie no serán procesadas.")
    catch_sps <- data.frame(codigo_faena = character(0), n_cala = character(0))
  } else {
    # Renombrar fechas en calas con validación
    tryCatch({
      data_calas <- data_calas %>%
        dplyr::rename_with(~ gsub("fecha_inicio", "fecha_inicio_cala", .x), .cols = dplyr::matches("fecha_inicio")) %>%
        dplyr::rename_with(~ gsub("fecha_fin", "fecha_fin_cala", .x), .cols = dplyr::matches("fecha_fin"))
    }, error = function(e) {
      warning("Error al renombrar fechas en 'data_calas': ", e$message)
    })

    # Renombrar fechas en tallas_faenas con validación
    tryCatch({
      data_tallas_faenas <- data_tallas_faenas %>%
        dplyr::rename_with(~ gsub("fecha_inicio", "fecha_inicio_faena", .x), .cols = dplyr::matches("fecha_inicio")) %>%
        dplyr::rename_with(~ gsub("fecha_fin", "fecha_fin_faena", .x), .cols = dplyr::matches("fecha_fin"))
    }, error = function(e) {
      warning("Error al renombrar fechas en 'data_tallas_faenas': ", e$message)
    })

    # Identificar columnas de tallas
    tallas <- grep(pattern = "^[1-9]", x = names(data_tallas_faenas), value = TRUE)
    if (length(tallas) == 0) {
      warning("No se encontraron columnas de tallas (números) en 'data_tallas_faenas'. El procesamiento de tallas será omitido.")
      return(merge(data_calas, data_tallas_faenas, by = c("codigo_faena", "n_cala", "descripcion"), all = TRUE))
    }

    # ---- CAPTURAS POR ESPECIE ----
    tryCatch({
      catch_sps <- data_calas %>%
        dplyr::filter(!is.na(descripcion), descripcion != "") %>%
        dplyr::mutate(catch = as.numeric(catch)) %>%
        dplyr::group_by(codigo_faena, n_cala, descripcion) %>%
        dplyr::summarise(catch = sum(catch, na.rm = TRUE), .groups = "drop")

      # Verificar si hay datos después del filtrado
      if (nrow(catch_sps) == 0) {
        warning("No hay datos válidos de capturas después de filtrar. Revise los valores de 'descripcion' y 'catch'.")
        catch_sps <- data.frame(codigo_faena = character(0), n_cala = character(0))
      } else {
        catch_sps <- tidyr::pivot_wider(catch_sps,
                                        names_from = descripcion,
                                        values_from = catch,
                                        names_prefix = "catch_")
      }
    }, error = function(e) {
      warning("Error al procesar capturas por especie: ", e$message)
      catch_sps <- data.frame(codigo_faena = character(0), n_cala = character(0))
    })
  }

  # ---- RANGOS MÍNIMOS Y MÁXIMOS ----
  tryCatch({
    # Asegurar que las columnas de tallas sean numéricas
    data_tallas_numericas <- data_tallas_faenas
    for (col in tallas) {
      if (!is.numeric(data_tallas_numericas[[col]])) {
        data_tallas_numericas[[col]] <- as.numeric(data_tallas_numericas[[col]])
      }
    }

    data_tallas_rangos <- data_tallas_numericas %>%
      dplyr::filter(!is.na(descripcion), descripcion != "") %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        min_rango = tryCatch(
          min_range(dplyr::c_across(dplyr::all_of(tallas)), as.numeric(tallas)),
          error = function(e) NA_real_
        ),
        max_rango = tryCatch(
          max_range(dplyr::c_across(dplyr::all_of(tallas)), as.numeric(tallas)),
          error = function(e) NA_real_
        )
      ) %>%
      dplyr::ungroup()

    # Verificar si hay datos después del filtrado
    if (nrow(data_tallas_rangos) == 0) {
      warning("No hay datos válidos de tallas después de filtrar. Revise los valores de 'descripcion'.")
      min_max_sps <- data.frame(codigo_faena = character(0), n_cala = character(0))
    } else {
      min_sps <- data_tallas_rangos %>%
        dplyr::select(codigo_faena, n_cala, descripcion, min_rango) %>%
        tidyr::pivot_wider(names_from = descripcion, values_from = min_rango, names_prefix = "min_")

      max_sps <- data_tallas_rangos %>%
        dplyr::select(codigo_faena, n_cala, descripcion, max_rango) %>%
        tidyr::pivot_wider(names_from = descripcion, values_from = max_rango, names_prefix = "max_")

      min_max_sps <- merge(min_sps, max_sps, by = c("codigo_faena", "n_cala"), all = TRUE)
      data_tallas_faenas <- data_tallas_rangos  # Actualizar con rangos calculados
    }
  }, error = function(e) {
    warning("Error al procesar rangos de tallas: ", e$message)
    min_max_sps <- data.frame(codigo_faena = character(0), n_cala = character(0))
  })

  # ---- UNIÓN DE TALLAS Y RANGOS ----
  tryCatch({
    if (exists("min_max_sps") && nrow(min_max_sps) > 0) {
      tallas_total <- merge(
        data_tallas_faenas,
        min_max_sps,
        by = c("codigo_faena", "n_cala"),
        all = TRUE
      )
    } else {
      tallas_total <- data_tallas_faenas
    }
  }, error = function(e) {
    warning("Error al unir tallas y rangos: ", e$message)
    tallas_total <- data_tallas_faenas
  })

  # ---- UNIÓN DE TODO CON CALAS ----
  tryCatch({
    if (exists("catch_sps") && nrow(catch_sps) > 0) {
      total_data <- merge(catch_sps, tallas_total, by = c("codigo_faena", "n_cala"), all = TRUE)
    } else {
      total_data <- tallas_total
    }

    # Eliminar la columna 'catch' de data_calas para evitar duplicados
    if ("catch" %in% names(data_calas)) {
      data_calas_sin_catch <- data_calas %>% dplyr::select(-dplyr::matches("catch"))
    } else {
      data_calas_sin_catch <- data_calas
    }

    final_data <- merge(
      data_calas_sin_catch,
      total_data,
      by = c("codigo_faena", "n_cala", "descripcion"),
      all = TRUE
    )

    # Eliminar duplicados si existen
    if (nrow(final_data) > 0) {
      final_data <- final_data[!duplicated(final_data[, c("codigo_faena", "n_cala", "descripcion")]), ]
    }

  }, error = function(e) {
    warning("Error en la unión final de datos: ", e$message)
    return(data_calas)  # Devolver al menos los datos de calas originales
  })

  return(final_data)
}
