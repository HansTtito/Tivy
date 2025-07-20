#' Merge fishing trips, length and hauls data
#'
#' @description
#' Joins data from fishing trips, length and hauls, combining catches by species,
#' length ranges (minimum and maximum) and spatial-temporal information of each haul.
#'
#' @param data_hauls Data frame processed with `process_hauls()`.
#' @param data_length_fishing_trips Data frame with length data by fishing trip and haul.
#'
#' @return Data frame with consolidated data from fishing trips, length and hauls.
#'
#' @examples
#' \dontrun{
#' data_hauls <- process_hauls(data_hauls = calas_bitacora)
#' data_fishing_trips <- process_fishing_trips(data_fishing_trips = faenas_bitacora)
#' data_length <- process_length(data_length = tallas_bitacora)
#'
#' data_length_fishing_trips <- merge(
#'    x = data_length,
#'    y = data_fishing_trips,
#'    by = "fishing_trip_code",
#'    all = TRUE
#' )
#'
#' data_total <- merge_length_fishing_trips_hauls(
#'    data_hauls = data_hauls,
#'    data_length_fishing_trips = data_length_fishing_trips
#' )
#' }
#'
#' @export
#' @importFrom dplyr rename_with matches filter mutate group_by reframe ungroup select rowwise c_across all_of
#' @importFrom tidyr pivot_wider
merge_length_fishing_trips_hauls <- function(data_hauls, data_length_fishing_trips) {

  if (missing(data_hauls)) {
    stop("The 'data_hauls' parameter is required.")
  }
  if (missing(data_length_fishing_trips)) {
    stop("The 'data_length_fishing_trips' parameter is required.")
  }

  if (!is.data.frame(data_hauls)) {
    stop("'data_hauls' must be a data.frame.")
  }
  if (!is.data.frame(data_length_fishing_trips)) {
    stop("'data_length_fishing_trips' must be a data.frame.")
  }

  required_hauls <- c("fishing_trip_code", "haul_number", "species")
  required_length <- c("fishing_trip_code", "haul_number", "species")

  missing_hauls <- required_hauls[!required_hauls %in% names(data_hauls)]
  if (length(missing_hauls) > 0) {
    stop("The following required columns are not present in 'data_hauls': ",
         paste(missing_hauls, collapse = ", "))
  }

  missing_length <- required_length[!required_length %in% names(data_length_fishing_trips)]
  if (length(missing_length) > 0) {
    stop("The following required columns are not present in 'data_length_fishing_trips': ",
         paste(missing_length, collapse = ", "))
  }

  if (nrow(data_hauls) == 0) {
    stop("'data_hauls' is empty.")
  }

  if (nrow(data_length_fishing_trips) == 0) {
    stop("'data_length_fishing_trips' is empty.")
  }

  if (!"catch" %in% names(data_hauls)) {
    warning("The 'catch' column is not present in 'data_hauls'. Catches by species will not be processed.")
    catch_sps <- data.frame(fishing_trip_code = character(0), haul_number = character(0))
  } else {

    length_cols <- grep(pattern = "^[1-9]", x = names(data_length_fishing_trips), value = TRUE)
    if (length(length_cols) == 0) {
      warning("No length columns (numbers) found in 'data_length_fishing_trips'. Length processing will be omitted.")
      return(merge(data_hauls, data_length_fishing_trips, by = c("fishing_trip_code", "haul_number", "species"), all = TRUE))
    }

    tryCatch({
      catch_sps <- data_hauls %>%
        dplyr::filter(!is.na(.data$species), .data$species != "") %>%
        dplyr::mutate(catch = safe_numeric_conversion(.data$catch)) %>%
        dplyr::group_by(.data$fishing_trip_code, .data$haul_number, .data$species) %>%
        dplyr::reframe(catch = sum(.data$catch, na.rm = TRUE))

      if (nrow(catch_sps) == 0) {
        warning("No valid catch data after filtering. Check the values of 'species' and 'catch'.")
        catch_sps <- data.frame(fishing_trip_code = character(0), haul_number = character(0))
      } else {
        catch_sps <- tidyr::pivot_wider(catch_sps,
                                        names_from = "species",
                                        values_from = "catch",
                                        names_prefix = "catch_")
      }
    }, error = function(e) {
      warning("Error processing catches by species: ", e$message)
      catch_sps <- data.frame(fishing_trip_code = character(0), haul_number = character(0))
    })
  }

  tryCatch({
    data_length_numeric <- data_length_fishing_trips
    for (col in length_cols) {
      if (!is.numeric(data_length_numeric[[col]])) {
        data_length_numeric[[col]] <- safe_numeric_conversion(data_length_numeric[[col]])
      }
    }

    data_length_ranges <- data_length_numeric %>%
      dplyr::filter(!is.na(.data$species), .data$species != "") %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        min_range = tryCatch(
          get_length_range(dplyr::c_across(dplyr::all_of(length_cols)), safe_numeric_conversion(length_cols), "min"),
          error = function(e) NA_real_
        ),
        max_range = tryCatch(
          get_length_range(dplyr::c_across(dplyr::all_of(length_cols)), safe_numeric_conversion(length_cols), "max"),
          error = function(e) NA_real_
        )
      ) %>%
      dplyr::ungroup()

    if (nrow(data_length_ranges) == 0) {
      warning("No valid length data after filtering. Check the values of 'species'.")
      min_max_sps <- data.frame(fishing_trip_code = character(0), haul_number = character(0))
    } else {
      min_sps <- data_length_ranges %>%
        dplyr::select("fishing_trip_code", "haul_number", "species", "min_range") %>%
        tidyr::pivot_wider(names_from = "species", values_from = "min_range", names_prefix = "min_")

      max_sps <- data_length_ranges %>%
        dplyr::select("fishing_trip_code", "haul_number", "species", "max_range") %>%
        tidyr::pivot_wider(names_from = "species", values_from = "max_range", names_prefix = "max_")

      min_max_sps <- merge(min_sps, max_sps, by = c("fishing_trip_code", "haul_number"), all = TRUE)
      data_length_fishing_trips <- data_length_ranges
    }
  }, error = function(e) {
    warning("Error processing length ranges: ", e$message)
    min_max_sps <- data.frame(fishing_trip_code = character(0), haul_number = character(0))
  })

  tryCatch({
    if (exists("min_max_sps") && nrow(min_max_sps) > 0) {
      length_total <- merge(
        data_length_fishing_trips,
        min_max_sps,
        by = c("fishing_trip_code", "haul_number"),
        all = TRUE
      )
    } else {
      length_total <- data_length_fishing_trips
    }
  }, error = function(e) {
    warning("Error joining length and ranges: ", e$message)
    length_total <- data_length_fishing_trips
  })

  tryCatch({
    if (exists("catch_sps") && nrow(catch_sps) > 0) {
      total_data <- merge(catch_sps, length_total, by = c("fishing_trip_code", "haul_number"), all = TRUE)
    } else {
      total_data <- length_total
    }

    if ("catch" %in% names(data_hauls)) {
      data_hauls_without_catch <- data_hauls %>% dplyr::select(-dplyr::matches("catch"))
    } else {
      data_hauls_without_catch <- data_hauls
    }

    final_data <- merge(
      data_hauls_without_catch,
      total_data,
      by = c("fishing_trip_code", "haul_number", "species"),
      all = TRUE
    )

    if (nrow(final_data) > 0) {
      final_data <- final_data[!duplicated(final_data[, c("fishing_trip_code", "haul_number", "species")]), ]
    }

  }, error = function(e) {
    warning("Error in the final data joining: ", e$message)
    return(data_hauls)
  })

  return(final_data)
}
