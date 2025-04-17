#' Merging data from fishing trips, length and hauls
#'
#' @description
#' `merge_length_fishing_trips_hauls()` is a function to join data from fishing trips, length and hauls, combining catches by species,
#' length ranges (minimum and maximum) and the spatial and temporal information of each haul.
#'
#' @param data_hauls A data frame processed with `process_hauls()`.
#' @param data_length_fishing_trips A data frame with length data by fishing trip and haul.
#'
#' @return A data frame with the consolidated data from fishing trips, length and hauls.
#' @export
#'
#' @examples
#'
#' data(calas_bitacora, faenas_bitacora, tallas_bitacora)
#'
#' data_hauls <- process_hauls(data_hauls = calas_bitacora, format = "xlsx")
#' data_fishing_trips <- process_fishing_trips(data_fishing_trips = faenas_bitacora, format = "xlsx")
#' data_length <- process_length(data_length = tallas_bitacora, format = "xlsx")
#'
#' data_length_fishing_trips <- merge(x = data_length, y = data_fishing_trips, by = "fishing_trip_code", all = TRUE)
#'
#' data_total <- merge_length_fishing_trips_hauls(data_hauls = data_hauls, data_length_fishing_trips = data_length_fishing_trips)
#'
#' print(head(data_total))
#'
#' @importFrom dplyr rename_with matches filter mutate group_by reframe ungroup select rowwise c_across all_of
#' @importFrom tidyr pivot_wider
merge_length_fishing_trips_hauls <- function(data_hauls, data_length_fishing_trips) {
  # Parameter validation
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

  # Verify required columns
  required_hauls <- c("fishing_trip_code", "haul_number", "description")
  required_length <- c("fishing_trip_code", "haul_number", "description")

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

  # Check if there is data
  if (nrow(data_hauls) == 0) {
    warning("'data_hauls' is empty.")
  }
  if (nrow(data_length_fishing_trips) == 0) {
    warning("'data_length_fishing_trips' is empty.")
  }

  # Check if 'catch' exists in data_hauls
  if (!"catch" %in% names(data_hauls)) {
    warning("The 'catch' column is not present in 'data_hauls'. Catches by species will not be processed.")
    catch_sps <- data.frame(fishing_trip_code = character(0), haul_number = character(0))
  } else {
    # Rename dates in hauls with validation
    tryCatch({
      data_hauls <- data_hauls %>%
        dplyr::rename_with(~ gsub("start_date", "start_date_haul", .x), .cols = dplyr::matches("start_date")) %>%
        dplyr::rename_with(~ gsub("end_date", "end_date_haul", .x), .cols = dplyr::matches("end_date"))
    }, error = function(e) {
      warning("Error renaming dates in 'data_hauls': ", e$message)
    })

    # Rename dates in length_fishing_trips with validation
    tryCatch({
      data_length_fishing_trips <- data_length_fishing_trips %>%
        dplyr::rename_with(~ gsub("start_date", "start_date_fishing_trip", .x), .cols = dplyr::matches("start_date")) %>%
        dplyr::rename_with(~ gsub("end_date", "end_date_fishing_trip", .x), .cols = dplyr::matches("end_date"))
    }, error = function(e) {
      warning("Error renaming dates in 'data_length_fishing_trips': ", e$message)
    })

    # Identify length columns
    length <- grep(pattern = "^[1-9]", x = names(data_length_fishing_trips), value = TRUE)
    if (length(length) == 0) {
      warning("No length columns (numbers) found in 'data_length_fishing_trips'. length processing will be omitted.")
      return(merge(data_hauls, data_length_fishing_trips, by = c("fishing_trip_code", "haul_number", "description"), all = TRUE))
    }

    # ---- CATCHES BY SPECIES ----
    tryCatch({
      catch_sps <- data_hauls %>%
        dplyr::filter(!is.na(description), description != "") %>%
        dplyr::mutate(catch = as.numeric(catch)) %>%
        dplyr::group_by(fishing_trip_code, haul_number, description) %>%
        dplyr::reframe(catch = sum(catch, na.rm = TRUE))

      # Check if there is data after filtering
      if (nrow(catch_sps) == 0) {
        warning("No valid catch data after filtering. Check the values of 'description' and 'catch'.")
        catch_sps <- data.frame(fishing_trip_code = character(0), haul_number = character(0))
      } else {
        catch_sps <- tidyr::pivot_wider(catch_sps,
                                        names_from = description,
                                        values_from = catch,
                                        names_prefix = "catch_")
      }
    }, error = function(e) {
      warning("Error processing catches by species: ", e$message)
      catch_sps <- data.frame(fishing_trip_code = character(0), haul_number = character(0))
    })
  }

  # ---- MINIMUM AND MAXIMUM RANGES ----
  tryCatch({
    # Ensure length columns are numeric
    data_length_numeric <- data_length_fishing_trips
    for (col in length) {
      if (!is.numeric(data_length_numeric[[col]])) {
        data_length_numeric[[col]] <- as.numeric(data_length_numeric[[col]])
      }
    }

    data_length_ranges <- data_length_numeric %>%
      dplyr::filter(!is.na(description), description != "") %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        min_range = tryCatch(
          min_range(dplyr::c_across(dplyr::all_of(length)), as.numeric(length)),
          error = function(e) NA_real_
        ),
        max_range = tryCatch(
          max_range(dplyr::c_across(dplyr::all_of(length)), as.numeric(length)),
          error = function(e) NA_real_
        )
      ) %>%
      dplyr::ungroup()

    # Check if there is data after filtering
    if (nrow(data_length_ranges) == 0) {
      warning("No valid length data after filtering. Check the values of 'description'.")
      min_max_sps <- data.frame(fishing_trip_code = character(0), haul_number = character(0))
    } else {
      min_sps <- data_length_ranges %>%
        dplyr::select(fishing_trip_code, haul_number, description, min_range) %>%
        tidyr::pivot_wider(names_from = description, values_from = min_range, names_prefix = "min_")

      max_sps <- data_length_ranges %>%
        dplyr::select(fishing_trip_code, haul_number, description, max_range) %>%
        tidyr::pivot_wider(names_from = description, values_from = max_range, names_prefix = "max_")

      min_max_sps <- merge(min_sps, max_sps, by = c("fishing_trip_code", "haul_number"), all = TRUE)
      data_length_fishing_trips <- data_length_ranges  # Update with calculated ranges
    }
  }, error = function(e) {
    warning("Error processing length ranges: ", e$message)
    min_max_sps <- data.frame(fishing_trip_code = character(0), haul_number = character(0))
  })

  # ---- JOINING length AND RANGES ----
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

  # ---- JOINING EVERYTHING WITH HAULS ----
  tryCatch({
    if (exists("catch_sps") && nrow(catch_sps) > 0) {
      total_data <- merge(catch_sps, length_total, by = c("fishing_trip_code", "haul_number"), all = TRUE)
    } else {
      total_data <- length_total
    }

    # Remove the 'catch' column from data_hauls to avoid duplicates
    if ("catch" %in% names(data_hauls)) {
      data_hauls_without_catch <- data_hauls %>% dplyr::select(-dplyr::matches("catch"))
    } else {
      data_hauls_without_catch <- data_hauls
    }

    final_data <- merge(
      data_hauls_without_catch,
      total_data,
      by = c("fishing_trip_code", "haul_number", "description"),
      all = TRUE
    )

    # Remove duplicates if they exist
    if (nrow(final_data) > 0) {
      final_data <- final_data[!duplicated(final_data[, c("fishing_trip_code", "haul_number", "description")]), ]
    }

  }, error = function(e) {
    warning("Error in the final data joining: ", e$message)
    return(data_hauls)  # Return at least the original hauls data
  })

  return(final_data)
}