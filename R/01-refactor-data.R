#' Process fishing haul data from PRODUCE sitrapesca files
#'
#' @description
#' Processes fishing haul data from PRODUCE logbooks. Automatically detects
#' required columns and creates standardized output with coordinates converted
#' to decimal degrees.
#'
#' @param data_hauls Data frame with raw haul data.
#' @param correct_coordinates Logical. Correct coordinate errors during conversion.
#' @param verbose Logical. Print column mapping information.
#'
#' @return Data frame with 16 standardized columns including fishing trip code,
#'   haul number, dates, coordinates, species, and catch data.
#'
#' @examples
#' processed_hauls <- process_hauls(data_hauls = calas_bitacora)
#' processed_hauls <- process_hauls(data_hauls = calas_bitacora, verbose = TRUE)
#'
#' @export
#' @importFrom dplyr select mutate
#' @importFrom stringi stri_trim
process_hauls <- function(data_hauls, correct_coordinates = TRUE, verbose = FALSE) {
  
  if (!is.data.frame(data_hauls)) {
    stop("'data_hauls' must be a data frame.")
  }
  
  if (nrow(data_hauls) == 0) {
    stop("Input data frame is empty.")
  }
  
  column_patterns <- list(
    fishing_trip_code = c("codigo.*faena", "trip.*code", "faena", "codigo_faena", "viaje","fishing_trip_code"),
    haul_number = c("numero.*cala", "haul.*number", "cala", "numero_cala", "lance","n_cala"),
    start_date = c("fecha.*inicio", "start.*date", "inicio", "fecha_inicio","start_date", "date_start"),
    start_latitude = c("latitud.*inicial", "lat.*inicial", "start.*lat", "latitud_inicial", "lat_inicio", "start_latitude"),
    start_longitude = c("longitud.*inicial", "lon.*inicial", "start.*lon", "longitud_inicial", "lon_inicio", "start_longitude"),
    species = c("especie", "species", "sp", "nombre_cientifico", "nombre_comun"),
    catch = c("captura", "catch", "peso", "kg", "toneladas"),
    registration_date = c("fecha.*registro", "registration.*date", "registro", "fecha_registro", "date_reg","registration_date"),
    end_date = c("fecha.*fin", "end.*date", "fin", "fecha_fin", "date_end","end_date"),
    end_latitude = c("latitud.*final", "lat.*final", "end.*lat", "latitud_final", "lat_fin","end_latitude"),
    end_longitude = c("longitud.*final", "lon.*final", "end.*lon", "longitud_final", "lon_fin","end_longitude"),
    gear_type = c("tipo.*arte", "gear.*type", "arte", "tipo_arte", "aparejo","gear_type","gear")
  )
  
  critical_columns <- c("fishing_trip_code", "haul_number", "start_date", 
                       "start_latitude", "start_longitude", "species", 
                       "catch", "registration_date")
  
  optional_columns <- c("end_date", "end_latitude", "end_longitude", "gear_type")
  
  original_names <- names(data_hauls)
  column_mapping <- list()
  missing_critical <- character()
  
  if (verbose) {
    message("=== Column Mapping Process ===")
  }
  
  for (col_name in names(column_patterns)) {
    col_index <- find_column(column_patterns[[col_name]], original_names, verbose = verbose)
    
    if (!is.null(col_index)) {
      column_mapping[[col_name]] <- col_index
      if (verbose) {
        message(sprintf("Mapped '%s' -> '%s' (column %d)", 
                       col_name, original_names[col_index], col_index))
      }
    } else {
      if (col_name %in% critical_columns) {
        missing_critical <- c(missing_critical, col_name)
      }
      if (verbose) {
        message(sprintf("Column '%s' not found", col_name))
      }
    }
  }
  
  if (length(missing_critical) > 0) {
    stop(sprintf("Critical columns not found: %s\nPlease ensure your data contains these required columns.", 
                paste(missing_critical, collapse = ", ")))
  }
  
  found_indices <- unlist(column_mapping)
  selected_data <- data_hauls[, found_indices, drop = FALSE]
  names(selected_data) <- names(column_mapping)
  
  for (opt_col in optional_columns) {
    if (!opt_col %in% names(selected_data)) {
      selected_data[[opt_col]] <- NA
      if (verbose) {
        message(sprintf("+ Added missing optional column '%s' with NA values", opt_col))
      }
    }
  }
  
  final_order <- c(critical_columns, optional_columns)
  selected_data <- selected_data[, final_order]
  
  if (verbose) {
    message("\n=== Data Processing ===")
  }
  
  selected_data <- selected_data %>%
    dplyr::mutate(
      species = stringi::stri_trim(as.character(.data[["species"]])),
      gear_type = ifelse(!is.na(.data[["gear_type"]]), 
                        stringi::stri_trim(as.character(.data[["gear_type"]])), NA),
      start_date_haul = convert_to_date(.data[["start_date"]], output_type = "datetime"),
      end_date_haul = convert_to_date(.data[["end_date"]], output_type = "datetime"),
      registration_date = convert_to_date(.data[["registration_date"]], output_type = "datetime"),
      catch = safe_numeric_conversion(.data[["catch"]]),
      haul_number = safe_numeric_conversion(.data[["haul_number"]])
    )
  
  selected_data <- selected_data %>%
    dplyr::mutate(
      lat_initial = dms_to_decimal(.data[["start_latitude"]], 
                                  correct_errors = correct_coordinates),
      lon_initial = dms_to_decimal(.data[["start_longitude"]], 
                                  hemisphere = "W", 
                                  correct_errors = correct_coordinates),
      lat_final = ifelse(!is.na(.data[["end_latitude"]]),
                        dms_to_decimal(.data[["end_latitude"]], 
                                      correct_errors = correct_coordinates), NA),
      lon_final = ifelse(!is.na(.data[["end_longitude"]]),
                        dms_to_decimal(.data[["end_longitude"]], 
                                      hemisphere = "W", 
                                      correct_errors = correct_coordinates), NA)
    )
  
  if (verbose) {
    message("\n=== Processing Summary ===")
    message(sprintf("Records processed: %d", nrow(selected_data)))
    message(sprintf("Critical columns found: %d/%d", 
                   length(critical_columns), length(critical_columns)))
    message(sprintf("Optional columns found: %d/%d", 
                   length(column_mapping) - length(critical_columns), length(optional_columns)))
    message(sprintf("Total output columns: %d", ncol(selected_data)))
  }
  
  return(selected_data)
}

#' Validate processed haul data quality
#'
#' @description
#' Validates data quality metrics for processed haul data.
#'
#' @param processed_hauls Data frame returned by process_hauls().
#'
#' @return List with data quality metrics including completeness scores and issue counts.
#'
#' @export
validate_haul_data <- function(processed_hauls) {
  
  if (!is.data.frame(processed_hauls)) {
    stop("Input must be a data frame from process_hauls()")
  }
  
  quality_metrics <- list(
    total_records = nrow(processed_hauls),
    missing_trip_codes = sum(is.na(processed_hauls$fishing_trip_code)),
    missing_haul_numbers = sum(is.na(processed_hauls$haul_number)),
    missing_species = sum(is.na(processed_hauls$species) | processed_hauls$species == ""),
    missing_catches = sum(is.na(processed_hauls$catch)),
    missing_start_coordinates = sum(is.na(processed_hauls$lat_initial) | 
                                   is.na(processed_hauls$lon_initial)),
    invalid_coordinates = sum(abs(processed_hauls$lat_initial) > 90 | 
                             abs(processed_hauls$lon_initial) > 180, na.rm = TRUE),
    negative_catches = sum(processed_hauls$catch < 0, na.rm = TRUE),
    duplicate_hauls = sum(duplicated(paste(processed_hauls$fishing_trip_code, 
                                          processed_hauls$haul_number))),
    future_dates = sum(processed_hauls$start_date_haul > Sys.time(), na.rm = TRUE),
    has_end_dates = sum(!is.na(processed_hauls$end_date_haul)),
    has_end_coordinates = sum(!is.na(processed_hauls$lat_final) & 
                             !is.na(processed_hauls$lon_final)),
    has_gear_type = sum(!is.na(processed_hauls$gear_type)),
    processing_timestamp = Sys.time()
  )
  
  critical_issues <- quality_metrics$missing_trip_codes + 
                    quality_metrics$missing_haul_numbers + 
                    quality_metrics$missing_species +
                    quality_metrics$missing_start_coordinates +
                    quality_metrics$invalid_coordinates +
                    quality_metrics$negative_catches +
                    quality_metrics$duplicate_hauls
  
  quality_metrics$quality_score <- max(0, round(100 - (critical_issues / quality_metrics$total_records * 100), 1))
  
  return(quality_metrics)
}

#' Process fishing trip data from PRODUCE sitrapesca files
#'
#' @description
#' Processes fishing trip data from PRODUCE logbooks. Automatically detects
#' required columns and creates standardized output with proper date conversion.
#'
#' @param data_fishing_trips Data frame with raw fishing trip data.
#' @param verbose Logical. Print column mapping information.
#'
#' @return Data frame with 6 standardized columns including trip code, vessel
#'   information, and trip dates.
#'
#' @examples
#' fishing_trips <- process_fishing_trips(data_fishing_trips = faenas_bitacora)
#' fishing_trips <- process_fishing_trips(data_fishing_trips = faenas_bitacora, verbose = TRUE)
#'
#' @export
#' @importFrom dplyr select mutate
#' @importFrom stringi stri_trim
process_fishing_trips <- function(data_fishing_trips, verbose = FALSE) {
  
  if (!is.data.frame(data_fishing_trips)) {
    stop("'data_fishing_trips' must be a data frame.")
  }
  
  if (nrow(data_fishing_trips) == 0) {
    stop("Input data frame is empty.")
  }
  
  column_patterns <- list(
    fishing_trip_code = c("codigo.*faena", "trip.*code", "faena", "codigo_faena", "viaje"),
    vessel = c("embarcacion", "vessel", "barco", "nave", "buque"),
    id_vessel = c("id.*embarcacion","id_embarcacion", "vessel.*id", "matricula", "registro", "id_vessel"),
    start_date_fishing_trip = c("fecha.*inicio.*viaje", "start.*date.*trip", "inicio.*faena", 
                               "fecha_inicio_faena", "start_trip", "fecha.*salida","start_date",
                               "start_date_fishing_trip", "start_date_fishing"),
    end_date_fishing_trip = c("fecha.*fin.*viaje", "end.*date.*trip", "fin.*faena", 
                             "fecha_fin_faena", "end_trip", "fecha.*llegada","end_date","end_date_fishing_trip",
                            "end_date_fishing"),
    owner = c("armador", "owner", "propietario", "dueno", "dueño")
  )
  
  critical_columns <- c("fishing_trip_code", "vessel", "id_vessel", 
                       "start_date_fishing_trip", "end_date_fishing_trip")
  
  optional_columns <- c("owner")
  
  original_names <- names(data_fishing_trips)
  column_mapping <- list()
  missing_critical <- character()
  
  if (verbose) {
    message("=== Fishing Trip Column Mapping Process ===")
  }
  
  for (col_name in names(column_patterns)) {
    col_index <- find_column(column_patterns[[col_name]], original_names, verbose = verbose)
    
    if (!is.null(col_index)) {
      column_mapping[[col_name]] <- col_index
      if (verbose) {
        message(sprintf("Mapped '%s' -> '%s' (column %d)", 
                       col_name, original_names[col_index], col_index))
      }
    } else {
      if (col_name %in% critical_columns) {
        missing_critical <- c(missing_critical, col_name)
      }
      if (verbose) {
        message(sprintf("Column '%s' not found", col_name))
      }
    }
  }
  
  if (length(missing_critical) > 0) {
    stop(sprintf("Critical columns not found: %s\nPlease ensure your data contains these required columns.", 
                paste(missing_critical, collapse = ", ")))
  }
  
  found_indices <- unlist(column_mapping)
  selected_data <- data_fishing_trips[, found_indices, drop = FALSE]
  names(selected_data) <- names(column_mapping)
  
  for (opt_col in optional_columns) {
    if (!opt_col %in% names(selected_data)) {
      selected_data[[opt_col]] <- NA
      if (verbose) {
        message(sprintf("+ Added missing optional column '%s' with NA values", opt_col))
      }
    }
  }
  
  final_order <- c(critical_columns, optional_columns)
  selected_data <- selected_data[, final_order]
  
  if (verbose) {
    message("\n=== Fishing Trip Data Processing ===")
  }
  
  selected_data <- selected_data %>%
    dplyr::mutate(
      vessel = stringi::stri_trim(as.character(.data[["vessel"]])),
      owner = ifelse(!is.na(.data[["owner"]]), 
                    stringi::stri_trim(as.character(.data[["owner"]])), NA),
      id_vessel = stringi::stri_trim(as.character(.data[["id_vessel"]])),
      start_date_fishing_trip = convert_to_date(.data[["start_date_fishing_trip"]], output_type = "datetime"),
      end_date_fishing_trip = convert_to_date(.data[["end_date_fishing_trip"]], output_type = "datetime"),
      fishing_trip_code = stringi::stri_trim(as.character(.data[["fishing_trip_code"]]))
    )
  
  if (verbose) {
    message("\n=== Fishing Trip Processing Summary ===")
    message(sprintf("Records processed: %d", nrow(selected_data)))
    message(sprintf("Critical columns found: %d/%d", 
                   length(critical_columns), length(critical_columns)))
    message(sprintf("Optional columns found: %d/%d", 
                   length(column_mapping) - length(critical_columns), length(optional_columns)))
    message(sprintf("Total output columns: %d", ncol(selected_data)))
  }
  
  return(selected_data)
}

#' Validate processed fishing trip data quality
#'
#' @description
#' Validates data quality metrics for processed fishing trip data.
#'
#' @param processed_trips Data frame returned by process_fishing_trips().
#'
#' @return List with data quality metrics including completeness scores and issue counts.
#'
#' @export
validate_fishing_trip_data <- function(processed_trips) {
  
  if (!is.data.frame(processed_trips)) {
    stop("Input must be a data frame from process_fishing_trips()")
  }
  
  quality_metrics <- list(
    total_records = nrow(processed_trips),
    missing_trip_codes = sum(is.na(processed_trips$fishing_trip_code) | 
                            processed_trips$fishing_trip_code == ""),
    missing_vessels = sum(is.na(processed_trips$vessel) | 
                         processed_trips$vessel == ""),
    missing_vessel_ids = sum(is.na(processed_trips$id_vessel) | 
                            processed_trips$id_vessel == ""),
    missing_start_dates = sum(is.na(processed_trips$start_date_fishing_trip)),
    missing_end_dates = sum(is.na(processed_trips$end_date_fishing_trip)),
    duplicate_trip_codes = sum(duplicated(processed_trips$fishing_trip_code)),
    future_start_dates = sum(processed_trips$start_date_fishing_trip > Sys.time(), na.rm = TRUE),
    invalid_date_ranges = sum(processed_trips$end_date_fishing_trip < processed_trips$start_date_fishing_trip, 
                             na.rm = TRUE),
    has_owners = sum(!is.na(processed_trips$owner) & processed_trips$owner != ""),
    processing_timestamp = Sys.time()
  )
  
  critical_issues <- quality_metrics$missing_trip_codes + 
                    quality_metrics$missing_vessels + 
                    quality_metrics$missing_vessel_ids +
                    quality_metrics$duplicate_trip_codes +
                    quality_metrics$invalid_date_ranges
  
  quality_metrics$quality_score <- max(0, round(100 - (critical_issues / quality_metrics$total_records * 100), 1))
  
  return(quality_metrics)
}

#' Process length data from hauls
#'
#' @description
#' Processes length data from PRODUCE logbooks. Automatically detects required
#' columns and transforms from long to wide format.
#'
#' @param data_length Data frame with raw length data.
#' @param verbose Logical. Print column mapping information.
#'
#' @return Data frame with length by haul in wide format with individual columns
#'   for each length class.
#'
#' @examples
#' length_data <- process_length(data_length = tallas_bitacora)
#' length_data <- process_length(data_length = tallas_bitacora, verbose = TRUE)
#'
#' @export
#' @importFrom dplyr select mutate
#' @importFrom stringi stri_trim
#' @importFrom tidyr pivot_wider
#' @importFrom stats na.omit
#' @importFrom utils head
process_length <- function(data_length, verbose = FALSE) {
  
  if (!is.data.frame(data_length)) {
    stop("'data_length' must be a data frame.")
  }
  
  if (nrow(data_length) == 0) {
    stop("Input data frame is empty.")
  }
  
  column_patterns <- list(
    fishing_trip_code = c("codigo.*faena", "trip.*code", "faena", "codigo_faena", "viaje"),
    haul_number = c("numero.*cala", "haul.*number", "cala", "numero_cala", "lance"),
    species = c("especie", "species", "sp", "nombre_cientifico", "nombre_comun"),
    length = c("talla", "length", "longitud", "size", "cm"),
    freq = c("frecuencia", "frequency", "freq", "count", "cantidad", "numero")
  )
  
  critical_columns <- c("fishing_trip_code", "haul_number", "species", "length", "freq")
  
  original_names <- names(data_length)
  column_mapping <- list()
  missing_critical <- character()
  
  if (verbose) {
    message("=== Length Data Column Mapping Process ===")
  }
  
  for (col_name in names(column_patterns)) {
    col_index <- find_column(column_patterns[[col_name]], original_names, verbose = verbose)
    
    if (!is.null(col_index)) {
      column_mapping[[col_name]] <- col_index
      if (verbose) {
        message(sprintf("Mapped '%s' -> '%s' (column %d)", 
                       col_name, original_names[col_index], col_index))
      }
    } else {
      missing_critical <- c(missing_critical, col_name)
      if (verbose) {
        message(sprintf("Column '%s' not found", col_name))
      }
    }
  }
  
  if (length(missing_critical) > 0) {
    stop(sprintf("Required columns not found: %s\nAll columns are mandatory for length data processing.", 
                paste(missing_critical, collapse = ", ")))
  }
  
  found_indices <- unlist(column_mapping)
  selected_data <- data_length[, found_indices, drop = FALSE]
  names(selected_data) <- names(column_mapping)
  selected_data <- selected_data[, critical_columns]
  
  if (verbose) {
    message("\n=== Length Data Processing ===")
  }
  
  selected_data <- selected_data %>%
    dplyr::mutate(
      species = stringi::stri_trim(as.character(.data[["species"]])),
      fishing_trip_code = stringi::stri_trim(as.character(.data[["fishing_trip_code"]])),
      haul_number = safe_numeric_conversion(.data[["haul_number"]]),
      length = safe_numeric_conversion(.data[["length"]]),
      freq = safe_numeric_conversion(.data[["freq"]])
    )
  
  initial_rows <- nrow(selected_data)
  selected_data <- selected_data[!is.na(selected_data$length) & !is.na(selected_data$freq), ]
  removed_rows <- initial_rows - nrow(selected_data)
  
  if (verbose && removed_rows > 0) {
    message(sprintf("! Removed %d rows with invalid length or frequency values", removed_rows))
  }
  
  length_order <- sort(unique(stats::na.omit(selected_data$length)))
  
  if (verbose) {
    message(sprintf("Found %d unique length classes: %s", 
                   length(length_order), 
                   paste(head(length_order, 10), collapse = ", ")))
    if (length(length_order) > 10) {
      message("  (showing first 10 only)")
    }
  }
  
  wide_data <- tidyr::pivot_wider(
    selected_data,
    names_from = .data[["length"]],
    values_from = .data[["freq"]],
    values_fill = list(freq = 0)
  )
  
  fixed_columns <- c("fishing_trip_code", "haul_number", "species")
  ordered_length_columns <- as.character(length_order)
  final_data <- wide_data[, c(fixed_columns, ordered_length_columns)]
  
  if (verbose) {
    message("\n=== Length Data Processing Summary ===")
    message(sprintf("Records processed: %d -> %d (long to wide)", initial_rows, nrow(final_data)))
    message(sprintf("Length classes created: %d columns", length(length_order)))
    message(sprintf("Total output columns: %d", ncol(final_data)))
  }
  
  return(as.data.frame(final_data))
}

#' Validate processed length data quality
#'
#' @description
#' Validates data quality metrics for processed length data.
#'
#' @param processed_length Data frame returned by process_length().
#'
#' @return List with data quality metrics including completeness scores and distribution statistics.
#'
#' @export
validate_length_data <- function(processed_length) {
  
  if (!is.data.frame(processed_length)) {
    stop("Input must be a data frame from process_length()")
  }
  
  fixed_cols <- c("fishing_trip_code", "haul_number", "species")
  length_cols <- setdiff(names(processed_length), fixed_cols)
  numeric_length_cols <- length_cols[!is.na(safe_numeric_conversion(length_cols))]
  
  quality_metrics <- list(
    total_records = nrow(processed_length),
    total_length_classes = length(numeric_length_cols),
    missing_trip_codes = sum(is.na(processed_length$fishing_trip_code) | 
                            processed_length$fishing_trip_code == ""),
    missing_haul_numbers = sum(is.na(processed_length$haul_number)),
    missing_species = sum(is.na(processed_length$species) | 
                         processed_length$species == ""),
    min_length_class = min(safe_numeric_conversion(numeric_length_cols), na.rm = TRUE),
    max_length_class = max(safe_numeric_conversion(numeric_length_cols), na.rm = TRUE),
    length_range = max(safe_numeric_conversion(numeric_length_cols), na.rm = TRUE) - 
                   min(safe_numeric_conversion(numeric_length_cols), na.rm = TRUE),
    duplicate_hauls = sum(duplicated(paste(processed_length$fishing_trip_code, 
                                          processed_length$haul_number, 
                                          processed_length$species))),
    zero_frequency_records = sum(rowSums(processed_length[, numeric_length_cols, drop = FALSE], na.rm = TRUE) == 0),
    total_individuals = sum(processed_length[, numeric_length_cols], na.rm = TRUE),
    processing_timestamp = Sys.time()
  )
  
  critical_issues <- quality_metrics$missing_trip_codes + 
                    quality_metrics$missing_haul_numbers + 
                    quality_metrics$missing_species +
                    quality_metrics$duplicate_hauls
  
  quality_metrics$quality_score <- max(0, round(100 - (critical_issues / quality_metrics$total_records * 100), 1))
  
  return(quality_metrics)
}