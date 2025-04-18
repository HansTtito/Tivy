#' Length - Weight Relationship
#'
#' This function estimates the weight of an individual from its length, using
#' the general formula of the length-weight relationship: \eqn{W = a \cdot L^b}, where
#' \eqn{W} is the weight, \eqn{L} the length (length), \eqn{a} and \eqn{b} the specific parameters.
#'
#' @param length A numeric vector containing the lengths of the individuals.
#' @param a Numeric value of the coefficient of the length-weight relationship.
#' @param b Numeric value of the exponent of the length-weight relationship.
#' @return A numeric vector with the estimated weights.
#' @export
#' @examples
#'
#' lengths <- seq(5, 20, by = 0.5)
#' a <- 0.0001
#' b <- 2.984
#'
#' weights <- length_weight(length = lengths, a = a, b = b)
#'
#' print(weights)
length_weight <- function(length, a, b) {
  # Parameter validation
  if (!is.numeric(length)) stop("The 'length' parameter must be numeric.")
  if (!is.numeric(a)) stop("The 'a' parameter must be numeric.")
  if (!is.numeric(b)) stop("The 'b' parameter must be numeric.")

  if (any(length <= 0, na.rm = TRUE)) warning("Length values <= 0 were detected, which could produce invalid results.")
  if (a <= 0) warning("The value of 'a' is <= 0, which could produce biologically implausible results.")

  return(a * length^b)
}



#' Weighting length according to total catch
#'
#' Calculates a weighting of the sampled length based on the total recorded catch.
#' This allows scaling the observed frequency to the total catch.
#'
#' @param frequency A numeric vector with the frequency of length observed in the sampling.
#' @param catch Numeric value with the total catch (in kg or tons, as appropriate).
#' @param length A numeric vector with the length corresponding to the frequencies.
#' @param a Numeric value of the coefficient of the length-weight relationship.
#' @param b Numeric value of the exponent of the length-weight relationship.
#' @param silence_warnings Logical. If TRUE, warning messages are suppressed (default = FALSE).
#' @return A numeric vector with the weighted frequencies.
#' @export
#' @examples
#'
#' frequency <- c(0, 1, 4, 8, 16, 12, 23, 34, 55, 35, 24, 15, 10, 6, 3, 2)
#'
#' length <- seq(5, 20, by = 1)
#'
#' catch <- 1000
#' a <- 0.0001
#' b <- 2.984
#'
#' weighted <- weighting(frequency, catch, length, a, b)
#'
#' print(head(weighted))
weighting <- function(frequency, catch, length, a, b, silence_warnings = FALSE) {
  # Parameter validation
  if (!is.numeric(frequency)) stop("The 'frequency' parameter must be numeric.")
  if (!is.numeric(length)) stop("The 'length' parameter must be numeric.")
  if (!is.numeric(catch)) stop("The 'catch' parameter must be numeric.")
  if (!is.numeric(a)) stop("The 'a' parameter must be numeric.")
  if (!is.numeric(b)) stop("The 'b' parameter must be numeric.")
  if (length(frequency) != length(length)) {
    stop("The 'frequency' and 'length' vectors must have the same length.")
  }

  # Accumulate warning messages
  warning_messages <- character()

  # Catch validation
  if (is.na(catch) || catch <= 0) {
    warning_messages <- c(warning_messages,
                          "The value of 'catch' is NA or <= 0, catch = 1 will be used.")
    catch <- 1
  }

  # length validation
  if (any(length <= 0, na.rm = TRUE)) {
    warning_messages <- c(warning_messages,
                          "There are length <= 0, invalid results could be produced.")
  }

  # Frequency validation
  frequency[is.na(frequency)] <- 0
  if (sum(frequency, na.rm = TRUE) == 0) {
    warning_messages <- c(warning_messages,
                          "The sum of frequencies is zero. A vector of zeros will be returned.")
    if (!silence_warnings && length(warning_messages) > 0)
      warning(paste(warning_messages, collapse = " | "))
    return(rep(0, length(length)))
  }

  # Weight calculation
  weight <- length_weight(length = length, a = a, b = b) * frequency
  weight_sum <- sum(weight, na.rm = TRUE)
  if (weight_sum == 0) {
    warning_messages <- c(warning_messages,
                          "The sum of weights is zero. A vector of zeros will be returned.")
    if (!silence_warnings && length(warning_messages) > 0)
      warning(paste(warning_messages, collapse = " | "))
    return(rep(0, length(length)))
  }

  # Show accumulated warnings (if they exist)
  if (!silence_warnings && length(warning_messages) > 0) {
    warning(paste(warning_messages, collapse = " | "))
  }

  # Final calculation
  weighted_length <- (catch / weight_sum) * frequency
  return(weighted_length)
}


#' Weighting length in a data frame
#'
#' This function performs the weighting of length frequencies according to the total catch
#' using the length-weight relationship. It allows parallel processing for large datasets.
#'
#' @param df A data frame containing the length columns and catch.
#' @param length_cols A character vector with the names of the columns that represent the length.
#' @param catch_col Name of the column that contains the catch values.
#' @param a Numeric value of the coefficient of the length-weight relationship.
#' @param b Numeric value of the exponent of the length-weight relationship.
#' @param parallel Boolean indicating whether to use parallel processing.
#' @param num_cores Number of cores to use (if parallel=TRUE).
#' @param block_size Size of the blocks for processing (if parallel=TRUE).
#' @param silence_warnings Logical. If TRUE, warning messages are suppressed (default = TRUE).
#'
#' @return A data frame with the original columns and the weighted length columns.
#'         The weighted columns will have the prefix "pond_" followed by the original name.
#'
#' @examples
#'
#' data(calas_bitacora, faenas_bitacora, tallas_bitacora)
#'
#' data_hauls <- process_hauls(data_hauls = calas_bitacora)
#' data_fishing_trips <- process_fishing_trips(data_fishing_trips = faenas_bitacora)
#' hauls_length <- process_length(data_length = tallas_bitacora)
#'
#' data_length_fishing_trips <- merge(
#'    x = data_fishing_trips, 
#'    y = hauls_length, 
#'    by = 'fishing_trip_code'
#' )
#'
#' data_total <- merge_length_fishing_trips_hauls(
#'    data_hauls = data_hauls, 
#'    data_length_fishing_trips = data_length_fishing_trips
#' )
#'
#' length_columns <- c("8", "8.5", "9", "9.5", "10", "10.5",
#'                     "11", "11.5","12", "12.5", "13", "13.5",
#'                     "14", "14.5", "15")
#'
#' # Sequential processing
#' results <- weight_length_df(
#'    df = data_total, 
#'    length_cols = length_columns, 
#'    catch_col = "catch_ANCHOVETA", 
#'    a= 0.0001,
#'    b = 2.984
#' )
#'
#' print(head(results))
#'
#' # Parallel processing for large datasets
#' parallel_results <- weight_length_df(
#'    df = data_total, 
#'    length_cols = length_columns, 
#'    catch_col = "catch_ANCHOVETA", 
#'    a = 0.0001,
#'    b = 2.984,
#'    parallel = TRUE
#' )
#'
#' print(head(parallel_results))
#'
#' @import parallel
#' @export
weight_length_df <- function(df, length_cols, catch_col, a, b,
                               parallel = FALSE, num_cores = NULL, block_size = 10000,
                               silence_warnings = TRUE) {
  # Initial validations
  if (!is.data.frame(df)) {
    stop("The first argument must be a data frame.")
  }
  if (!all(length_cols %in% names(df))) {
    stop("Some length columns do not exist in the data frame.")
  }
  if (!(catch_col %in% names(df))) {
    stop("The catch column does not exist in the data frame.")
  }

  # If parallel processing is requested
  if (parallel) {
    # Check required packages
    if (!requireNamespace("future", quietly = TRUE) ||
        !requireNamespace("future.apply", quietly = TRUE)) {
      stop("future and future.apply packages needed for parallel processing. Install them.")
    }

    # Configure parallel processing
    if (is.null(num_cores)) {
      num_cores <- max(1, parallel::detectCores() / 2)
    }
    future::plan(future::multisession, workers = num_cores)

    # Split into blocks
    num_rows <- nrow(df)
    block_indices <- split(1:num_rows,
                             ceiling(seq_along(1:num_rows) / block_size))

    # Process blocks in parallel
    results <- future.apply::future_lapply(
      block_indices,
      function(indices) {
        block <- df[indices, ]
        process_block(block, length_cols, catch_col, a, b, silence_warnings = TRUE)
      },
      future.seed = TRUE
    )

    # Combine results
    final_result <- do.call(rbind, results)
    future::plan(future::sequential)

    # Show warnings summary at the end if not silenced
    if (!silence_warnings) {
      # Here we could add a summary message if needed
      # but as the processing is parallel, it would be more complex to get exact counts
      message("Parallel processing completed. Some rows might have NA or zero values for weighted length.")
    }

    return(final_result)
  } else {
    # Sequential processing
    return(process_block(df, length_cols, catch_col, a, b, silence_warnings))
  }
}



#' Percentage of juveniles
#'
#' Estimates the percentage of individuals considered juveniles in a sample
#' according to an established length limit.
#'
#' @param frequency A numeric vector with the frequency of sampled length.
#' @param length Vector of length corresponding to the frequencies.
#' @param juvLim length limit (default 12 cm) to classify as juvenile.
#' @param silence_warnings Logical. If TRUE, warning messages are suppressed (default = FALSE).
#' @return Percentage of juveniles in the sample.
#' @export
#' @examples
#'
#' frequency <- c(0, 1, 4, 8, 16, 12, 23, 34, 55, 35, 24, 15, 10, 6, 3, 2)
#'
#' length <- seq(5, 20, by = 1)
#'
#' perc <- juvenile_percentage(frequency, length, juvLim = 12)
#'
#' print(perc)
juvenile_percentage <- function(frequency, length, juvLim = 12, silence_warnings = FALSE) {
  # Parameter validation
  if (!is.numeric(frequency)) stop("The 'frequency' parameter must be numeric.")
  if (!is.numeric(length)) stop("The 'length' parameter must be numeric.")
  if (!is.numeric(juvLim)) stop("The 'juvLim' parameter must be numeric.")
  if (length(frequency) != length(length)) {
    stop("The 'frequency' and 'length' vectors must have the same length.")
  }
  if (juvLim <= 0 && !silence_warnings)
    warning("The value of 'juvLim' is <= 0, which may not be biologically plausible.")

  total_frequency <- sum(frequency, na.rm = TRUE)
  if (total_frequency == 0) {
    if (!silence_warnings) warning("The sum of frequencies is zero. NA will be returned.")
    return(NA_real_)
  }

  juv <- 100 * (sum(frequency[length < juvLim], na.rm = TRUE) / total_frequency)
  return(juv)
}



#' Minimum observed length with positive frequency
#'
#' @param frequency A numeric vector with the frequencies of length.
#' @param length Corresponding length vector.
#' @return Minimum length value with frequency greater than zero.
#' @export
#' @examples
#' min_range(frequency = c(0,0,1,2,3), length = c(5,6,7,8,9))
min_range <- function(frequency, length) {
  # Parameter validation
  if (!is.numeric(frequency)) stop("The 'frequency' parameter must be numeric.")
  if (!is.numeric(length)) stop("The 'length' parameter must be numeric.")

  if (length(frequency) != length(length)) {
    stop("The 'frequency' and 'length' vectors must have the same length.")
  }

  # Check if there are positive frequencies
  if (all(frequency <= 0, na.rm = TRUE) || all(is.na(frequency))) {
    warning("No positive frequencies. NA will be returned.")
    return(NA_real_)
  }

  frequency[frequency <= 0] <- NA
  return(min(length[!is.na(frequency)], na.rm = TRUE))
}



#' Maximum observed length with positive frequency
#'
#' @param frequency A numeric vector with the frequencies of length.
#' @param length Corresponding length vector.
#' @return Maximum length value with frequency greater than zero.
#' @export
#' @examples
#' max_range(frequency = c(0,0,1,2,3), length = c(5,6,7,8,9))
max_range <- function(frequency, length) {
  # Parameter validation
  if (!is.numeric(frequency)) stop("The 'frequency' parameter must be numeric.")
  if (!is.numeric(length)) stop("The 'length' parameter must be numeric.")

  if (length(frequency) != length(length)) {
    stop("The 'frequency' and 'length' vectors must have the same length.")
  }

  # Check if there are positive frequencies
  if (all(frequency <= 0, na.rm = TRUE) || all(is.na(frequency))) {
    warning("No positive frequencies. NA will be returned.")
    return(NA_real_)
  }

  frequency[frequency <= 0] <- NA
  return(max(length[!is.na(frequency)], na.rm = TRUE))
}



#' Conversion from number of individuals to weight
#'
#' Converts numerical length frequencies in a data.frame to weight estimates,
#' using the length-weight relationship.
#'
#' @param data Data frame where columns with names equal to length contain frequencies.
#' @param length Vector of length (which must match the column names of `data`).
#' @param a Numeric value of the coefficient of the length-weight relationship.
#' @param b Numeric value of the exponent of the length-weight relationship.
#' @return Data frame with the same dimensions but expressed in weight.
#' @export
#' @importFrom stats setNames
#' @examples
#'
#' data(calas_bitacora, faenas_bitacora, tallas_bitacora)
#'
#' # Process data
#' data_hauls <- process_hauls(data_hauls = calas_bitacora)
#' data_fishing_trips <- process_fishing_trips(data_fishing_trips = faenas_bitacora)
#' hauls_length <- process_length(data_length = tallas_bitacora)
#'
#' # Integrate data
#' data_length_fishing_trips <- merge(
#'    x = data_fishing_trips, 
#'    y = hauls_length, 
#'    by = 'fishing_trip_code'
#' )
#' 
#' data_total <- merge_length_fishing_trips_hauls(
#'    data_hauls = data_hauls,
#'    data_length_fishing_trips = data_length_fishing_trips
#' )
#' 
#' final_data <- add_variables(data_total)
#'
#' # Define length columns
#' length_cols <- c("8", "8.5", "9", "9.5", "10", "10.5", "11", "11.5",
#'                  "12", "12.5", "13", "13.5", "14", "14.5", "15")
#'
#' # Weight length
#' results <- weight_length_df(df = final_data,
#'                                length_cols = length_cols,
#'                                catch_col = "catch_ANCHOVETA",
#'                                a = 0.0001,
#'                                b = 2.984)
#'
#'number_to_weight(data = results, 
#'    length = paste0('pond_', length_cols), 
#'    a = 0.0012, 
#'    b = 3.1242
#' )
number_to_weight <- function(data, length, a, b) {
  # Parameter validation
  if (!is.data.frame(data)) stop("The 'data' parameter must be a data.frame.")
  if (!is.numeric(a)) stop("The 'a' parameter must be numeric.")
  if (!is.numeric(b)) stop("The 'b' parameter must be numeric.")

  if (is.character(length)) {
    length_num <- extract_length_values(length)
  }

  # Validation that length are present in the data.frame
  length_present <- length %in% colnames(data)
  if (!all(length_present)) {
    missing_length <- length[!length_present]
    stop("The following length are not present as columns in the data.frame: ",
         paste(missing_length, collapse = ", "))
  }

  # Check that length columns contain numeric values
  for (length_col in length) {
    if (!is.numeric(data[[length_col]])) {
      data[[length_col]] <- as.numeric(data[[length_col]])
      warning("Column '", length_col, "' has been converted to numeric.")
    }
  }

  # Weight calculation
  tryCatch({
    weights <- as.data.frame(t(apply(data[, length, drop = FALSE], 1, function(x) {
      length_weight(length = length_num, a = a, b = b) * x
    })))

    # Rename columns with prefix "weight_"
    colnames(weights) <- paste0("weight_", length)

    # Add weight columns to the original data
    result <- cbind(data, weights)

    return(result)
  }, error = function(e) {
    stop("Error calculating weights: ", e$message)
  })
}


#' Calculation of juvenile percentage by groups
#'
#' @description
#' Calculates the percentage of juveniles by specified groups, both in number and
#' in weight. Uses a modern approach with dplyr to process the data and calculate
#' the proportions of juveniles based on length frequencies.
#'
#' @param data Data frame with length frequency data.
#' @param group_cols Vector of column names to group the data.
#' @param cols_length Vector of column names or indices that contain the
#'   length frequencies. Can be names with patterns like "pond_X", "length_X" or "X".
#' @param juvLim length limit to consider juveniles (default 12 cm).
#' @param a Coefficient of the length-weight relationship (default 0.0012).
#' @param b Exponent of the length-weight relationship (default 3.1242).
#' @param remove_empty Logical. If TRUE (default), removes groups
#'   without data (total_number = 0).
#'
#' @return Data frame with the following fields:
#'   \itemize{
#'     \item Grouping columns specified in group_cols
#'     \item perc_juv_number: Percentage of juveniles in number
#'     \item perc_juv_weight: Percentage of juveniles in weight
#'     \item total_number: Total number of individuals in the group
#'     \item total_weight: Total weight in the group
#'   }
#'
#' @export
#' @importFrom dplyr group_by_at reframe across everything filter pick
#' @importFrom tidyr unnest
#'
#' @examples
#' # Load sample data
#' data(calas_bitacora, faenas_bitacora, tallas_bitacora)
#'
#' # Process data
#' data_hauls <- process_hauls(data_hauls = calas_bitacora)
#' data_fishing_trips <- process_fishing_trips(data_fishing_trips = faenas_bitacora)
#' hauls_length <- process_length(data_length = tallas_bitacora)
#'
#' # Integrate data
#' data_length_fishing_trips <- merge(
#'    x = data_fishing_trips, 
#'    y = hauls_length, 
#'    by = 'fishing_trip_code'
#' )
#' 
#' data_total <- merge_length_fishing_trips_hauls(
#'    data_hauls = data_hauls,
#'    data_length_fishing_trips = data_length_fishing_trips
#' )
#' final_data <- add_variables(data_total)
#'
#' # Define length columns
#' length_cols <- c("8", "8.5", "9", "9.5", "10", "10.5", "11", "11.5",
#'                  "12", "12.5", "13", "13.5", "14", "14.5", "15")
#'
#' # Weight length
#' results <- weight_length_df(df = final_data,
#'                                length_cols = length_cols,
#'                                catch_col = "catch_ANCHOVETA",
#'                                a = 0.0001,
#'                                b = 2.984)
#'
#' # Add date column for grouping
#' results$unique_date <- convert_to_date(results$start_date_haul, type = "date")
#'
#' # Calculate juveniles by date
#' results_by_date <- juveniles_by_group(
#'   data = results,
#'   group_cols = "unique_date",
#'   cols_length = paste0("pond_", length_cols),
#'   juvLim = 12,
#'   a = 0.0012,
#'   b = 3.1242
#' )
#'
#' head(results_by_date)
#'
#' # Calculate juveniles by date and distance to coast
#' results_date_dc <- juveniles_by_group(
#'   data = results,
#'   group_cols = c("unique_date", "dc_cat"),
#'   cols_length = paste0("pond_", length_cols),
#'   juvLim = 12,
#'   a = 0.0012,
#'   b = 3.1242
#' )
#'
#' # View results
#' head(results_date_dc)
juveniles_by_group <- function(data, group_cols, cols_length, juvLim = 12, a = 0.0012, b = 3.1242,
                                remove_empty = TRUE) {
  # Parameter validation
  if (!is.data.frame(data)) stop("The 'data' parameter must be a data.frame.")
  if (!all(group_cols %in% colnames(data)))
    stop("Not all grouping columns are in the data.frame.")

  # Determine if cols_length contains names or indices
  if (is.numeric(cols_length)) {
    # If they are numeric indices, get the corresponding names
    if (any(cols_length > ncol(data) | cols_length < 1))
      stop("Some of the indices in cols_length are outside the range of the data.frame.")

    # Convert indices to names to work uniformly
    cols_names <- names(data)[cols_length]
  } else {
    # If they are already names, check that they exist in the data.frame
    if (!all(cols_length %in% colnames(data)))
      stop("Not all length columns are in the data.frame.")

    cols_names <- cols_length
  }

  # Ensure that length columns are numeric
  data <- data %>%
    dplyr::mutate(dplyr::across(all_of(cols_names), ~as.numeric(.x)))

  # Extract numerical length values from column names
  # Only if the names seem to contain length information (e.g., "pond_8.5", "8", "length_9")
  if (all(grepl("^(pond_)?([0-9]+(\\.[0-9]+)?)$|^length_[0-9]+(\\.[0-9]+)?$", cols_names))) {
    # Extract numerical values removing common prefixes
    length_values <- as.numeric(gsub("^(pond_|length_)?", "", cols_names))
  } else if (is.numeric(cols_length)) {
    # If cols_length were originally numeric and don't appear to be length patterns,
    # use the original values
    length_values <- cols_length
  } else {
    # Otherwise, try to convert the names directly to numeric
    length_values <- suppressWarnings(as.numeric(cols_names))

    # If the conversion doesn't work (generating NAs), use sequential numbers
    if (anyNA(length_values)) {
      warning("Could not determine numerical length values from column names. Using sequence 1:n.")
      length_values <- seq_along(cols_names)
    }
  }

  # Function to process each group using calculate_juveniles
  process_group <- function(df) {
    # Check if the dataframe is empty or all frequencies are zero
    if (nrow(df) == 0) {
      return(data.frame(
        perc_juv_number = NA_real_,
        perc_juv_weight = NA_real_,
        total_number = 0,
        total_weight = 0
      ))
    }

    # Extract and sum frequencies by length
    frequencies <- colSums(df[, cols_names, drop = FALSE], na.rm = TRUE)

    # Check if all frequencies are zero
    if (all(frequencies == 0)) {
      return(data.frame(
        perc_juv_number = NA_real_,
        perc_juv_weight = NA_real_,
        total_number = 0,
        total_weight = 0
      ))
    }

    # Call the external function to calculate juveniles
    calculate_juveniles(frequencies, length_values, juvLim, a, b)
  }

  # If there are no grouping columns, calculate for the entire set
  if (length(group_cols) == 0) {
    return(process_group(data))
  }

  # Group and calculate - updated version without using cur_data()
  results <- data %>%
    dplyr::group_by(dplyr::across(all_of(group_cols))) %>%
    dplyr::reframe(
      result = list(
        {
          # Use pick() to select the length columns
          df_group <- dplyr::pick(all_of(cols_names))
          # Add rows to complete the data.frame
          df_group <- as.data.frame(df_group)
          process_group(df_group)
        }
      )
    ) %>%
    tidyr::unnest(.data$result)

  # Optionally remove groups without data
  if (remove_empty && any(results$total_number == 0, na.rm = TRUE)) {
    results <- results %>%
      dplyr::filter(.data$total_number > 0)
  }

  return(as.data.frame(results))
}