#' Calculate fish weight from length
#'
#' @description
#' Estimates individual fish weight from length using the length-weight relationship: W = a * L^b
#'
#' @param length Numeric vector of fish lengths.
#' @param a Coefficient of the length-weight relationship.
#' @param b Exponent of the length-weight relationship.
#'
#' @return Numeric vector of estimated weights.
#'
#' @examples
#' lengths <- seq(8, 20, by = 0.5)
#' weights <- calculate_fish_weight(length = lengths, a = 0.0001, b = 2.984)
#'
#' @export
calculate_fish_weight <- function(length, a, b) {
  if (!is.numeric(length)) stop("'length' must be numeric.")
  if (!is.numeric(a)) stop("'a' must be numeric.")
  if (!is.numeric(b)) stop("'b' must be numeric.")
  
  if (any(length <= 0, na.rm = TRUE)) {
    warning("Length values <= 0 detected, which may produce invalid results.")
  }
  if (a <= 0) {
    warning("Value of 'a' <= 0 may produce biologically implausible results.")
  }
  
  return(a * length^b)
}

#' Weight length frequencies by total catch
#'
#' @description
#' Scales observed length frequencies based on total recorded catch using length-weight relationship.
#'
#' @param frequency Numeric vector of observed length frequencies.
#' @param catch Total catch amount (in kg or tons).
#' @param length Numeric vector of lengths corresponding to frequencies.
#' @param a Coefficient of the length-weight relationship.
#' @param b Exponent of the length-weight relationship.
#' @param silence_warnings Logical. Suppress warning messages.
#'
#' @return Numeric vector of weighted frequencies.
#'
#' @examples
#' freq <- c(0, 4, 8, 16, 12, 23, 34, 55, 35, 24, 15, 6, 0)
#' lengths <- seq(8, 20, by = 1)
#' weighted_freq <- weight_by_catch(freq, catch = 1000, lengths, a = 0.0001, b = 2.984)
#'
#' @export
weight_by_catch <- function(frequency, catch, length, a, b, silence_warnings = FALSE) {
  if (!is.numeric(frequency)) stop("'frequency' must be numeric.")
  if (!is.numeric(length)) stop("'length' must be numeric.")
  if (!is.numeric(catch)) stop("'catch' must be numeric.")
  if (!is.numeric(a)) stop("'a' must be numeric.")
  if (!is.numeric(b)) stop("'b' must be numeric.")
  
  if (length(frequency) != length(length)) {
    stop("'frequency' and 'length' vectors must have the same length.")
  }
  
  warning_messages <- character()
  
  if (is.na(catch) || catch <= 0) {
    warning_messages <- c(warning_messages,
                          "Catch value is NA or <= 0, using catch = 1.")
    catch <- 1
  }
  
  if (any(length <= 0, na.rm = TRUE)) {
    warning_messages <- c(warning_messages,
                          "Length values <= 0 detected, may produce invalid results.")
  }
  
  frequency[is.na(frequency)] <- 0
  if (sum(frequency, na.rm = TRUE) == 0) {
    warning_messages <- c(warning_messages,
                          "Sum of frequencies is zero. Returning vector of zeros.")
    if (!silence_warnings && length(warning_messages) > 0) {
      warning(paste(warning_messages, collapse = " | "))
    }
    return(rep(0, length(length)))
  }
  
  weight <- calculate_fish_weight(length = length, a = a, b = b) * frequency
  weight_sum <- sum(weight, na.rm = TRUE)
  
  if (weight_sum == 0) {
    warning_messages <- c(warning_messages,
                          "Sum of weights is zero. Returning vector of zeros.")
    if (!silence_warnings && length(warning_messages) > 0) {
      warning(paste(warning_messages, collapse = " | "))
    }
    return(rep(0, length(length)))
  }
  
  if (!silence_warnings && length(warning_messages) > 0) {
    warning(paste(warning_messages, collapse = " | "))
  }
  
  weighted_length <- (catch / weight_sum) * frequency
  return(weighted_length)
}

#' Apply catch weighting to data frame
#'
#' @description
#' Applies catch weighting to length frequency columns in a data frame.
#' Supports parallel processing for large datasets.
#'
#' @param data Data frame containing length columns and catch data.
#' @param length_cols Character vector of length column names.
#' @param catch_col Name of the catch column.
#' @param a Coefficient of the length-weight relationship.
#' @param b Exponent of the length-weight relationship.
#' @param parallel Logical. Use parallel processing.
#' @param num_cores Number of cores for parallel processing. If NULL, auto-detect.
#' @param block_size Block size for parallel processing.
#' @param silence_warnings Logical. Suppress warnings.
#'
#' @return Data frame with original columns plus weighted length columns (prefixed with "weighted_").
#'
#' @examples
#' \dontrun{
#' length_cols <- c("8", "8.5", "9", "9.5", "10", "10.5", "11", "11.5")
#' weighted_data <- apply_catch_weighting(
#'   data = fishery_data,
#'   length_cols = length_cols,
#'   catch_col = "total_catch",
#'   a = 0.0001,
#'   b = 2.984
#' )
#' }
#'
#' @export
#' @import parallel
apply_catch_weighting <- function(data, 
                                  length_cols, 
                                  catch_col, 
                                  a, 
                                  b,
                                  parallel = FALSE, 
                                  num_cores = NULL, 
                                  block_size = 10000,
                                  silence_warnings = TRUE) {
  
  if (!is.data.frame(data)) {
    stop("First argument must be a data.frame.")
  }
  if (!all(length_cols %in% names(data))) {
    stop("Some length columns do not exist in the data frame.")
  }
  if (!(catch_col %in% names(data))) {
    stop("Catch column does not exist in the data frame.")
  }
  
  if (parallel) {
    if (!requireNamespace("future", quietly = TRUE) ||
        !requireNamespace("future.apply", quietly = TRUE)) {
      stop("future and future.apply packages needed for parallel processing.")
    }
    
    if (is.null(num_cores)) {
      num_cores <- max(1, parallel::detectCores() / 2)
    }
    future::plan(future::multisession, workers = num_cores)
    
    num_rows <- nrow(data)
    block_indices <- split(1:num_rows, ceiling(seq_along(1:num_rows) / block_size))
    
    results <- future.apply::future_lapply(
      block_indices,
      function(indices) {
        block <- data[indices, ]
        process_weighting_block(block, length_cols, catch_col, a, b, silence_warnings = TRUE)
      },
      future.seed = TRUE
    )
    
    final_result <- do.call(rbind, results)
    future::plan(future::sequential)
    
    if (!silence_warnings) {
      message("Parallel processing completed. Some rows might have NA or zero values for weighted length.")
    }
    
    return(final_result)
  } else {
    return(process_weighting_block(data, length_cols, catch_col, a, b, silence_warnings))
  }
}

#' Calculate juvenile percentage
#'
#' @description
#' Calculates the percentage of individuals considered juveniles based on a length threshold.
#'
#' @param frequency Numeric vector of length frequencies.
#' @param length Numeric vector of corresponding lengths.
#' @param juvenile_limit Length threshold for juvenile classification.
#' @param silence_warnings Logical. Suppress warnings.
#'
#' @return Percentage of juveniles in the sample.
#'
#' @examples
#' freq <- c(0, 4, 8, 16, 12, 23, 34, 55, 35, 24, 15, 6, 0)
#' lengths <- seq(8, 20, by = 1)
#' juv_pct <- calculate_juvenile_percentage(freq, lengths, juvenile_limit = 12)
#'
#' @export
calculate_juvenile_percentage <- function(frequency, length, juvenile_limit = 12, silence_warnings = FALSE) {
  if (!is.numeric(frequency)) stop("'frequency' must be numeric.")
  if (!is.numeric(length)) stop("'length' must be numeric.")
  if (!is.numeric(juvenile_limit)) stop("'juvenile_limit' must be numeric.")
  
  if (length(frequency) != length(length)) {
    stop("'frequency' and 'length' vectors must have the same length.")
  }
  
  if (juvenile_limit <= 0 && !silence_warnings) {
    warning("juvenile_limit <= 0 may not be biologically plausible.")
  }
  
  total_frequency <- sum(frequency, na.rm = TRUE)
  if (total_frequency == 0) {
    if (!silence_warnings) warning("Sum of frequencies is zero. Returning NA.")
    return(NA_real_)
  }
  
  juvenile_freq <- sum(frequency[length < juvenile_limit], na.rm = TRUE)
  juvenile_pct <- 100 * (juvenile_freq / total_frequency)
  
  return(juvenile_pct)
}

#' Get length range from frequencies
#'
#' @description
#' Finds the minimum or maximum length with positive frequency.
#'
#' @param frequency Numeric vector of length frequencies.
#' @param length Numeric vector of corresponding lengths.
#' @param type Either "min" or "max" to specify which range to return.
#'
#' @return Minimum or maximum length value with frequency > 0.
#'
#' @examples
#' freq <- c(0, 0, 1, 2, 3, 4, 2, 1, 0)
#' lengths <- c(5, 6, 7, 8, 9, 10, 11, 12, 13)
#' min_length <- get_length_range(freq, lengths, type = "min")
#' max_length <- get_length_range(freq, lengths, type = "max")
#'
#' @export
get_length_range <- function(frequency, length, type = "min") {
  if (!is.numeric(frequency)) stop("'frequency' must be numeric.")
  if (!is.numeric(length)) stop("'length' must be numeric.")
  if (!type %in% c("min", "max")) stop("'type' must be 'min' or 'max'.")
  
  if (length(frequency) != length(length)) {
    stop("'frequency' and 'length' vectors must have the same length.")
  }
  
  if (all(frequency <= 0, na.rm = TRUE) || all(is.na(frequency))) {
    warning("No positive frequencies found. Returning NA.")
    return(NA_real_)
  }
  
  valid_lengths <- length[frequency > 0 & !is.na(frequency)]
  
  if (length(valid_lengths) == 0) {
    return(NA_real_)
  }
  
  if (type == "min") {
    return(min(valid_lengths, na.rm = TRUE))
  } else {
    return(max(valid_lengths, na.rm = TRUE))
  }
}

#' Convert numbers to weight
#'
#' @description
#' Converts numerical length frequencies to weight estimates using length-weight relationship.
#'
#' @param data Data frame with length frequency columns.
#' @param length_cols Vector of length column names or numeric values.
#' @param a Coefficient of the length-weight relationship.
#' @param b Exponent of the length-weight relationship.
#'
#' @return Data frame with original columns plus weight columns (prefixed with "weight_").
#'
#' @examples
#' \dontrun{
#' weight_data <- convert_numbers_to_weight(
#'   data = frequency_data,
#'   length_cols = c("8", "8.5", "9", "9.5", "10"),
#'   a = 0.0012,
#'   b = 3.1242
#' )
#' }
#'
#' @export
#' @importFrom stats setNames
convert_numbers_to_weight <- function(data, length_cols, a, b) {
  if (!is.data.frame(data)) stop("'data' must be a data.frame.")
  if (!is.numeric(a)) stop("'a' must be numeric.")
  if (!is.numeric(b)) stop("'b' must be numeric.")
  
  if (is.character(length_cols)) {
    length_values <- extract_numeric_values(length_cols)
  } else {
    length_values <- as.numeric(length_cols)
  }
  
  missing_cols <- setdiff(length_cols, colnames(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "))
  }
  
  for (col in length_cols) {
    if (!is.numeric(data[[col]])) {
      data[[col]] <- as.numeric(data[[col]])
      warning("Column '", col, "' converted to numeric.")
    }
  }
  
  tryCatch({
    weights <- as.data.frame(t(apply(data[, length_cols, drop = FALSE], 1, function(x) {
      calculate_fish_weight(length = length_values, a = a, b = b) * x
    })))
    
    colnames(weights) <- paste0("weight_", length_cols)
    result <- cbind(data, weights)
    return(result)
    
  }, error = function(e) {
    stop("Error calculating weights: ", e$message)
  })
}

#' Summarize juveniles by group
#'
#' @description
#' Calculates juvenile percentages by specified groups, both in number and weight.
#' Uses modern dplyr approach for efficient processing. Can auto-detect length columns
#' if not specified.
#'
#' @param data Data frame with length frequency data.
#' @param group_cols Vector of column names for grouping.
#' @param length_cols Vector of length column names or indices. If NULL, auto-detection is attempted.
#' @param juvenile_limit Length threshold for juveniles.
#' @param a Coefficient of length-weight relationship.
#' @param b Exponent of length-weight relationship.
#' @param remove_empty Logical. Remove groups with no data.
#' @param verbose Logical. Print information about detected columns.
#'
#' @return Data frame with juvenile statistics by group.
#'
#' @examples
#' \dontrun{
#' juvenile_summary <- summarize_juveniles_by_group(
#'   data = fishery_data,
#'   group_cols = "date",
#'   juvenile_limit = 12
#' )
#' }
#'
#' @export
#' @importFrom dplyr group_by_at reframe across everything filter pick all_of mutate
#' @importFrom tidyr unnest
#' @importFrom utils head tail
summarize_juveniles_by_group <- function(data, 
                                         group_cols, 
                                         length_cols = NULL, 
                                         juvenile_limit = 12, 
                                         a = 0.0012, 
                                         b = 3.1242,
                                         remove_empty = TRUE,
                                         verbose = FALSE) {
  
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame.")
  }
  
  if (!all(group_cols %in% colnames(data))) {
    stop("Not all grouping columns found in data.")
  }
  
  if (is.null(length_cols)) {
    if (verbose) {
      message("=== Auto-detecting Length Columns ===")
    }
    
    length_cols <- find_columns_by_pattern(data, pattern = "length_", sort = TRUE)
    pattern_used <- "length_"
    
    if (length(length_cols) == 0) {
      length_cols <- find_columns_by_pattern(data, pattern = "weighted_", sort = TRUE)
      pattern_used <- "weighted_"
    }
    
    if (length(length_cols) == 0) {
      numeric_cols <- names(data)[sapply(names(data), function(x) {
        !is.na(suppressWarnings(as.numeric(x))) && 
        grepl("^[0-9]+(\\.[0-9]+)?$", x)
      })]
      
      if (length(numeric_cols) > 0) {
        numeric_values <- as.numeric(numeric_cols)
        length_cols <- numeric_cols[order(numeric_values)]
        pattern_used <- "numeric column names"
      }
    }
    
    if (length(length_cols) == 0) {
      stop("No length columns found. Please specify length_cols manually.\n",
           "Tried patterns: 'length_', 'weighted_', and numeric column names.")
    }
    
    if (verbose) {
      message(sprintf("Auto-detected %d length columns using pattern '%s'", 
                     length(length_cols), pattern_used))
      message(sprintf("Length range: %s to %s", 
                     head(length_cols, 1), tail(length_cols, 1)))
    }
    
    cols_names <- length_cols
    
  } else {
    if (is.numeric(length_cols)) {
      if (any(length_cols > ncol(data) | length_cols < 1)) {
        stop("Some length column indices are out of range.")
      }
      cols_names <- names(data)[length_cols]
    } else {
      if (!all(length_cols %in% colnames(data))) {
        missing_cols <- length_cols[!length_cols %in% colnames(data)]
        stop("Not all length columns found in data. Missing: ", 
             paste(missing_cols, collapse = ", "))
      }
      cols_names <- length_cols
    }
    
    if (verbose) {
      message(sprintf("Using %d manually specified length columns", length(cols_names)))
    }
  }
  
  data <- data %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(cols_names), ~as.numeric(.x)))
  
  length_values <- extract_numeric_values(cols_names, verbose = verbose)
  
  if (verbose) {
    message(sprintf("Length values range: %.1f to %.1f", 
                   min(length_values), max(length_values)))
    message(sprintf("Juvenile limit set to: %.1f", juvenile_limit))
  }
  
  process_group <- function(df) {
    if (nrow(df) == 0) {
      return(data.frame(
        perc_juv_number = NA_real_,
        perc_juv_weight = NA_real_,
        total_number = 0,
        total_weight = 0,
        juvenil_number = 0,
        juvenil_weight = 0
      ))
    }
    
    frequencies <- colSums(df[, cols_names, drop = FALSE], na.rm = TRUE)
    
    if (all(frequencies == 0)) {
      return(data.frame(
        perc_juv_number = NA_real_,
        perc_juv_weight = NA_real_,
        total_number = 0,
        total_weight = 0,
        juvenil_number = 0,
        juvenil_weight = 0
      ))
    }
    
    calculate_juvenile_statistics(frequencies, length_values, juvenile_limit, a, b)
  }
  
  if (length(group_cols) == 0) {
    result <- process_group(data)
  } else {
    results <- data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
      dplyr::reframe(
        result = list({
          df_group <- dplyr::pick(dplyr::all_of(cols_names))
          df_group <- as.data.frame(df_group)
          process_group(df_group)
        })
      ) %>%
      tidyr::unnest(.data$result)
    
    if (remove_empty && any(results$total_number == 0, na.rm = TRUE)) {
      initial_groups <- nrow(results)
      results <- results %>%
        dplyr::filter(.data$total_number > 0)
      
      if (verbose && nrow(results) < initial_groups) {
        message(sprintf("! Removed %d empty groups", initial_groups - nrow(results)))
      }
    }
    
    result <- results
  }
  
  if (verbose) {
    message("\n=== Processing Summary ===")
    if (length(group_cols) > 0) {
      message(sprintf("Groups processed: %d", nrow(result)))
    }
    message(sprintf("Length classes used: %d", length(length_values)))
    
    if (nrow(result) > 0 && !all(is.na(result$perc_juv_number))) {
      avg_juv_perc <- mean(result$perc_juv_number, na.rm = TRUE)
      message(sprintf("Average juvenile percentage: %.1f%%", avg_juv_perc))
    }
  }
  
  return(as.data.frame(result))
}

#' Calculate juvenile statistics for a group
#'
#' @description
#' Helper function that calculates juvenile percentages in number and weight.
#' Used internally by summarize_juveniles_by_group.
#'
#' @param frequencies Numeric vector of frequencies by length.
#' @param length_values Numeric vector of corresponding lengths.
#' @param juvenile_limit Length threshold for juveniles.
#' @param a Coefficient of length-weight relationship.
#' @param b Exponent of length-weight relationship.
#'
#' @return Data frame with juvenile statistics.
#'
#' @examples
#' frequencies <- c(10, 15, 25, 30, 20, 10)
#' lengths <- c(8, 9, 10, 11, 12, 13)
#' stats <- calculate_juvenile_statistics(frequencies, lengths)
#'
#' @export
calculate_juvenile_statistics <- function(frequencies, 
                                          length_values, 
                                          juvenile_limit = 12, 
                                          a = 0.0012, 
                                          b = 3.1242) {
  
  if (!is.numeric(frequencies)) stop("'frequencies' must be numeric.")
  if (!is.numeric(length_values)) stop("'length_values' must be numeric.")
  if (length(frequencies) != length(length_values)) {
    stop("'frequencies' and 'length_values' must have same length.")
  }
  
  total_number <- sum(frequencies, na.rm = TRUE)
  
  if (total_number == 0) {
    return(data.frame(
      perc_juv_number = NA_real_,
      perc_juv_weight = NA_real_,
      total_number = 0,
      total_weight = 0,
      juvenil_number = 0,
      juvenil_weight = 0
    ))
  }
  
  perc_juv_number <- suppressWarnings(
    calculate_juvenile_percentage(frequencies, length_values, juvenile_limit)
  )
  
  weights <- calculate_fish_weight(length_values, a, b) * frequencies
  total_weight <- sum(weights, na.rm = TRUE) / 1000
  
  if (total_weight == 0) {
    perc_juv_weight <- NA_real_
  } else {
    perc_juv_weight <- suppressWarnings(
      calculate_juvenile_percentage(weights, length_values, juvenile_limit)
    )
  }
  
  return(data.frame(
    perc_juv_number = perc_juv_number,
    perc_juv_weight = perc_juv_weight,
    total_number = total_number,
    total_weight = total_weight,
    juvenil_number = total_number * perc_juv_number / 100,
    juvenil_weight = total_weight * perc_juv_weight / 100
  ))
}