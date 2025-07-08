#' Fetch fishing announcements from external sources
#'
#' @description
#' Retrieves fishing announcements from official websites within a specified date range.
#' This function is specifically designed for PRODUCE (Peru) but can be adapted for other sources.
#'
#' @param start_date Start date in "dd/mm/yyyy" format.
#' @param end_date End date in "dd/mm/yyyy" format.
#' @param download Logical. Download PDF files.
#' @param download_dir Directory for downloaded files.
#' @param batch_size Records per request.
#' @param verbose Print detailed information.
#' @param source_url Base URL for the announcement source. Defaults to the PRODUCE page:
#'   \url{https://consultasenlinea.produce.gob.pe/ConsultasEnLinea/consultas.web/comunicados/suspensionPreventiva}
#' @param max_records Maximum records to retrieve.
#'
#' @return Data frame with announcement information and download links.
#'
#' @examples
#' \dontrun{
#' announcements <- fetch_fishing_announcements(
#'   start_date = "01/01/2023",
#'   end_date = "31/12/2023"
#' )
#' 
#' announcements <- fetch_fishing_announcements(
#'   start_date = "01/01/2023",
#'   end_date = "31/01/2023",
#'   download = TRUE,
#'   download_dir = "announcements"
#' )
#' }
#'
#' @export
#' @importFrom httr GET POST set_cookies add_headers status_code content
#' @importFrom jsonlite fromJSON
#' @importFrom rvest read_html html_node html_attr
fetch_fishing_announcements <- function(start_date, 
                                        end_date, 
                                        download = FALSE,
                                        download_dir = "downloads",
                                        batch_size = 10,
                                        verbose = TRUE,
                                        source_url = NULL,
                                        max_records = 5000) {
  
  # Set default source URL if not provided
  if (is.null(source_url) || !is.character(source_url) || source_url == "") {
    source_url <- "https://consultasenlinea.produce.gob.pe/ConsultasEnLinea/consultas.web/comunicados/suspensionPreventiva"
    if (verbose) message("Using default source URL: ", source_url)
  }

  if (!is.numeric(batch_size) || batch_size <= 0) {
  stop("batch_size must be a positive integer.")
  }
  
  check_required_packages(c("httr", "rvest", "jsonlite"))
  
  main_response <- get_main_page(source_url, verbose)
  
  if (httr::status_code(main_response) != 200) {
    stop("Failed to connect to announcement website. Status code: ", httr::status_code(main_response))
  }
  
  cookies <- extract_cookies(main_response, verbose)
  html_content <- httr::content(main_response, "text", encoding = "UTF-8")
  token <- extract_token(html_content, verbose)
  
  if (download) {
    create_download_dir(download_dir, verbose)
  }
  
  all_announcements <- data.frame()
  total_records <- 0
  start_index <- 0
  
  repeat {
    batch <- fetch_announcements_batch(
      start_index = start_index,
      batch_size = batch_size,
      token = token,
      cookies = cookies,
      main_url = source_url,
      tipo = 2,
      start_date = start_date,
      end_date = end_date,
      verbose = verbose
    )
    
    if (is.null(batch) || nrow(batch) == 0) {
      if (verbose) message("No more announcements to fetch or error encountered.")
      break
    }
    
    in_range <- rep(TRUE, nrow(batch))
    if (start_date != "" && end_date != "") {
      in_range <- sapply(batch$Date, is_date_in_range, 
                        start_date = start_date, end_date = end_date)
    }
    
    batch_filtered <- batch[in_range, ]
    
    if (nrow(batch_filtered) > 0) {
      batch_filtered$DownloadURL <- sapply(batch_filtered$InternalFile, generate_download_url)
      all_announcements <- rbind(all_announcements, batch_filtered)
      
      if (verbose) message("Found ", nrow(batch_filtered), " announcements within date range.")
    }
    
    total_records <- total_records + nrow(batch)
    
    if (total_records >= max_records || nrow(batch) < batch_size) {
      if (verbose && total_records >= max_records) {
        message("Reached maximum record limit of ", max_records)
      }
      break
    }
    
    start_index <- start_index + batch_size
  }
  
  if (download && nrow(all_announcements) > 0) {
    if (verbose) message("Starting download of ", nrow(all_announcements), " files...")
    
    all_announcements$Downloaded <- FALSE
    
    for (i in 1:nrow(all_announcements)) {
      url <- all_announcements$DownloadURL[i]
      file_name <- all_announcements$FileName[i]
      
      if (verbose) message("Downloading (", i, "/", nrow(all_announcements), "): ", file_name)
      
      success <- download_file(url, file_name, download_dir, verbose)
      all_announcements$Downloaded[i] <- success
      
      Sys.sleep(0.5)
    }
    
    if (verbose) {
      downloaded_count <- sum(all_announcements$Downloaded)
      message("Downloaded ", downloaded_count, " out of ", nrow(all_announcements), " files.")
    }
  }
  
  if (nrow(all_announcements) == 0) {
    message("No announcements found within the specified date range.")
  }
  
  return(all_announcements)
}

#' Extract data from PDF announcements
#'
#' @description
#' Processes PDF files containing official fishing announcements and extracts relevant information
#' such as dates, coordinates, and nautical miles. Handles both local files and URLs.
#'
#' @param pdf_sources Character vector of PDF file paths or URLs.
#' @param temp_dir Temporary directory for downloaded files. If NULL, uses tempdir().
#' @param verbose Show processing messages.
#' @param max_retries Maximum download retries for URLs.
#'
#' @return Data frame with extracted announcement information including coordinates,
#'   dates, and nautical mile distances.
#'
#' @examples
#' \dontrun{
#' pdf_files <- c("announcement1.pdf", "announcement2.pdf")
#' results <- extract_pdf_data(pdf_sources = pdf_files)
#' 
#' pdf_urls <- c(
#'   "https://example.com/announcement1.pdf",
#'   "https://example.com/announcement2.pdf"
#' )
#' results <- extract_pdf_data(pdf_sources = pdf_urls)
#' }
#'
#' @export
#' @importFrom pdftools pdf_text
#' @importFrom stringr str_squish str_split str_extract_all str_extract
#' @importFrom utils download.file
extract_pdf_data <- function(pdf_sources = NULL, temp_dir = NULL, verbose = TRUE, max_retries = 3) {
  
  # Input validation
  if (is.null(pdf_sources) || length(pdf_sources) == 0) {
    stop("'pdf_sources' is required and cannot be NULL or empty.")
  }

  if (!is.character(pdf_sources)) {
    stop("'pdf_sources' must be a character vector of file paths or URLs.")
  }

  if (!is.numeric(max_retries) || max_retries < 1) {
    stop("'max_retries' must be a positive integer.")
  }

  if (is.null(temp_dir)) {
    temp_dir <- tempdir()
  }
  
  is_url <- function(x) {
    grepl("^(http|https|ftp)://", x, ignore.case = TRUE)
  }
  
  processed_files <- character(length(pdf_sources))
  
  for (i in seq_along(pdf_sources)) {
    if (is_url(pdf_sources[i])) {
      file_name <- basename(pdf_sources[i])
      temp_path <- file.path(temp_dir, file_name)
      
      success <- FALSE
      for (retry in 1:max_retries) {
        tryCatch({
          download_result <- utils::download.file(
            url = pdf_sources[i],
            destfile = temp_path,
            mode = "wb",
            quiet = !verbose
          )
          
          if (download_result == 0) {
            processed_files[i] <- temp_path
            success <- TRUE
            break
          }
        }, error = function(e) {
          if (verbose && retry == max_retries) {
            warning("Error downloading URL '", pdf_sources[i], "' after ", max_retries, " attempts: ", e$message)
          }
        })
        
        if (!success && retry < max_retries) {
          Sys.sleep(1)
        }
      }
      
      if (!success) {
        processed_files[i] <- NA_character_
      }
    } else {
      if (file.exists(pdf_sources[i])) {
        processed_files[i] <- pdf_sources[i]
      } else {
        warning("Local file does not exist: ", pdf_sources[i])
        processed_files[i] <- NA_character_
      }
    }
  }
  
  valid_files <- processed_files[!is.na(processed_files)]
  
  if (length(valid_files) == 0) {
    stop("Could not access any of the specified files. Cannot continue.")
  }
  
  all_results <- list()
  
  empty_structure <- data.frame(
    StartDateTime = as.POSIXct(character()),
    EndDateTime = as.POSIXct(character()),
    StartLatitude = character(),
    EndLatitude = character(),
    StartLongitude = character(),
    EndLongitude = character(),
    StartNauticalMiles = numeric(),
    EndNauticalMiles = numeric(),
    file_name = character(),
    announcement = character(),
    stringsAsFactors = FALSE
  )
  
  for (file in valid_files) {
    original_name <- basename(file)
    
    if (verbose) message("Processing: ", original_name)
    
    text <- tryCatch({
      pdftools::pdf_text(file)
    }, error = function(e) {
      warning("Error reading PDF file '", file, "': ", e$message)
      return(NULL)
    })
    
    if (is.null(text) || length(text) == 0) {
      warning("Could not extract text from file '", file, "'")
      all_results[[length(all_results) + 1]] <- empty_structure
      next
    }
    
    result <- process_pdf_text(text, original_name, verbose)
    all_results[[length(all_results) + 1]] <- result
  }
  
  if (length(all_results) > 0) {
    tryCatch({
      final_results <- do.call(rbind, all_results)
      if (nrow(final_results) == 0) {
        warning("Could not extract information from any PDF files.")
        return(empty_structure)
      }
      return(final_results)
    }, error = function(e) {
      warning("Error combining results: ", e$message)
      return(empty_structure)
    })
  } else {
    warning("Could not extract information from any PDF files.")
    return(empty_structure)
  }
}

#' Format extracted announcement data
#'
#' @description
#' Formats and filters data extracted from announcements, converting dates to proper formats
#' and allowing filtering by date range.
#'
#' @param data Data frame with structure from extract_pdf_data.
#' @param min_date Minimum date for filtering (YYYY-MM-DD format or Date/POSIXct object).
#' @param max_date Maximum date for filtering (YYYY-MM-DD format or Date/POSIXct object).
#' @param convert_coordinates Logical. Convert DMS coordinates to decimal.
#'
#' @return Data frame with formatted and filtered announcement data.
#'
#' @examples
#' \dontrun{
#' formatted_data <- format_extracted_data(raw_data)
#' 
#' filtered_data <- format_extracted_data(
#'   data = raw_data,
#'   min_date = "2024-11-01",
#'   max_date = "2024-12-31"
#' )
#' }
#'
#' @export
format_extracted_data <- function(data, min_date = NULL, max_date = NULL, convert_coordinates = TRUE) {
  
  required_columns <- c("StartDateTime", "EndDateTime", "StartLatitude", "EndLatitude")
  
  if (!all(required_columns %in% names(data))) {
    stop("Data does not have expected structure. Required columns: ",
         paste(required_columns, collapse = ", "))
  }
  
  fmt_data <- data
  
  for (col in c("StartDateTime", "EndDateTime")) {
    if (col %in% names(fmt_data)) {
      if (inherits(fmt_data[[col]], "POSIXct")) {
        next
      }
      
      fmt_data[[col]] <- convert_to_date(fmt_data[[col]], output_type = "datetime")
    }
  }
  
  if (convert_coordinates) {
    coord_cols <- c("StartLatitude", "EndLatitude", "StartLongitude", "EndLongitude")
    for (col in coord_cols) {
      if (col %in% names(fmt_data)) {
        if (any(grepl("[°'\"]", fmt_data[[col]], ignore.case = TRUE), na.rm = TRUE)) {
          hemisphere <- ifelse(grepl("Lon", col), "W", "S")
          fmt_data[[paste0(col, "_decimal")]] <- sapply(fmt_data[[col]], function(x) {
            if (is.na(x) || x == "") return(NA)
            tryCatch(dms_to_decimal(x, hemisphere = hemisphere), error = function(e) NA)
          })
        }
      }
    }
  }
  
  numeric_cols <- c("StartNauticalMiles", "EndNauticalMiles")
  for (col in numeric_cols) {
    if (col %in% names(fmt_data)) {
      fmt_data[[col]] <- safe_numeric_conversion(fmt_data[[col]], col)
    }
  }
  
  if (!is.null(min_date) || !is.null(max_date)) {
    if (!is.null(min_date)) {
      if (is.character(min_date)) {
        min_date <- convert_to_date(min_date, output_type = "datetime")
      } else if (inherits(min_date, "Date")) {
        min_date <- as.POSIXct(min_date)
      }
    }
    
    if (!is.null(max_date)) {
      if (is.character(max_date)) {
        max_date <- convert_to_date(max_date, output_type = "datetime")
      } else if (inherits(max_date, "Date")) {
        max_date <- as.POSIXct(max_date)
      }
    }
    
    if (!is.null(min_date) && !is.null(max_date)) {
      fmt_data <- fmt_data[fmt_data$StartDateTime >= min_date & 
                          fmt_data$StartDateTime <= max_date, ]
    } else if (!is.null(min_date)) {
      fmt_data <- fmt_data[fmt_data$StartDateTime >= min_date, ]
    } else if (!is.null(max_date)) {
      fmt_data <- fmt_data[fmt_data$StartDateTime <= max_date, ]
    }
  }
  
  if (nrow(fmt_data) == 0) {
    warning("No data meets the filtering criteria.")
  }
  
  return(fmt_data)
}