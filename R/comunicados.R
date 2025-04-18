#' @title Get PRODUCE Announcements by Date Range
#' @description
#' Retrieves announcements from the PRODUCE website within a specified date range.
#' @param start_date Character string in "dd/mm/yyyy" format for start date
#' @param end_date Character string in "dd/mm/yyyy" format for end date
#' @param download Logical. If TRUE, downloads PDF files
#' @param download_dir Directory for downloaded files
#' @param batch_size Records to fetch in each request
#' @param verbose Print detailed information during execution
#' @return DataFrame with announcement information and download links
#' @export 
#' @examples
#' 
#' announcements = get_produce_announcements(
#'    start_date = "01/01/2023", 
#'    end_date = "31/12/2023"
#' )
#' 
#' print(announcements)
#' 
get_produce_announcements <- function(
  start_date, 
  end_date, 
  download = FALSE,
  download_dir = "downloads",
  batch_size = 10,
  verbose = TRUE
) {
  # Verificar paquetes requeridos
  check_required_packages()
  
  # Obtener página principal y extraer token
  main_response <- get_main_page(verbose)
  
  # Verificar estado de respuesta
  if(status_code(main_response) != 200) {
    stop("Failed to connect to the PRODUCE website. Status code: ", status_code(main_response))
  }
  
  # Extraer cookies
  cookies <- extract_cookies(main_response, verbose)
  
  # Extraer token
  html_content <- content(main_response, "text", encoding = "UTF-8")
  token <- extract_token(html_content, verbose)
  
  # URL principal para referencia
  main_url <- "https://consultasenlinea.produce.gob.pe/ConsultasEnLinea/consultas.web/comunicados/suspensionPreventiva"
  
  # Crear directorio de descarga si es necesario
  if(download) {
    create_download_dir(download_dir, verbose)
  }
  
  # Proceso principal para recolectar todos los anuncios
  all_announcements <- data.frame()
  total_records <- 0
  start_index <- 0
  max_records <- 5000 # Máximo de registros a obtener
  
  # Obtener anuncios en lotes
  repeat {
    batch <- fetch_announcements_batch(
      start_index, 
      batch_size, 
      token, 
      cookies, 
      main_url,
      2,
      start_date, 
      end_date, 
      verbose
    )
    
    if(is.null(batch) || nrow(batch) == 0) {
      if(verbose) message("No more announcements to fetch or error encountered.")
      break
    }
    
    # Si filtramos por fecha en el servidor, todos los registros deberían estar en rango
    # Pero verificamos igualmente
    in_range <- rep(TRUE, nrow(batch))
    if(start_date == "" || end_date == "") {
      in_range <- sapply(batch$Date, is_date_in_range, start_date = start_date, end_date = end_date)
    }
    
    batch_filtered <- batch[in_range, ]
    
    if(nrow(batch_filtered) > 0) {
      # Añadir columna URL de descarga
      batch_filtered$DownloadURL <- sapply(batch_filtered$InternalFile, generate_download_url)
      
      # Añadir a resultados
      all_announcements <- rbind(all_announcements, batch_filtered)
      
      if(verbose) message("Found ", nrow(batch_filtered), " announcements within date range.")
    }
    
    total_records <- total_records + nrow(batch)
    
    # Verificar si debemos parar
    if(total_records >= max_records || nrow(batch) < batch_size) {
      if(verbose && total_records >= max_records) 
        message("Reached maximum record limit of ", max_records)
      break
    }
    
    # Pasar al siguiente lote
    start_index <- start_index + batch_size
  }
  
  # Descargar archivos si se solicita
  if(download && nrow(all_announcements) > 0) {
    if(verbose) message("Starting download of ", nrow(all_announcements), " files...")
    
    # Añadir columna de estado de descarga
    all_announcements$Downloaded <- FALSE
    
    for(i in 1:nrow(all_announcements)) {
      url <- all_announcements$DownloadURL[i]
      file_name <- all_announcements$FileName[i]
      
      if(verbose) message("Downloading (", i, "/", nrow(all_announcements), "): ", file_name)
      
      success <- download_announcement_file(url, file_name, download_dir, verbose)
      all_announcements$Downloaded[i] <- success
      
      # Pequeña pausa para no sobrecargar el servidor
      Sys.sleep(0.5)
    }
    
    if(verbose) {
      downloaded_count <- sum(all_announcements$Downloaded)
      message("Downloaded ", downloaded_count, " out of ", nrow(all_announcements), " files.")
    }
  }
  
  if(nrow(all_announcements) == 0) {
    message("No announcements found within the specified date range.")
  }
  
  return(all_announcements)
}



#' Extract data from PRODUCE announcements in PDF format
#'
#' This function processes PDF files containing official announcements and extracts relevant information,
#' such as dates, times, latitude and longitude coordinates, and nautical miles. If longitude data is not found,
#' the function will use the nautical mile values instead. The result is a data.frame containing the start and
#' end dates, coordinates, and other information related to the announcement.
#'
#' @param vector_pdf_names A character vector of PDF file names or URLs to be processed.
#'
#' @return A data.frame with the extracted information from the announcements. The data.frame includes the following
#' columns:
#' \itemize{
#'   \item StartDateTime: Start date and time.
#'   \item EndDateTime: End date and time.
#'   \item StartLatitude: Starting latitude.
#'   \item EndLatitude: Ending latitude.
#'   \item StartLongitude: Starting longitude (if available).
#'   \item EndLongitude: Ending longitude (if available).
#'   \item StartNauticalMiles: Starting nautical miles (if longitude is not available).
#'   \item EndNauticalMiles: Ending nautical miles (if longitude is not available).
#'   \item announcement: Name of the PDF file or announcement.
#' }
#'
#' @examples
#' # Using local files
#' \dontrun{
#' pdf_files <- c("announcement1.pdf", "announcement2.pdf")
#' results <- extract_announcement_data(vector_pdf_names = pdf_files)
#' }
#'
#' # Using URLs
#' announcements <- get_produce_announcements(
#'   start_date = "01/02/2025", 
#'   end_date = "28/02/2025",
#'   download = FALSE
#'  )
#'
#' results <- extract_announcement_data(vector_pdf_names = announcements$DownloadURL)
#'
#' print(head(results))
#'
#' @export
#' @importFrom pdftools pdf_text
#' @importFrom stringr str_squish str_split str_extract_all str_extract
#' @importFrom utils download.file
extract_announcement_data <- function(vector_pdf_names) {
  # Parameter validation
  if (missing(vector_pdf_names)) {
    stop("The 'vector_pdf_names' parameter is required.")
  }

  if (length(vector_pdf_names) == 0) {
    warning("No PDF file names were provided for processing.")
    return(data.frame())
  }

  if (!is.character(vector_pdf_names)) {
    stop("'vector_pdf_names' must be a character vector with PDF file names.")
  }

  # Create helper function to check if it's a URL
  is_url <- function(x) {
    grepl("^(http|https|ftp)://", x, ignore.case = TRUE)
  }

  # Create temporary directory for downloaded files
  temp_dir <- tempdir()
  temp_files <- character(length(vector_pdf_names))

  # Process URLs and download files if necessary
  for (i in seq_along(vector_pdf_names)) {
    if (is_url(vector_pdf_names[i])) {
      # It's a URL, try to download
      file_name <- basename(vector_pdf_names[i])
      temp_path <- file.path(temp_dir, file_name)

      tryCatch({
        download_result <- utils::download.file(
          url = vector_pdf_names[i],
          destfile = temp_path,
          mode = "wb",  # Binary mode for PDFs
          quiet = TRUE
        )

        if (download_result == 0) {
          # Successful download, use temporary file
          temp_files[i] <- temp_path
        } else {
          warning("Error downloading URL: ", vector_pdf_names[i])
          temp_files[i] <- NA_character_
        }
      }, error = function(e) {
        warning("Error downloading URL '", vector_pdf_names[i], "': ", e$message)
        temp_files[i] <- NA_character_
      })
    } else {
      # It's a local file, use the path as is
      temp_files[i] <- vector_pdf_names[i]
    }
  }

  # Remove URLs that couldn't be downloaded
  temp_files <- temp_files[!is.na(temp_files)]

  if (length(temp_files) == 0) {
    stop("Could not access any of the specified files. Cannot continue.")
  }

  # Check that files exist
  non_existent_files <- temp_files[!file.exists(temp_files)]
  if (length(non_existent_files) > 0) {
    warning("The following files do not exist: ", paste(non_existent_files, collapse = ", "))
    temp_files <- temp_files[file.exists(temp_files)]

    if (length(temp_files) == 0) {
      stop("None of the specified files exist. Cannot continue.")
    }
  }

  # Check that files are PDFs (.pdf extension)
  non_pdf <- temp_files[!grepl("\\.pdf$", temp_files, ignore.case = TRUE)]
  if (length(non_pdf) > 0) {
    warning("The following files do not have .pdf extension: ", paste(non_pdf, collapse = ", "))
  }

  # Initialize list to store results of all PDFs
  all_results <- list()

  # Define structure for empty results
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
    stringsAsFactors = FALSE,
    announcement = character()
  )

  # Process each PDF file
  for (file in temp_files) {
    # Preserve original name for the file_name field
    original_name <- ifelse(is_url(file), basename(file), basename(file))

    # Read the text from the PDF file with error handling
    text <- tryCatch({
      pdftools::pdf_text(file)
    }, error = function(e) {
      warning("Error reading PDF file '", file, "': ", e$message)
      return(NULL)
    })

    # Check if the PDF could be read
    if (is.null(text) || length(text) == 0) {
      warning("Could not extract text from file '", file, "' or the file is empty.")
      all_results[[length(all_results) + 1]] <- empty_structure
      next
    }

    # Clean the text
    clean_text <- stringr::str_squish(text)

    # Check if it contains the key text string
    if (!any(grepl("DISPONER LA SUSPENSIÓN PREVENTIVA DE LA ACTIVIDAD EXTRACTIVA", clean_text))) {
      warning("The file '", file, "' does not appear to be a valid PRODUCE announcement. It does not contain the key text.")
      all_results[[length(all_results) + 1]] <- empty_structure
      next
    }

    # Split the text into blocks
    blocks <- tryCatch({
      stringr::str_split(clean_text, "DISPONER LA SUSPENSIÓN PREVENTIVA DE LA ACTIVIDAD EXTRACTIVA")[[1]]
    }, error = function(e) {
      warning("Error splitting text into blocks for file '", file, "': ", e$message)
      return(character(0))
    })

    # Check if there are blocks to process (after the first one)
    if (length(blocks) <= 1) {
      warning("No valid information blocks found in file '", file, "'.")
      all_results[[length(all_results) + 1]] <- empty_structure
      next
    }

    # Check if it contains the key text string
    if (!any(grepl("COMUNICADO N°", clean_text))) {
      stop("The file '", file, "' has no title.")
    }

    # Initialize list for results of this file
    results <- list()

    # Process each block (skip the first one)
    for (i in 2:length(blocks)) {
      block <- blocks[i]

      # Extract dates and times with error handling
      tryCatch({
        pattern_dates <- "(\\d{2}:\\d{2} horas del \\d{2} de \\w+ de \\d{4})"
        dates_text <- stringr::str_extract_all(block, pattern_dates)[[1]]

        announcement <- stringr::str_extract(clean_text, "COMUNICADO\\s*N[°º]\\s*\\d+(?:\\s*[-–]\\s*\\d+)?(?:-[A-Z]+)?")

        if (length(dates_text) < 2) {
          warning("Not enough dates found in block ", i, " of file '", file, "'.")
          next
        }

        dates <- gsub("horas del ", "", dates_text)
        final_dates <- as.POSIXct(dates, format = "%H:%M %d de %B de %Y", tz = "America/Lima")

        if (any(is.na(final_dates))) {
          warning("Error converting dates in block ", i, " of file '", file, "'.")
          next
        }
      }, error = function(e) {
        warning("Error processing dates in block ", i, " of file '", file, "': ", e$message)
        final_dates <- rep(as.POSIXct(NA), 2)
      })

      # Extract coordinates with error handling
      tryCatch({
        # Various patterns to try to capture different coordinate formats
        lat_patterns <- c(
          "\\d{1,2}°\\d{1,2}['´’][NS]",
          "\\d{1,2}°\\d{1,2}['’][NS]",
          "\\d{1,2}°\\d{1,2}['`][NS]"
        )

        lon_patterns <- c(
          "\\d{1,2}°\\d{1,2}['´’][WEO]",
          "\\d{1,2}°\\d{1,2}['’][WEO]",
          "\\d{1,2}°\\d{1,2}['`][WEO]"
        )

        # Try each pattern until matches are found
        data_positions_lat <- character(0)
        for (pattern in lat_patterns) {
          data_positions_lat <- stringr::str_extract_all(block, pattern)[[1]]
          if (length(data_positions_lat) > 0) break
        }

        data_positions_lon <- character(0)
        for (pattern in lon_patterns) {
          data_positions_lon <- stringr::str_extract_all(block, pattern)[[1]]
          if (length(data_positions_lon) > 0) break
        }

      }, error = function(e) {
        warning("Error extracting coordinates in block ", i, " of file '", file, "': ", e$message)
        data_positions_lat <- character(0)
        data_positions_lon <- character(0)
      })

      # Check if there are longitudes present
      if (length(data_positions_lon) == 0) {
        # Extract nautical miles if there are no longitudes
        tryCatch({
          # Various patterns for different nautical mile formats
          nautical_mile_patterns <- c(
            "de (\\d+) a (\\d+) millas náuticas",
            "(?:entre las )?(\\d+) y (\\d+) millas náuticas",
            "dentro de las (\\d+) millas náuticas",
            "(\\d+)\\s*a\\s*(\\d+)\\s*mn"
          )

          # Try each pattern
          miles_texts <- character(0)
          for (pattern in nautical_mile_patterns) {
            miles_texts <- stringr::str_extract_all(block, pattern)[[1]]
            if (length(miles_texts) > 0) break
          }

          # Initialize vectors for miles
          start_miles <- numeric()
          end_miles <- numeric()

          # Process each nautical miles text
          for (miles_text in miles_texts) {
            miles_numbers <- as.numeric(stringr::str_extract_all(miles_text, "\\d+")[[1]])

            if (length(miles_numbers) > 0) {
              if (length(miles_numbers) == 1) {
                start_miles <- c(start_miles, NA)
                end_miles <- c(end_miles, miles_numbers)
              } else {
                sorted_miles_numbers <- sort(miles_numbers)
                for (k in seq(1, length(sorted_miles_numbers) - 1, by = 2)) {
                  if (k + 1 <= length(sorted_miles_numbers)) {
                    start_miles <- c(start_miles, sorted_miles_numbers[k])
                    end_miles <- c(end_miles, sorted_miles_numbers[k + 1])
                  }
                }
              }
            } else {
              start_miles <- c(start_miles, NA)
              end_miles <- c(end_miles, NA)
            }
          }

          # If there are no miles, generate NA values
          if (length(start_miles) == 0) {
            warning("No nautical miles found in block ", i, " of file '", file, "'.")
            start_miles <- NA
            end_miles <- NA
          }

        }, error = function(e) {
          warning("Error processing nautical miles in block ", i, " of file '", file, "': ", e$message)
          start_miles <- NA
          end_miles <- NA
        })

        # Create dataframes for each pair of coordinates
        num_positions <- length(data_positions_lat)
        if (num_positions >= 2) {
          for (j in seq(1, num_positions - 1, by = 2)) {
            if (j + 1 <= num_positions) {
              df <- data.frame(
                StartDateTime = final_dates[1],
                EndDateTime = final_dates[2],
                StartLatitude = data_positions_lat[j],
                EndLatitude = data_positions_lat[j + 1],
                StartLongitude = NA_character_,
                EndLongitude = NA_character_,
                StartNauticalMiles = start_miles[ceiling(j/2)],
                EndNauticalMiles = end_miles[ceiling(j/2)],
                file_name = original_name,
                announcement = announcement,
                stringsAsFactors = FALSE
              )
              results <- append(results, list(df))
            }
          }
        } else if (num_positions == 1) {
          # Special case: a single coordinate
          warning("Only one latitude coordinate found in block ", i, " of file '", file, "'. It will be used as start and end.")
          df <- data.frame(
            StartDateTime = final_dates[1],
            EndDateTime = final_dates[2],
            StartLatitude = data_positions_lat[1],
            EndLatitude = data_positions_lat[1],
            StartLongitude = NA_character_,
            EndLongitude = NA_character_,
            StartNauticalMiles = start_miles[1],
            EndNauticalMiles = end_miles[1],
            file_name = original_name,
            announcement = announcement,
            stringsAsFactors = FALSE
          )
          results <- append(results, list(df))
        } else {
          warning("No latitude coordinates found in block ", i, " of file '", file, "'.")
        }

      } else {
        # Case with longitudes present
        num_positions_lat <- length(data_positions_lat)
        num_positions_lon <- length(data_positions_lon)

        if (num_positions_lat == 0 || num_positions_lon == 0) {
          warning("Missing coordinates in block ", i, " of file '", file, "'.")
          next
        }

        # Determine how many complete pairs we can form
        num_positions <- min(num_positions_lat, num_positions_lon)

        if (num_positions < 2) {
          warning("Not enough coordinates to form a range in block ", i, " of file '", file, "'.")
          next
        }

        # Check that we can form pairs (even number of coordinates)
        if (num_positions %% 2 != 0) {
          warning("Odd number of coordinates in block ", i, " of file '", file, "'. Only complete pairs will be used.")
          num_positions <- num_positions - 1  # Adjust to use only complete pairs
        }

        for (j in seq(1, num_positions - 1, by = 2)) {
          if (j + 1 <= num_positions) {
            df <- data.frame(
              StartDateTime = final_dates[1],
              EndDateTime = final_dates[2],
              StartLatitude = data_positions_lat[j],
              EndLatitude = data_positions_lat[j + 1],
              StartLongitude = data_positions_lon[j],
              EndLongitude = data_positions_lon[j + 1],
              StartNauticalMiles = NA_real_,
              EndNauticalMiles = NA_real_,
              file_name = original_name,
              announcement = announcement,
              stringsAsFactors = FALSE
            )
            results <- append(results, list(df))
          }
        }
      }
    }

    # Combine results from this file
    if (length(results) > 0) {
      final_results <- tryCatch({
        do.call(rbind, results)
      }, error = function(e) {
        warning("Error combining results from file '", file, "': ", e$message)
        return(empty_structure)
      })
      all_results <- append(all_results, list(final_results))
    } else {
      warning("Could not extract information from file '", file, "'.")
      all_results <- append(all_results, list(empty_structure))
    }
  }

  # Combine all results
  if (length(all_results) > 0) {
    tryCatch({
      all_final_results <- do.call(rbind, all_results)
      if (nrow(all_final_results) == 0) {
        warning("Could not extract information from any of the provided PDF files.")
        return(empty_structure)
      }
      return(all_final_results)
    }, error = function(e) {
      warning("Error combining results from all files: ", e$message)
      return(empty_structure)
    })
  } else {
    warning("Could not extract information from any of the provided PDF files.")
    return(empty_structure)
  }
}



#' Format and filter announcement data
#'
#' This function correctly formats the data extracted from announcements,
#' converting dates to POSIXct format and allowing filtering by date range.
#'
#' @param data A data frame with the structure generated by extract_announcement_data.
#' @param min_date Minimum date for filtering (optional). Can be a string in "YYYY-MM-DD" format
#'        or a POSIXct/Date object.
#' @param max_date Maximum date for filtering (optional). Can be a string in "YYYY-MM-DD" format
#'        or a POSIXct/Date object.
#'
#' @return A data frame with all columns correctly formatted and filtered according to the
#'         specified parameters.
#'
#' @examples
#' 
#' # Using URLs
#' pdf_urls <- c(
#'   "https://consultasenlinea.produce.gob.pe/produce/descarga/comunicados/dgsfs/1542_comunicado1.pdf",
#'   "https://consultasenlinea.produce.gob.pe/produce/descarga/comunicados/dgsfs/1478_comunicado1.pdf",
#'   "https://consultasenlinea.produce.gob.pe/produce/descarga/comunicados/dgsfs/1468_comunicado1.pdf"
#' )
#'
#' results <- extract_announcement_data(pdf_urls)
#'
#' formatted_data <- format_announcement_data(results)
#'
#' # Format and filter by date range
#' filtered_data <- format_announcement_data(
#'   results,
#'   min_date = "2024-11-01",
#'   max_date = "2024-12-31"
#' )
#'
#' @export
format_announcement_data <- function(data, min_date = NULL, max_date = NULL) {
  # Check if the dataframe has the expected structure
  required_columns <- c(
    "StartDateTime", "EndDateTime",
    "StartLatitude", "EndLatitude"
  )

  if (!all(required_columns %in% names(data))) {
    stop("The dataframe does not have the expected structure. It must contain the columns: ",
         paste(required_columns, collapse = ", "))
  }

  # Clone the dataframe to avoid modifying the original
  fmt_data <- data

  # Convert date columns using the convert_to_date function
  for (col in c("StartDateTime", "EndDateTime")) {
    if (col %in% names(fmt_data)) {
      # If it's already POSIXct, do nothing
      if (inherits(fmt_data[[col]], "POSIXct")) {
        next
      }

      # Convert dates using the convert_to_date function
      fmt_data[[col]] <- convert_to_date(fmt_data[[col]], type = "datetime")
    }
  }

  # Convert latitude and longitude coordinates if they're not in decimal format
  for (col in c("StartLatitude", "EndLatitude", "StartLongitude", "EndLongitude")) {
    if (col %in% names(fmt_data)) {
      # Check if the column contains coordinates in DMS format
      if (any(grepl("[°'\"]", fmt_data[[col]], ignore.case = TRUE))) {
        # Convert to decimal using the dms_to_decimal function
        fmt_data[[col]] <- sapply(fmt_data[[col]], function(x) {
          if (is.na(x) || x == "") return(NA)
          tryCatch(dms_to_decimal(x), error = function(e) NA)
        })
      }
    }
  }

  # Make sure numeric columns are numeric
  for (col in c("StartNauticalMiles", "EndNauticalMiles")) {
    if (col %in% names(fmt_data)) {
      fmt_data[[col]] <- as.numeric(fmt_data[[col]])
    }
  }

  # Filter by date range if specified
  if (!is.null(min_date) || !is.null(max_date)) {
    # Convert min_date to POSIXct if necessary
    if (!is.null(min_date)) {
      if (is.character(min_date)) {
        min_date <- convert_to_date(min_date, type = "datetime")
      } else if (inherits(min_date, "Date")) {
        min_date <- as.POSIXct(min_date)
      }
    }

    # Convert max_date to POSIXct if necessary
    if (!is.null(max_date)) {
      if (is.character(max_date)) {
        max_date <- convert_to_date(max_date, type = "datetime")
      } else if (inherits(max_date, "Date")) {
        max_date <- as.POSIXct(max_date)
      }
    }

    # Apply filters
    if (!is.null(min_date) && !is.null(max_date)) {
      fmt_data <- fmt_data[fmt_data$StartDateTime >= min_date &
                               fmt_data$StartDateTime <= max_date, ]
    } else if (!is.null(min_date)) {
      fmt_data <- fmt_data[fmt_data$StartDateTime >= min_date, ]
    } else if (!is.null(max_date)) {
      fmt_data <- fmt_data[fmt_data$StartDateTime <= max_date, ]
    }
  }

  # Check that the dataframe has rows after filtering
  if (nrow(fmt_data) == 0) {
    warning("No data meets the filtering criteria.")
  }

  return(fmt_data)
}