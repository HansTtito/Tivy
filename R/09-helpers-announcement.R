#' Get main page
#'
#' @description
#' Makes request to main page and returns HTTP response.
#'
#' @param url Main page URL.
#' @param verbose Show detailed information.
#'
#' @return HTTP response object.
#' @keywords internal
#' @importFrom httr GET add_headers
get_main_page <- function(url, verbose = TRUE) {
  if (verbose) message("Fetching main page to extract token...")

  httr::GET(
    url,
    httr::add_headers(
      `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36",
      `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
      `Accept-Language` = "en-US,en;q=0.5"
    )
  )
}

#' Extract cookies
#'
#' @description
#' Extracts cookies from HTTP response.
#'
#' @param response HTTP response.
#' @param verbose Show detailed information.
#'
#' @return Data frame with cookies.
#' @keywords internal
#' @importFrom httr cookies
extract_cookies <- function(response, verbose = TRUE) {
  cookies <- httr::cookies(response)
  if(verbose && length(cookies) > 0) {
    message("Found ", nrow(cookies), " cookies")
  }
  return(cookies)
}

#' Extract token
#'
#' @description
#' Extracts security token from HTML page.
#'
#' @param html_content HTML content of the page.
#' @param verbose Show detailed information.
#'
#' @return Extracted token.
#' @keywords internal
#' @importFrom rvest read_html html_node html_attr
extract_token <- function(html_content, verbose = TRUE) {
  html_doc <- rvest::read_html(html_content)

  token_input <- rvest::html_node(html_doc, "input#token")

  if(is.na(token_input)) {
    stop("Could not find token input field in the web page. The site structure may have changed.")
  }

  token <- rvest::html_attr(token_input, "value")

  if(is.na(token) || token == "") {
    stop("Found token input field but couldn't extract the token value.")
  }

  if(verbose) message("Successfully extracted token: ", substr(token, 1, 20), "...")

  return(token)
}

#' Build request parameters
#'
#' @description
#' Creates parameter set for API request.
#'
#' @param start_index Start index for pagination.
#' @param batch_size Batch size.
#' @param token Security token.
#' @param tipo Announcement type.
#' @param start_date Start date (optional).
#' @param end_date End date (optional).
#'
#' @return List of parameters.
#' @keywords internal
#' @importFrom stats runif
build_request_params <- function(start_index, batch_size, token, tipo = 2, start_date = "", end_date = "") {
  params <- list(
    draw = as.character(round(stats::runif(1) * 10)),
    iColumns = "4",
    sColumns = ",,,",
    iDisplayStart = as.character(start_index),
    iDisplayLength = as.character(batch_size),
    mDataProp_0 = "0",
    mDataProp_1 = "1",
    mDataProp_2 = "2",
    mDataProp_3 = "3",
    iSortCol_0 = "0",
    sSortDir_0 = "desc",
    iSortingCols = "1",
    tipo = as.character(tipo),
    token = token,
    num_comunicado = "",
    fec_ini = "",
    fec_fin = ""
  )

  if(!is.na(start_date) && start_date != "") {
    params$fec_ini <- start_date
  }

  if(!is.na(end_date) && end_date != "") {
    params$fec_fin <- end_date
  }

  return(params)
}

#' Make API request
#'
#' @description
#' Makes POST request to API with provided parameters.
#'
#' @param url Endpoint URL.
#' @param params Request parameters.
#' @param cookies Cookies for request.
#' @param main_url Reference URL.
#'
#' @return HTTP response object.
#' @keywords internal
#' @importFrom httr POST set_cookies add_headers
make_api_request <- function(url, params, cookies, main_url) {
  tryCatch({
    httr::POST(
      url = url,
      body = params,
      encode = "form",
      httr::set_cookies(setNames(cookies$value, cookies$name)),
      httr::add_headers(
        `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36",
        `Accept` = "application/json, text/javascript, */*; q=0.01",
        `Content-Type` = "application/x-www-form-urlencoded; charset=UTF-8",
        `X-Requested-With` = "XMLHttpRequest",
        `Origin` = "https://consultasenlinea.produce.gob.pe",
        `Referer` = main_url
      )
    )
  }, error = function(e) {
    message("Error making request: ", e$message)
    return(NULL)
  })
}

#' Process JSON response
#'
#' @description
#' Processes JSON response from API and extracts announcements.
#'
#' @param content_text Response text.
#' @param verbose Show detailed information.
#'
#' @return Data frame with announcements or NULL if error.
#' @keywords internal
#' @importFrom jsonlite fromJSON
process_json_response <- function(content_text, verbose = TRUE) {
  tryCatch({
    data <- jsonlite::fromJSON(content_text)

    announcements <- data$aaData

    if(length(announcements) > 0) {
      df <- as.data.frame(announcements)

      if(ncol(df) >= 6) {
        colnames(df) <- c("ID", "Title", "Date", "FileName", "InternalFile", "Status")
      }

      return(df)
    } else {
      if(verbose) message("No announcements found in this batch.")
      return(NULL)
    }
  }, error = function(e) {
    message("Error parsing JSON: ", e$message)
    message("First 100 chars of response: ", substr(content_text, 1, 100))
    return(NULL)
  })
}

#' Generate download URL
#'
#' @description
#' Internal function to construct download URLs for files.
#'
#' @param file_name Name of the file to download.
#' @param base_url Base URL for downloads.
#'
#' @return Complete download URL.
#' @keywords internal
generate_download_url <- function(file_name, base_url = "https://consultasenlinea.produce.gob.pe/produce/descarga/comunicados/dgsfs/") {
  paste0(base_url, file_name)
}

#' Download file
#'
#' @description
#' Internal function to download a file from a URL.
#'
#' @param url Download URL.
#' @param file_name Local file name.
#' @param download_dir Download directory.
#' @param verbose Show progress messages.
#'
#' @return TRUE if download successful, FALSE otherwise.
#' @keywords internal
#' @importFrom utils download.file
download_file <- function(url, file_name, download_dir, verbose = TRUE) {
  dest_file <- file.path(download_dir, file_name)

  result <- tryCatch({
    utils::download.file(url, destfile = dest_file, mode = "wb", quiet = !verbose)
    return(TRUE)
  }, error = function(e) {
    message("Error downloading ", file_name, ": ", e$message)
    return(FALSE)
  })

  return(result)
}

#' Fetch announcements batch
#'
#' @description
#' Retrieves batch of announcements using token.
#'
#' @param start_index Start index for pagination.
#' @param batch_size Batch size.
#' @param token Security token.
#' @param cookies Cookies for request.
#' @param main_url Reference URL.
#' @param tipo Announcement type.
#' @param start_date Start date (optional).
#' @param end_date End date (optional).
#' @param verbose Show detailed information.
#'
#' @return Data frame with announcements or NULL if error.
#' @keywords internal
#' @importFrom httr status_code content
fetch_announcements_batch <- function(
  start_index,
  batch_size,
  token,
  cookies,
  main_url,
  tipo = 2,
  start_date = "",
  end_date = "",
  verbose = TRUE
) {
  if(verbose) message("Fetching records ", start_index + 1, " to ", start_index + batch_size)

  url <- "https://consultasenlinea.produce.gob.pe/ConsultasEnLinea/consultas.web/ajax/listado.comunicados.ajax.php"

  params <- build_request_params(start_index, batch_size, token, tipo, start_date, end_date)
  response <- make_api_request(url, params, cookies, main_url)

  if(is.null(response)) return(NULL)

  status <- httr::status_code(response)
  if(status != 200) {
    message("Request failed with status code: ", status)
    return(NULL)
  }

  content_text <- httr::content(response, "text", encoding = "UTF-8")

  if(verbose && nchar(content_text) < 50) {
    message("Warning: Short response received: ", content_text)
    return(NULL)
  }

  return(process_json_response(content_text, verbose))
}

#' Process PDF text content
#'
#' @description
#' Internal function to extract structured data from PDF text content.
#'
#' @param text PDF text content.
#' @param file_name Source file name.
#' @param verbose Show processing messages.
#'
#' @return Data frame with extracted information.
#' @keywords internal
#' @importFrom stringr str_squish str_split str_extract str_extract_all
process_pdf_text <- function(text, file_name, verbose = TRUE) {
  empty_result <- data.frame(
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

  clean_text <- stringr::str_squish(text)
  clean_text <- iconv(clean_text, from = "UTF-8", to = "ASCII//TRANSLIT")

  if (!any(grepl("DISPONER LA SUSPENSION PREVENTIVA DE LA ACTIVIDAD EXTRACTIVA", clean_text))) {
    if (verbose) warning("File '", file_name, "' doesn't appear to be a valid announcement.")
    return(empty_result)
  }

  blocks <- tryCatch({
    stringr::str_split(clean_text, "DISPONER LA SUSPENSION PREVENTIVA DE LA ACTIVIDAD EXTRACTIVA")[[1]]
  }, error = function(e) {
    if (verbose) warning("Error splitting text for file '", file_name, "': ", e$message)
    return(character(0))
  })

  if (length(blocks) <= 1) {
    if (verbose) warning("No valid information blocks found in file '", file_name, "'.")
    return(empty_result)
  }

  announcement <- stringr::str_extract(clean_text, "COMUNICADO\\s*N[?]\\s*\\d+(?:\\s*[-]\\s*\\d+)?(?:-[A-Z]+)?")

  results <- list()

  for (i in 2:length(blocks)) {
    block <- blocks[i]

    tryCatch({
      pattern_dates <- "(\\d{2}:\\d{2} horas del \\d{2} de \\w+ de \\d{4})"
      dates_text <- stringr::str_extract_all(block, pattern_dates)[[1]]

      if (length(dates_text) < 2) {
        if (verbose) warning("Not enough dates found in block ", i, " of file '", file_name, "'.")
        next
      }

      dates <- gsub("horas del ", "", dates_text)
      final_dates <- as.POSIXct(dates, format = "%H:%M %d de %B de %Y", tz = "America/Lima")

      if (any(is.na(final_dates))) {
        if (verbose) warning("Error converting dates in block ", i, " of file '", file_name, "'.")
        next
      }
    }, error = function(e) {
      if (verbose) warning("Error processing dates in block ", i, " of file '", file_name, "': ", e$message)
      final_dates <- rep(as.POSIXct(NA), 2)
    })

    data_positions_lat <- character(0)
    data_positions_lon <- character(0)

    tryCatch({
      lat_patterns <- c(
        "(\\d{1,2})\\?(\\d{1,2})'?([NS])",
        "(\\d{1,2})\\s+(\\d{1,2})\\s*([NS])",
        "(\\d{1,2})(\\d{2})([NS])",
        "(\\d{1,2})[^0-9](\\d{1,2})[^0-9]*([NS])"
      )

      lon_patterns <- c(
        "(\\d{1,2})\\?(\\d{1,2})'?([WEO])",
        "(\\d{1,2})\\s+(\\d{1,2})\\s*([WEO])",
        "(\\d{1,2})(\\d{2})([WEO])",
        "(\\d{1,2})[^0-9](\\d{1,2})[^0-9]*([WEO])"
      )

      for (pattern in lat_patterns) {
        data_positions_lat <- stringr::str_extract_all(block, pattern)[[1]]
        if (length(data_positions_lat) > 0) break
      }

      for (pattern in lon_patterns) {
        data_positions_lon <- stringr::str_extract_all(block, pattern)[[1]]
        if (length(data_positions_lon) > 0) break
      }

    }, error = function(e) {
      if (verbose) warning("Error extracting coordinates in block ", i, " of file '", file_name, "': ", e$message)
    })

    if (length(data_positions_lon) == 0) {
      start_miles <- NA_real_
      end_miles <- NA_real_

      tryCatch({
        nautical_patterns <- c(
          "de (\\d+) a (\\d+) millas nauticas",
          "(?:entre las )?(\\d+) y (\\d+) millas nauticas",
          "dentro de las (\\d+) millas nauticas",
          "(\\d+)\\s*a\\s*(\\d+)\\s*mn"
        )

        for (pattern in nautical_patterns) {
          miles_matches <- stringr::str_extract_all(block, pattern)[[1]]
          if (length(miles_matches) > 0) {
            miles_numbers <- safe_numeric_conversion(stringr::str_extract_all(miles_matches[1], "\\d+")[[1]])
            if (length(miles_numbers) >= 2) {
              start_miles <- min(miles_numbers)
              end_miles <- max(miles_numbers)
            } else if (length(miles_numbers) == 1) {
              end_miles <- miles_numbers[1]
            }
            break
          }
        }
      }, error = function(e) {
        if (verbose) warning("Error processing nautical miles in block ", i, " of file '", file_name, "': ", e$message)
      })

      if (length(data_positions_lat) >= 2) {
        df <- data.frame(
          StartDateTime = final_dates[1],
          EndDateTime = final_dates[2],
          StartLatitude = data_positions_lat[1],
          EndLatitude = data_positions_lat[2],
          StartLongitude = NA_character_,
          EndLongitude = NA_character_,
          StartNauticalMiles = start_miles,
          EndNauticalMiles = end_miles,
          file_name = file_name,
          announcement = announcement,
          stringsAsFactors = FALSE
        )
        results <- append(results, list(df))
      }
    } else {
      if (length(data_positions_lat) >= 2 && length(data_positions_lon) >= 2) {
        df <- data.frame(
          StartDateTime = final_dates[1],
          EndDateTime = final_dates[2],
          StartLatitude = data_positions_lat[1],
          EndLatitude = data_positions_lat[2],
          StartLongitude = data_positions_lon[1],
          EndLongitude = data_positions_lon[2],
          StartNauticalMiles = NA_real_,
          EndNauticalMiles = NA_real_,
          file_name = file_name,
          announcement = announcement,
          stringsAsFactors = FALSE
        )
        results <- append(results, list(df))
      }
    }
  }

  if (length(results) > 0) {
    return(do.call(rbind, results))
  } else {
    if (verbose) warning("Could not extract information from file '", file_name, "'.")
    return(empty_result)
  }
}

#' Find parallel line by distance
#'
#' @description
#' Internal function to find the parallel line closest to a specified distance.
#'
#' @param miles Distance in nautical miles.
#' @param coast_parallels List of parallel line data frames.
#'
#' @return List with data frame and actual mile value.
#' @keywords internal
find_parallel_line <- function(miles, coast_parallels) {
  available_miles <- safe_numeric_conversion(gsub("l", "", names(coast_parallels)))
  idx_close <- which.min(abs(available_miles - miles))
  close_mile <- available_miles[idx_close]

  return(list(
    df = coast_parallels[[paste0("l", close_mile)]],
    actual_mile = close_mile
  ))
}

#' Interpolate point on line
#'
#' @description
#' Internal function to interpolate a point for a specific latitude on a parallel line.
#'
#' @param line Data frame representing a parallel line.
#' @param latitude Target latitude.
#' @param lat_name Name of latitude column.
#' @param lon_name Name of longitude column.
#'
#' @return Vector with interpolated longitude and latitude.
#' @keywords internal
interpolate_point <- function(line, latitude, lat_name, lon_name) {
  line <- line[order(line[[lat_name]]), ]

  idx_inf <- max(which(line[[lat_name]] <= latitude), na.rm = TRUE)
  idx_sup <- min(which(line[[lat_name]] >= latitude), na.rm = TRUE)

  if (length(idx_inf) == 0 || is.infinite(idx_inf)) {
    idx_close <- which.min(abs(line[[lat_name]] - latitude))
    return(c(
      lon = line[[lon_name]][idx_close],
      lat = line[[lat_name]][idx_close]
    ))
  }

  if (length(idx_sup) == 0 || is.infinite(idx_sup)) {
    idx_close <- which.min(abs(line[[lat_name]] - latitude))
    return(c(
      lon = line[[lon_name]][idx_close],
      lat = line[[lat_name]][idx_close]
    ))
  }

  if (idx_inf == idx_sup) {
    return(c(
      lon = line[[lon_name]][idx_inf],
      lat = line[[lat_name]][idx_inf]
    ))
  }

  lat_inf <- line[[lat_name]][idx_inf]
  lat_sup <- line[[lat_name]][idx_sup]
  lon_inf <- line[[lon_name]][idx_inf]
  lon_sup <- line[[lon_name]][idx_sup]

  prop <- (latitude - lat_inf) / (lat_sup - lat_inf)
  longitude <- lon_inf + prop * (lon_sup - lon_inf)

  return(c(
    lon = longitude,
    lat = latitude
  ))
}

#' Calculate coast longitude
#'
#' @description
#' Internal function to calculate approximate longitude of coast at a given latitude.
#'
#' @param coastline Coastline data frame.
#' @param latitude Target latitude.
#'
#' @return Approximate coast longitude.
#' @keywords internal
calculate_coast_longitude <- function(coastline, latitude) {
  if (!all(c("Lat", "Long") %in% names(coastline))) {
    stop("Coastline must contain 'Lat' and 'Long' columns")
  }

  if (!is.numeric(latitude)) {
    stop("Latitude must be numeric")
  }

  nearby_idx <- which(abs(coastline$Lat - latitude) < 0.1)
  if (length(nearby_idx) == 0) {
    warning("No coastal points found near latitude ", latitude, ". Using approximation.")
    return(-75)
  }

  coast_longitude <- mean(coastline$Long[nearby_idx])
  return(coast_longitude)
}

#' Extract points between latitudes
#'
#' @description
#' Extracts all points of a parallel line between two latitudes.
#'
#' @param line Data frame representing a line parallel to the coast.
#' @param lat_min Minimum latitude (southernmost).
#' @param lat_max Maximum latitude (northernmost).
#' @param lat_name Name of latitude column.
#' @param lon_name Name of longitude column.
#'
#' @return Matrix of points with longitude and latitude columns.
#' @keywords internal
extract_points_between_latitudes <- function(line, lat_min, lat_max, lat_name, lon_name) {
  line <- line[order(line[[lat_name]]), ]

  filtered_points <- line[line[[lat_name]] >= lat_min & line[[lat_name]] <= lat_max, ]

  if (nrow(filtered_points) < 2) {
    min_point <- interpolate_point(line, lat_min, lat_name, lon_name)
    max_point <- interpolate_point(line, lat_max, lat_name, lon_name)

    points <- rbind(
      min_point,
      max_point
    )
    colnames(points) <- c(lon_name, lat_name)

    return(points)
  }

  first_point <- filtered_points[1, ]
  last_point <- filtered_points[nrow(filtered_points), ]

  limit_points <- NULL

  if (!is.na(first_point[[lat_name]]) && abs(first_point[[lat_name]] - lat_min) > 0.0001) {
    min_point <- interpolate_point(line, lat_min, lat_name, lon_name)
    limit_points <- rbind(limit_points, min_point)
  }

  middle_points <- as.matrix(filtered_points[, c(lon_name, lat_name)])

  if (!is.na(last_point[[lat_name]]) && abs(last_point[[lat_name]] - lat_max) > 0.0001) {
    max_point <- interpolate_point(line, lat_max, lat_name, lon_name)
    limit_points <- rbind(limit_points, max_point)
  }

  if (!is.null(limit_points)) {
    colnames(limit_points) <- c(lon_name, lat_name)

    points_before <- limit_points[limit_points[, 2] < first_point[[lat_name]], , drop = FALSE]
    points_after <- limit_points[limit_points[, 2] > last_point[[lat_name]], , drop = FALSE]

    points <- rbind(
      points_before,
      middle_points,
      points_after
    )
  } else {
    points <- middle_points
  }

  return(points)
}

#' Prepare polygons from coordinate data
#'
#' @description
#' Helper function to process data and prepare polygons using preexisting parallel lines.
#'
#' @param data Data frame with coordinates.
#' @param coastline Data frame with coastline.
#' @param coast_parallels List of data frames with lines parallel to the coast.
#' @param column_names Vector with column names in coast_parallels data frames.
#'
#' @return List of polygons for visualization.
#' @keywords internal
prepare_polygons <- function(data, coastline, coast_parallels = NULL, column_names = c("lat", "lon", "dc")) {
  if (missing(data)) {
    stop("The 'data' parameter is required.")
  }

  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame.")
  }

  if (!is.data.frame(coastline) || !all(c("Long", "Lat") %in% names(coastline))) {
    stop("'coastline' must be a data.frame with columns 'Long' and 'Lat'.")
  }

  if (nrow(data) == 0) {
    stop("The data frame 'data' contains no rows.")
  }

  if (!is.null(coast_parallels)) {
    if (!is.list(coast_parallels)) {
      stop("'coast_parallels' must be a list of data.frames.")
    }

    for (name in names(coast_parallels)) {
      df <- coast_parallels[[name]]
      if (!is.data.frame(df)) {
        warning("The element '", name, "' in coast_parallels is not a data.frame.")
        next
      }
      if (!all(column_names %in% names(df))) {
        warning("The element '", name, "' in coast_parallels does not have all the required columns: ",
                paste(column_names, collapse = ", "))
      }
    }

    valid_elements <- sapply(names(coast_parallels), function(name) {
      df <- coast_parallels[[name]]
      is.data.frame(df) && all(column_names %in% names(df))
    })

    if (!any(valid_elements)) {
      warning("No element in coast_parallels has the correct format. The original approximation method will be used.")
      coast_parallels <- NULL
    }
  }

  validate_polygon_data(data)

  prepared_data <- data

  if (any(c("StartLatitude", "EndLatitude") %in% names(data))) {
    if (!"lat_ini" %in% names(data)) {
      prepared_data$lat_ini <- dms_to_decimal(data$StartLatitude)
    }
    if (!"lat_fin" %in% names(data)) {
      prepared_data$lat_fin <- dms_to_decimal(data$EndLatitude)
    }
  }

  if (any(c("StartLongitude", "EndLongitude") %in% names(data))) {
    if (!"lon_ini" %in% names(data)) {
      prepared_data$lon_ini <- dms_to_decimal(data$StartLongitude)
    }
    if (!"lon_fin" %in% names(data)) {
      prepared_data$lon_fin <- dms_to_decimal(data$EndLongitude)
    }
  }

  polygons <- list()

  for (i in 1:nrow(prepared_data)) {
    if (!is.na(data$StartNauticalMiles[i]) && !is.na(data$EndNauticalMiles[i]) &&
        (is.na(prepared_data$lon_ini[i]) || is.na(prepared_data$lon_fin[i]))) {

      lat_north <- max(prepared_data$lat_ini[i], prepared_data$lat_fin[i])
      lat_south <- min(prepared_data$lat_ini[i], prepared_data$lat_fin[i])

      miles_ini <- data$StartNauticalMiles[i]
      miles_fin <- data$EndNauticalMiles[i]

      if (!is.null(coast_parallels)) {
        lat_name <- column_names[1]
        lon_name <- column_names[2]

        line_ini_info <- find_parallel_line(miles_ini, coast_parallels)
        line_fin_info <- find_parallel_line(miles_fin, coast_parallels)

        line_ini <- line_ini_info$df
        line_fin <- line_fin_info$df

        if (!is.null(line_ini) && !is.null(line_fin) &&
            nrow(line_ini) > 0 && nrow(line_fin) > 0) {

          point_ne <- interpolate_point(line_ini, lat_north, lat_name, lon_name)
          point_se <- interpolate_point(line_ini, lat_south, lat_name, lon_name)
          point_nw <- interpolate_point(line_fin, lat_north, lat_name, lon_name)
          point_sw <- interpolate_point(line_fin, lat_south, lat_name, lon_name)

          west_points <- extract_points_between_latitudes(
            line_fin, lat_south, lat_north, lat_name, lon_name
          )

          east_points <- extract_points_between_latitudes(
            line_ini, lat_south, lat_north, lat_name, lon_name
          )

          west_points <- west_points[order(west_points[, 2], decreasing = TRUE), ]
          east_points <- east_points[order(east_points[, 2]), ]

          coords <- rbind(
            east_points,
            west_points,
            east_points[1, ]
          )

          polygon <- list(
            coords = coords,
            id = i,
            start_date = if ("StartDateTime" %in% names(data)) data$StartDateTime[i] else NA,
            end_date = if ("EndDateTime" %in% names(data)) data$EndDateTime[i] else NA,
            file_name = if ("file_name" %in% names(data)) data$file_name[i] else NA,
            Start_Long = if ("StartLongitude" %in% names(data)) data$StartLongitude[i] else NA,
            Start_Lat = if ("StartLatitude" %in% names(data)) data$StartLatitude[i] else NA,
            End_Long = if ("EndLongitude" %in% names(data)) data$EndLongitude[i] else NA,
            End_Lat = if ("EndLatitude" %in% names(data)) data$EndLatitude[i] else NA,
            StartNauticalMiles = miles_ini,
            EndNauticalMiles = miles_fin,
            announcement = if ("announcement" %in% names(data)) data$announcement[i] else paste("Polygon", i),
            actual_mile_ini = line_ini_info$actual_mile,
            actual_mile_fin = line_fin_info$actual_mile,
            method = "detailed"
          )

          polygons[[i]] <- polygon
          next
        }
      }

      coast_lon_lat_north <- calculate_coast_longitude(coastline, lat_north)
      coast_lon_lat_south <- calculate_coast_longitude(coastline, lat_south)

      factor_lat_north <- cos(lat_north * pi/180)
      factor_lat_south <- cos(lat_south * pi/180)

      offset_ini_lat_north <- miles_ini / 60 / factor_lat_north
      offset_fin_lat_north <- miles_fin / 60 / factor_lat_north
      offset_ini_lat_south <- miles_ini / 60 / factor_lat_south
      offset_fin_lat_south <- miles_fin / 60 / factor_lat_south

      coords <- rbind(
        c(coast_lon_lat_north - offset_ini_lat_north, lat_north),
        c(coast_lon_lat_north - offset_fin_lat_north, lat_north),
        c(coast_lon_lat_south - offset_fin_lat_south, lat_south),
        c(coast_lon_lat_south - offset_ini_lat_south, lat_south),
        c(coast_lon_lat_north - offset_ini_lat_north, lat_north)
      )

      polygon <- list(
        coords = coords,
        id = i,
        start_date = if ("StartDateTime" %in% names(data)) data$StartDateTime[i] else NA,
        end_date = if ("EndDateTime" %in% names(data)) data$EndDateTime[i] else NA,
        file_name = if ("file_name" %in% names(data)) data$file_name[i] else NA,
        Start_Long = if ("StartLongitude" %in% names(data)) data$StartLongitude[i] else NA,
        Start_Lat = if ("StartLatitude" %in% names(data)) data$StartLatitude[i] else NA,
        End_Long = if ("EndLongitude" %in% names(data)) data$EndLongitude[i] else NA,
        End_Lat = if ("EndLatitude" %in% names(data)) data$EndLatitude[i] else NA,
        StartNauticalMiles = miles_ini,
        EndNauticalMiles = miles_fin,
        announcement = if ("announcement" %in% names(data)) data$announcement[i] else paste("Polygon", i),
        method = "approximate"
      )

      polygons[[i]] <- polygon

    } else if (!is.na(prepared_data$lat_ini[i]) && !is.na(prepared_data$lat_fin[i]) &&
               !is.na(prepared_data$lon_ini[i]) && !is.na(prepared_data$lon_fin[i])) {

      coords <- rbind(
        c(prepared_data$lon_ini[i], prepared_data$lat_ini[i]),
        c(prepared_data$lon_fin[i], prepared_data$lat_ini[i]),
        c(prepared_data$lon_fin[i], prepared_data$lat_fin[i]),
        c(prepared_data$lon_ini[i], prepared_data$lat_fin[i]),
        c(prepared_data$lon_ini[i], prepared_data$lat_ini[i])
      )

      polygon <- list(
        coords = coords,
        id = i,
        start_date = if ("StartDateTime" %in% names(data)) data$StartDateTime[i] else NA,
        end_date = if ("EndDateTime" %in% names(data)) data$EndDateTime[i] else NA,
        file_name = if ("file_name" %in% names(data)) data$file_name[i] else NA,
        Start_Long = if ("StartLongitude" %in% names(data)) data$StartLongitude[i] else NA,
        Start_Lat = if ("StartLatitude" %in% names(data)) data$StartLatitude[i] else NA,
        End_Long = if ("EndLongitude" %in% names(data)) data$EndLongitude[i] else NA,
        End_Lat = if ("EndLatitude" %in% names(data)) data$EndLatitude[i] else NA,
        StartNauticalMiles = if ("StartNauticalMiles" %in% names(data)) data$StartNauticalMiles[i] else NA,
        EndNauticalMiles = if ("EndNauticalMiles" %in% names(data)) data$EndNauticalMiles[i] else NA,
        announcement = if ("announcement" %in% names(data)) data$announcement[i] else paste("Polygon", i),
        method = "explicit"
      )

      polygons[[i]] <- polygon
    } else {
      warning("Row ", i, " does not have sufficient data to create a valid polygon.")
    }
  }

  polygons <- polygons[!sapply(polygons, is.null)]

  if (length(polygons) == 0) {
    stop("No valid polygons could be created with the provided data.")
  }

  return(polygons)
}

#' Validate data for polygon creation
#'
#' @description
#' Validates that a data frame contains the necessary columns for creating
#' fishing zone polygons. Checks for either coordinate-based or distance-based
#' polygon definition data.
#'
#' @param data Data frame to validate. Must contain polygon definition columns.
#'
#' @details
#' The function requires either:
#' \itemize{
#'   \item Coordinate-based: StartLatitude, EndLatitude, StartLongitude, EndLongitude
#'   \item Distance-based: StartLatitude, EndLatitude, StartNauticalMiles, EndNauticalMiles
#' }
#'
#' @return Returns TRUE invisibly if validation passes, otherwise throws an error.
#'
#' @examples
#' # Coordinate-based polygon data
#' coord_data <- data.frame(
#'   StartLatitude = "15 30 S",
#'   EndLatitude = "15 45 S",
#'   StartLongitude = "75 30 W",
#'   EndLongitude = "75 45 W"
#' )
#' validate_polygon_data(coord_data)
#'
#' # Distance-based polygon data
#' distance_data <- data.frame(
#'   StartLatitude = "15 30 S",
#'   EndLatitude = "15 45 S",
#'   StartNauticalMiles = 5,
#'   EndNauticalMiles = 15
#' )
#' validate_polygon_data(distance_data)
#'
#' @export
validate_polygon_data <- function(data) {

  # Basic validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame")
  }

  if (nrow(data) == 0) {
    stop("'data' contains no rows")
  }

  # Define required column sets
  coordinate_cols <- c("StartLatitude", "EndLatitude", "StartLongitude", "EndLongitude")
  distance_cols <- c("StartLatitude", "EndLatitude", "StartNauticalMiles", "EndNauticalMiles")

  # Check if either set is complete
  has_coordinates <- all(coordinate_cols %in% names(data))
  has_distances <- all(distance_cols %in% names(data))

  if (!has_coordinates && !has_distances) {
    stop("Data must contain either complete coordinate columns (StartLatitude, EndLatitude, StartLongitude, EndLongitude) or complete distance columns (StartLatitude, EndLatitude, StartNauticalMiles, EndNauticalMiles)")
  }

  # Validate nautical miles if present
  if (has_distances) {
    for (col in c("StartNauticalMiles", "EndNauticalMiles")) {
      values <- suppressWarnings(as.numeric(data[[col]]))
      if (any(values < 0, na.rm = TRUE)) {
        stop("Nautical mile values must be non-negative in column: ", col)
      }
    }
  }

  return(invisible(TRUE))
}
