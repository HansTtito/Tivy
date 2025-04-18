#' Search for columns that contain a pattern followed by a number
#'
#' Identifies columns in a dataframe whose names follow the pattern
#' of a specific prefix followed by a number.
#'
#' @param data A data frame to search for columns.
#' @param pattern The pattern or prefix to search for (default "pond_").
#' @param sort Indicates if columns should be sorted numerically (default TRUE).
#' @return Character vector with the names of columns that match the pattern.
#' @export
#' @examples
#' 
#' data(calas_bitacora, faenas_bitacora, tallas_bitacora)
#' 
#' # Identify all pond_X columns in the dataframe
#' data_hauls <- process_hauls(data_hauls = calas_bitacora)
#' data_fishing_trips <- process_fishing_trips(data_fishing_trips = faenas_bitacora)
#' hauls_length <- process_length(data_length = tallas_bitacora)
#'
#' data_length_trips <- merge(
#'    x = data_fishing_trips, 
#'    y = hauls_length, 
#'    by = 'fishing_trip_code'
#' )
#'  
#' data_total <- merge_length_fishing_trips_hauls(
#'    data_hauls = data_hauls, 
#'    data_length_fishing_trips = data_length_trips
#' )
#' 
#' final_data <- add_variables(data_total)
#'
#' length_cols <- c("8", "8.5", "9", "9.5", "10", "10.5", 
#'                  "11", "11.5","12", "12.5", "13",
#'                  "13.5", "14", "14.5", "15")
#' 
#' length_weighted <- weight_length_df(
#'    df = final_data, 
#'    length_cols = length_cols, 
#'    catch_col = "catch_ANCHOVETA", 
#'    a = 0.0001, 
#'    b = 2.984
#' )
#'
#' pond_cols <- search_pattern_columns(data = length_weighted, pattern = "pond_")
#'
#' # Use the found columns for calculations
#' length_weighted[, pond_cols]
search_pattern_columns <- function(data, pattern = "pond_", sort = TRUE) {
  # Parameter validation
  if (!is.data.frame(data)) stop("The 'data' parameter must be a data.frame.")
  if (!is.character(pattern)) stop("The 'pattern' parameter must be a text string.")

  # Get all column names
  column_names <- colnames(data)

  # Search for columns that contain the pattern
  reg_exp <- paste0("^", pattern, "[0-9]+(\\.[0-9]+)?$")
  matching_columns <- column_names[grep(reg_exp, column_names)]

  if (length(matching_columns) == 0) {
    warning("No columns matching the pattern were found: ", pattern)
    return(character(0))
  }

  # Sort numerically if necessary
  if (sort) {
    # Extract numerical values from column names
    numerical_values <- as.numeric(gsub(pattern, "", matching_columns))
    # Sort columns according to these values
    matching_columns <- matching_columns[order(numerical_values)]
  }

  return(matching_columns)
}



#' Extract numerical values from column names
#'
#' This function extracts all numerical values (including decimals) from a vector
#' of column names, removing any non-numeric characters.
#'
#' @param column_names Character vector with column names.
#' @return Numeric vector with the extracted length values.
#' @export
#' @examples
#' names <- c("pond_10.5", "length_11", "12.5mm", "T14", "13-error")
#' extract_length_values(names)
#' # Result: 10.5 11.0 12.5 14.0 13.0
extract_length_values <- function(column_names) {
  if (!is.character(column_names)) {
    stop("The 'column_names' parameter must be a character vector.")
  }

  # Extract the first occurrence of a number in each string (supports decimals)
  values <- as.numeric(stringr::str_extract(column_names, "\\d+\\.?\\d*"))

  if (any(is.na(values))) {
    warning("Some names do not contain numerical values and were converted to NA.")
  }

  return(values)
}


#' Get names and positions of columns that match a pattern
#'
#' Returns both the names and positions of columns in a dataframe
#' that follow the specified pattern.
#'
#' @param data A data frame to search for columns.
#' @param pattern The pattern or prefix to search for (default "pond_").
#' @param sort Indicates if results should be sorted numerically (default TRUE).
#' @return List with two elements: "positions" and "names" of matching columns.
#' @export
#' @examples
#' # Get both positions and names
#'
#' data(calas_bitacora, faenas_bitacora, tallas_bitacora)
#'
#' data_hauls <- process_hauls(data_hauls = calas_bitacora)
#' data_fishing_trips <- process_fishing_trips(data_fishing_trips = faenas_bitacora)
#' hauls_length <- process_length(data_length = tallas_bitacora)
#'
#' data_length_trips <- merge(
#'    x = data_fishing_trips, 
#'    y = hauls_length, 
#'    by = 'fishing_trip_code'
#' )
#' 
#' data_total <- merge_length_fishing_trips_hauls(
#'    data_hauls = data_hauls, 
#'    data_length_fishing_trips = data_length_trips
#' )
#' 
#' final_data <- add_variables(data_total)
#'
#' length_cols <- c("8", "8.5", "9", "9.5", "10", "10.5", 
#'                  "11", "11.5","12", "12.5", "13", "13.5",
#'                  "14", "14.5", "15")
#' 
#' length_weighted <- weight_length_df(
#'    df = final_data, 
#'    length_cols = length_cols, 
#'    catch_col = "catch_ANCHOVETA", 
#'    a = 0.0001, 
#'    b = 2.984
#' )
#'
#' result <- pattern_columns_info(length_weighted, "pond_")
#'
#' positions <- result$positions
#' print(positions)
#'
#' names <- result$names
#' print(names)
pattern_columns_info <- function(data, pattern = "pond_", sort = TRUE) {
  # Parameter validation
  if (!is.data.frame(data)) stop("The 'data' parameter must be a data.frame.")
  if (!is.character(pattern)) stop("The 'pattern' parameter must be a text string.")

  # Get all column names
  column_names <- colnames(data)

  # Search for columns that contain the pattern
  reg_exp <- paste0("^", pattern, "[0-9]+(\\.[0-9]+)?$")
  indices <- grep(reg_exp, column_names)

  if (length(indices) == 0) {
    warning("No columns matching the pattern were found: ", pattern)
    return(list(positions = integer(0), names = character(0)))
  }

  matching_columns <- column_names[indices]

  # Sort numerically if necessary
  if (sort) {
    # Extract numerical values from column names
    numerical_values <- as.numeric(gsub(pattern, "", matching_columns))
    # Sort according to these values
    order <- order(numerical_values)
    indices <- indices[order]
    matching_columns <- matching_columns[order]
  }

  return(list(
    positions = indices,
    names = matching_columns
  ))
}



#' Calculate juvenile percentages for a set of frequencies
#'
#' Helper function that calculates juvenile percentages both in number and weight
#' from a set of length frequencies.
#'
#' @param frequencies Numeric vector with frequencies by length.
#' @param length_values Numeric vector with the corresponding length values.
#' @param juvLim Length limit to consider juveniles (default 12 cm).
#' @param a Coefficient of the length-weight relationship.
#' @param b Exponent of the length-weight relationship.
#' @return Data frame with juvenile percentages and totals.
#' @export
#' @examples
#' frequencies <- c(10, 15, 25, 30, 20, 10)
#' length_values <- c(8, 9, 10, 11, 12, 13)
#' calculate_juveniles(frequencies, length_values)
calculate_juveniles <- function(frequencies, length_values, juvLim = 12, a = 0.0012, b = 3.1242) {
  # Parameter validation
  if (!is.numeric(frequencies)) stop("The 'frequencies' parameter must be numeric.")
  if (!is.numeric(length_values)) stop("The 'length_values' parameter must be numeric.")
  if (length(frequencies) != length(length_values))
    stop("The 'frequencies' and 'length_values' vectors must have the same length.")

  # Check if there is data
  total_number <- sum(frequencies, na.rm = TRUE)

  if (total_number == 0) {
    # If there is no data, return NA without warnings
    return(data.frame(
      perc_juv_number = NA_real_,
      perc_juv_weight = NA_real_,
      total_number = 0,
      total_weight = 0,
      juvenil_number = 0,
      juvenil_weight = 0
    ))
  }

  # Calculate juveniles in number with the existing function
  # We use suppressWarnings to avoid redundant warnings
  perc_juv_number <- suppressWarnings(juvenile_percentage(frequencies, length_values, juvLim))

  # Calculate weights
  weights <- length_weight(length_values, a, b) * frequencies
  total_catch <- sum(weights, na.rm = TRUE) / 1000

  # Calculate juveniles in weight with the same function
  if (total_catch == 0) {
    perc_juv_weight <- NA_real_
  } else {
    perc_juv_weight <- suppressWarnings(juvenile_percentage(weights, length_values, juvLim))
  }

  return(data.frame(
    perc_juv_number = perc_juv_number,
    perc_juv_weight = perc_juv_weight,
    total_number = total_number,
    total_weight = total_catch,
    juvenil_number = total_number * perc_juv_number / 100,
    juvenil_weight = total_catch * perc_juv_weight / 100
  ))
}


#' @title Verificar rango de fechas
#' @description Comprueba si una fecha está dentro del rango especificado
#' @param date_str Fecha a comprobar
#' @param start_date Fecha de inicio del rango
#' @param end_date Fecha de fin del rango
#' @return Lógico indicando si la fecha está en el rango
#' @keywords internal
is_date_in_range <- function(date_str, start_date, end_date) {
  date_part <- convert_to_date(date_str, type = "date")
  date <- convert_to_date(date_part, type = "date")
  start <- convert_to_date(start_date, type = "date")
  end <- convert_to_date(end_date, type = "date")
  return(date >= start && date <= end)
}

# Funciones de conexión y validación
#' @title Verificar paquetes requeridos
#' @description Comprueba si están instalados los paquetes necesarios
#' @return TRUE si los paquetes están disponibles
#' @keywords internal
check_required_packages <- function() {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required. Please install it with install.packages('httr')")
  }
  if (!requireNamespace("rvest", quietly = TRUE)) {
    stop("Package 'rvest' is required. Please install it with install.packages('rvest')")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required. Please install it with install.packages('jsonlite')")
  }
  return(TRUE)
}

#' @title Obtener página principal
#' @description Realiza la petición a la página principal y devuelve la respuesta
#' @param verbose Mostrar información detallada
#' @return Objeto de respuesta HTTP
#' @keywords internal
#' @importFrom httr GET add_headers
get_main_page <- function(verbose = TRUE) {
  main_url <- "https://consultasenlinea.produce.gob.pe/ConsultasEnLinea/consultas.web/comunicados/suspensionPreventiva"
  
  if(verbose) message("Fetching main page to extract token...")
  
  httr::GET(
    main_url,
    httr::add_headers(
      `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36",
      `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
      `Accept-Language` = "en-US,en;q=0.5"
    )
  )
}

#' @title Extraer cookies
#' @description Extrae las cookies de una respuesta HTTP
#' @param response Respuesta HTTP
#' @param verbose Mostrar información detallada
#' @return DataFrame con cookies
#' @keywords internal
#' @importFrom httr cookies
extract_cookies <- function(response, verbose = TRUE) {
  cookies <- httr::cookies(response)
  if(verbose && length(cookies) > 0) {
    message("Found ", nrow(cookies), " cookies")
  }
  return(cookies)
}

#' @title Extraer token
#' @description Extrae el token de seguridad de la página HTML
#' @param html_content Contenido HTML de la página
#' @param verbose Mostrar información detallada
#' @return Token extraído
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

# Funciones para manejar directorios
#' @title Crear directorio de descarga
#' @description Crea el directorio para descargar archivos si no existe
#' @param download_dir Ruta del directorio de descarga
#' @param verbose Mostrar información detallada
#' @return TRUE si se creó el directorio o ya existía
#' @keywords internal
create_download_dir <- function(download_dir, verbose = TRUE) {
  if(!dir.exists(download_dir)) {
    dir.create(download_dir, recursive = TRUE)
    if(verbose) message("Created download directory: ", download_dir)
  }
  return(TRUE)
}

# Funciones de API y procesamiento
#' @title Construir parámetros de solicitud
#' @description Crea el conjunto de parámetros para la petición API
#' @param start_index Índice de inicio para paginación
#' @param batch_size Tamaño del lote
#' @param token Token de seguridad
#' @param tipo Tipo de anuncio
#' @param start_date Fecha de inicio (opcional)
#' @param end_date Fecha de fin (opcional)
#' @return Lista de parámetros
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
  
  # Añadir parámetros de fecha si se proporcionan
  if(!is.na(start_date) && start_date != "") {
    params$fec_ini <- start_date
  }
  
  if(!is.na(end_date) && end_date != "") {
    params$fec_fin <- end_date
  }
  
  return(params)
}

#' @title Realizar petición API
#' @description Realiza la petición POST al API con los parámetros proporcionados
#' @param url URL del endpoint
#' @param params Parámetros de la petición
#' @param cookies Cookies para la petición
#' @param main_url URL de referencia
#' @return Objeto de respuesta HTTP
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

#' @title Procesar respuesta JSON
#' @description Procesa la respuesta JSON del API y extrae los anuncios
#' @param content_text Texto de la respuesta
#' @param verbose Mostrar información detallada
#' @return DataFrame con los anuncios o NULL si hay error
#' @keywords internal
#' @importFrom jsonlite fromJSON
process_json_response <- function(content_text, verbose = TRUE) {
  tryCatch({
    data <- jsonlite::fromJSON(content_text)
    
    # Extraer anuncios
    announcements <- data$aaData
    
    # Convertir a data frame
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

#' @title Obtener lote de anuncios
#' @description Recupera un lote de anuncios usando el token
#' @param start_index Índice de inicio para paginación
#' @param batch_size Tamaño del lote
#' @param token Token de seguridad
#' @param cookies Cookies para la petición
#' @param main_url URL de referencia
#' @param tipo Tipo de anuncio
#' @param start_date Fecha de inicio (opcional)
#' @param end_date Fecha de fin (opcional)
#' @param verbose Mostrar información detallada
#' @return DataFrame con los anuncios o NULL si hay error
#' @keywords internal
#' @importFrom httr status_code
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
  
  # Construir parámetros
  params <- build_request_params(start_index, batch_size, token, tipo, start_date, end_date)
  
  # Hacer la petición
  response <- make_api_request(url, params, cookies, main_url)
  
  if(is.null(response)) return(NULL)
  
  # Verificar respuesta
  status <- status_code(response)
  if(status != 200) {
    message("Request failed with status code: ", status)
    return(NULL)
  }
  
  # Obtener contenido de la respuesta
  content_text <- content(response, "text", encoding = "UTF-8")
  
  # Información de depuración
  if(verbose && nchar(content_text) < 50) {
    message("Warning: Short response received: ", content_text)
    return(NULL)
  }
  
  # Procesar respuesta JSON
  return(process_json_response(content_text, verbose))
}

# Funciones de descarga
#' @title Generar URL de descarga
#' @description Construye la URL de descarga para un archivo
#' @param file_name Nombre del archivo
#' @return URL completa de descarga
#' @keywords internal
generate_download_url <- function(file_name) {
  base_url <- "https://consultasenlinea.produce.gob.pe/produce/descarga/comunicados/dgsfs/"
  paste0(base_url, file_name)
}

#' @title Descargar archivo de anuncio
#' @description Descarga un archivo desde la URL proporcionada
#' @param url URL de descarga
#' @param file_name Nombre del archivo
#' @param download_dir Directorio de descarga
#' @param verbose Mostrar información detallada
#' @return TRUE si se descargó correctamente, FALSE en caso contrario
#' @keywords internal
#' @importFrom utils download.file
download_announcement_file <- function(url, file_name, download_dir, verbose = TRUE) {
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

# Función principal mejorada
#' @title Test PRODUCE API Connection
#' @description Tests the connection to the PRODUCE API by extracting the token and making a test request.
#' @param verbose Print debug information
#' @return List with success status and detailed information
#' @keywords internal
#' @importFrom httr status_code content
#' @importFrom jsonlite fromJSON
test_produce_api <- function(verbose = TRUE) {
  if(verbose) message("Testing connection to PRODUCE website...")
  
  # Verificar paquetes requeridos
  check_required_packages()
  
  # URL principal
  main_url <- "https://consultasenlinea.produce.gob.pe/ConsultasEnLinea/consultas.web/comunicados/suspensionPreventiva"
  
  tryCatch({
    # Obtener página principal
    main_response <- get_main_page(verbose)
    
    # Verificar estado de respuesta
    if(httr::status_code(main_response) != 200) {
      return(list(
        success = FALSE, 
        message = paste("Failed to connect to website. Status code:", httr::status_code(main_response))
      ))
    }
    
    # Extraer cookies
    cookies <- extract_cookies(main_response, verbose)
    
    # Extraer token
    html_content <- httr::content(main_response, "text", encoding = "UTF-8")
    token <- extract_token(html_content, verbose)
    
    # Realizar petición de prueba al API
    params <- build_request_params(0, 1, token)
    url <- "https://consultasenlinea.produce.gob.pe/ConsultasEnLinea/consultas.web/ajax/listado.comunicados.ajax.php"
    
    test_response <- make_api_request(url, params, cookies, main_url)
    
    # Verificar estado de respuesta
    if(httr::status_code(test_response) != 200) {
      return(list(
        success = FALSE, 
        message = paste("API request failed. Status code:", httr::status_code(test_response))
      ))
    }
    
    content_text <- httr::content(test_response, "text", encoding = "UTF-8")
    
    if(nchar(content_text) < 10) {
      return(list(
        success = FALSE, 
        message = paste("API returned too short response:", content_text)
      ))
    }
    
    # Intentar parsear JSON
    data <- jsonlite::fromJSON(content_text)
    
    # Verificar si obtuvimos datos
    if(!is.list(data) || !("aaData" %in% names(data))) {
      return(list(
        success = FALSE, 
        message = "API response doesn't contain expected data structure."
      ))
    }
    
    return(list(
      success = TRUE,
      message = paste("API test successful! Received", length(data$aaData), "records."),
      records = data$aaData,
      token = token,
      cookies = cookies
    ))
    
  }, error = function(e) {
    return(list(
      success = FALSE, 
      message = paste("Error during API test:", e$message)
    ))
  })
}



#' Get or detect columns in a data frame
#'
#' This helper function is used to either:
#' (1) detect columns from a data frame using pattern matching (if not explicitly provided), or
#' (2) validate that the specified columns exist in the data frame.
#'
#' It is particularly useful for cleaning up code in functions that require flexible
#' column naming or support automatic column detection (e.g., "fecha", "date", etc.).
#'
#' @param df A data frame where the columns will be detected or validated.
#' @param cols A character vector of column names. If NULL, the function tries to detect columns based on `patterns`.
#' @param patterns A regular expression pattern used to detect columns if `cols` is NULL.
#' @param col_name_friendly A user-friendly name for the column(s), used in error messages.
#'
#' @return A character vector with validated or automatically detected column names.
#' @keywords internal
get_or_detect_columns <- function(df, cols, patterns, col_name_friendly) {
  if (is.null(cols)) {
    idx <- grep(patterns, colnames(df), ignore.case = TRUE)
    cols <- if (length(idx) > 0) colnames(df)[idx] else NULL
    if (is.null(cols)) {
      stop(paste0("Could not automatically detect ", col_name_friendly, " columns. Please specify them explicitly."))
    }
  } else {
    missing <- cols[!cols %in% colnames(df)]
    if (length(missing) > 0) {
      stop(paste0("The following ", col_name_friendly, " columns do not exist in the data: ", paste(missing, collapse = ", ")))
    }
  }
  return(cols)
}
