#' Convert latitude or longitude to decimal degrees
#'
#' @description
#' Converts coordinates expressed in degrees, minutes and seconds (DMS) or degrees and minutes (DM)
#' format to decimal degrees. By default, coordinates are assumed to be in the southern hemisphere
#' (negative latitudes). The function can automatically correct common errors such as minutes or
#' seconds greater than 60.
#'
#' @param coordinates Character vector. Each element should be in formats such as:
#'   "D M S", "D M", "17 26 S"
#' @param hemisphere Character indicating hemisphere when not specified in the coordinate.
#'   One of "N", "S", "E", "W" or "O". "S" and "W"/"O" generate negative values.
#' @param correct_errors Logical. If TRUE, automatically corrects out-of-range values.
#'
#' @return Numeric vector with coordinates converted to decimal degrees.
#'
#' @examples
#' dms_to_decimal(c("73 15 0"), hemisphere = "W")
#'
#' @export
#' @importFrom stringr str_split str_count str_detect str_extract
dms_to_decimal <- function(coordinates, hemisphere = "S", correct_errors = TRUE) {
  if (missing(coordinates)) {
    stop("The 'coordinates' parameter is required.")
  }

  if (length(coordinates) == 0) {
    warning("The coordinate vector is empty.")
    return(numeric(0))
  }

  if (is.factor(coordinates)) {
    coordinates <- as.character(coordinates)
    warning("The coordinate vector has been converted from factor to character.")
  }

  if (!is.character(coordinates)) {
    if (is.numeric(coordinates)) {
      warning("The provided coordinates are already numeric. They are returned unchanged.")
      return(coordinates)
    } else {
      stop("Coordinates must be a text string or a character vector.")
    }
  }

  if (!hemisphere %in% c("N", "S", "E", "W", "O")) {
    stop("The hemisphere must be one of: 'N', 'S', 'E', 'W' or 'O'.")
  }

  results <- vapply(coordinates, function(coord) {
    if (is.na(coord) || coord == "") {
      return(NA_real_)
    }

    tryCatch({
      local_hemisphere <- hemisphere
      original_coord <- coord

      # Replace common DMS symbols with spaces
      coord <- gsub("\u00B0|\u2032|\u2033", " ", coord)  # ° ′ ″ → space
      coord <- iconv(coord, from = "UTF-8", to = "ASCII//TRANSLIT")

      hemisphere_pattern <- "[NSEW]|O"
      found_hemisphere <- regmatches(coord, regexpr(hemisphere_pattern, coord))

      if (length(found_hemisphere) > 0 && found_hemisphere != "") {
        local_hemisphere <- found_hemisphere
        coord <- gsub(hemisphere_pattern, "", coord)
      }

      sign <- ifelse(local_hemisphere %in% c("S", "W", "O"), -1, 1)

      clean_coord <- gsub("[?'\"]", " ", coord)
      clean_coord <- gsub("\\s+", " ", clean_coord)
      clean_coord <- trimws(clean_coord)

      components <- unlist(strsplit(clean_coord, " "))
      num_components <- components[grepl("^[0-9]+(\\.[0-9]+)?$", components)]

      if (length(num_components) == 0) {
        warning(paste("No numeric components found in the coordinate:", original_coord))
        return(NA_real_)
      }

      parts <- safe_numeric_conversion(num_components)

      if (any(is.na(parts))) {
        warning(paste("Error converting some component of the coordinate to numeric:", original_coord))
        return(NA_real_)
      }

      if (correct_errors && length(parts) >= 3) {
        if (!is.na(parts[3]) && parts[3] >= 60) {
          extra_minutes <- floor(parts[3] / 60)
          parts[3] <- parts[3] %% 60
          parts[2] <- parts[2] + extra_minutes
        }

        if (!is.na(parts[2]) && parts[2] >= 60) {
          extra_degrees <- floor(parts[2] / 60)
          parts[2] <- parts[2] %% 60
          parts[1] <- parts[1] + extra_degrees
        }
      } else {
        if (length(parts) >= 1 && (is.na(parts[1]) || parts[1] < 0 || parts[1] > 180)) {
          warning(paste("Degrees out of range (0–180) in the coordinate:", original_coord))
        }
        if (length(parts) >= 2 && (is.na(parts[2]) || parts[2] < 0 || parts[2] >= 60)) {
          warning(paste("Minutes out of range (0–59) in the coordinate:", original_coord))
        }
        if (length(parts) >= 3 && (is.na(parts[3]) || parts[3] < 0 || parts[3] >= 60)) {
          warning(paste("Seconds out of range (0–59) in the coordinate:", original_coord))
        }
      }

      if (length(parts) == 3) {
        decimal <- sign * (parts[1] + parts[2] / 60 + parts[3] / 3600)
      } else if (length(parts) == 2) {
        decimal <- sign * (parts[1] + parts[2] / 60)
      } else if (length(parts) == 1) {
        decimal <- sign * parts[1]
      } else {
        warning(paste("Unrecognized format for coordinate:", original_coord))
        return(NA_real_)
      }

      if (abs(decimal) > 180) {
        warning(paste("The calculated decimal coordinate is out of range (-180 to 180):", decimal))
      }

      return(decimal)
    }, error = function(e) {
      warning(paste("Error processing coordinate", coord, ":", e$message))
      return(NA_real_)
    })
  }, FUN.VALUE = numeric(1))

  names(results) <- NULL
  return(results)
}
#

#' Vectorized distance to coast
#'
#' @description
#' Estimates the distance between a set of points (lon, lat) and a coastline defined by coordinates.
#' Can be executed sequentially or in parallel, and also return the indices of the nearest coastal points.
#'
#' @param lon Numeric vector with the longitudes of the points of interest.
#' @param lat Numeric vector with the latitudes of the points of interest.
#' @param coastline Data frame with coastline coordinates. Must have columns named `Long` and `Lat`.
#'   If `NULL`, uses internal dataset `peru_coastline`.
#' @param return_indices Logical. If `TRUE`, also returns the indices of the nearest coastline points.
#' @param distance_type Type of geographic distance to use: "haversine", "euclidean", "grid".
#' @param unit Unit of measurement for distance: "nm" (nautical miles), "km", etc.
#' @param window Search window in degrees around the point to limit calculations and improve efficiency.
#' @param parallel Logical. If `TRUE`, performs the calculation in parallel using multiple cores.
#' @param cores Number of cores to use for parallel processing.
#'
#' @return If `return_indices = FALSE`, returns a numeric vector with distances to the coast for each point.
#'   If `return_indices = TRUE`, returns a list with distance and index components.
#'
#' @examples
#' \dontrun{
#' data_hauls <- process_hauls(data_hauls = calas_bitacora)
#' distances <- coast_distance(
#'   lon = data_hauls$lon_final,
#'   lat = data_hauls$lat_final,
#'   distance_type = "haversine",
#'   unit = "nm",
#'   parallel = TRUE,
#'   cores = 2
#' )
#' }
#'
#' @export
#' @importFrom future plan multisession
#' @importFrom future.apply future_lapply
coast_distance <- function(lon,
                            lat,
                            coastline = NULL,
                            return_indices = FALSE,
                            distance_type = "haversine",
                            unit = "nm",
                            window = 1,
                            parallel = FALSE,
                            cores = 4) {
  if (missing(lon) || missing(lat)) {
    stop("The parameters 'lon', 'lat', and 'coastline' are required.")
  }

  if (is.null(coastline)) {
    coastline <- peru_coastline
  }

  if (!is.numeric(lon)) stop("The 'lon' parameter must be numeric.")
  if (!is.numeric(lat)) stop("The 'lat' parameter must be numeric.")
  if (!is.data.frame(coastline)) stop("The 'coastline' parameter must be a data.frame.")
  if (!is.logical(return_indices)) stop("The 'return_indices' parameter must be logical (TRUE/FALSE).")
  if (!is.character(distance_type)) stop("The 'distance_type' parameter must be text.")
  if (!is.character(unit)) stop("The 'unit' parameter must be text.")
  if (!is.numeric(window)) stop("The 'window' parameter must be numeric.")
  if (!is.logical(parallel)) stop("The 'parallel' parameter must be logical (TRUE/FALSE).")
  if (!is.numeric(cores) || cores < 1) stop("The 'cores' parameter must be a positive integer.")

  if (length(lon) != length(lat)) {
    stop("The 'lon' and 'lat' vectors must have the same length.")
  }

  if (any(abs(lon) > 180, na.rm = TRUE)) {
    warning("Longitude values outside the valid range (-180 to 180) were detected.")
  }
  if (any(abs(lat) > 90, na.rm = TRUE)) {
    warning("Latitude values outside the valid range (-90 to 90) were detected.")
  }

  valid_types <- c("haversine", "euclidean", "manhattan", "grid")
  if (!distance_type %in% valid_types) {
    stop("The 'distance_type' parameter must be one of: ", paste(valid_types, collapse = ", "))
  }

  known_units <- c("nm", "km")
  if (!unit %in% known_units) {
    stop("The unit '", unit, "' is not one of the common units: ", paste(known_units, collapse = ", "))
  }

  if (!all(c("Long", "Lat") %in% colnames(coastline))) {
    stop("coastline must contain columns 'Long' and 'Lat'")
  }

  if (nrow(coastline) == 0) {
    stop("coastline is empty")
  }

  if (!is.numeric(coastline$Long) || !is.numeric(coastline$Lat)) {
    stop("The 'Long' and 'Lat' columns of coastline must be numeric")
  }

  valid <- !is.na(lon) & !is.na(lat)
  if (sum(valid) == 0) {
    warning("All input points contain NA values")
    return(rep(NA, length(valid)))
  }

  valid_lon <- lon[valid]
  valid_lat <- lat[valid]

  n_points <- length(valid_lon)
  cores <- min(cores, n_points)
  batch_size <- ceiling(n_points / cores)
  batch_indices <- split(seq_len(n_points), ceiling(seq_len(n_points) / batch_size))

  if (parallel) {
    if (!requireNamespace("future.apply", quietly = TRUE)) {
      warning("Package 'future.apply' not found. Using sequential processing.")
      parallel <- FALSE
    } else {
      future::plan(future::multisession, workers = cores)
      on.exit(future::plan(future::sequential), add = TRUE)
    }
  }

  apply_fun <- if (parallel) future.apply::future_lapply else lapply

  batch_results <- tryCatch({
    apply_fun(batch_indices, function(batch_indices) {
      calculate_distances_vectorized(
        lon_point = valid_lon[batch_indices],
        lat_point = valid_lat[batch_indices],
        coast_lon = coastline$Long,
        coast_lat = coastline$Lat,
        distance_type = distance_type,
        window = window,
        unit = unit
      )
    })
  }, error = function(e) {
    stop("Error in distance calculation: ", e$message)
  })

  tryCatch({
    distances <- unlist(lapply(batch_results, `[[`, "distances"))
    indices <- unlist(lapply(batch_results, `[[`, "indices"))

    if (length(distances) != sum(valid)) {
      warning("The number of calculated distances does not match the number of valid points.")
    }

    final_result <- rep(NA_real_, length(valid))
    final_result[valid] <- distances

    if (return_indices) {
      final_indices <- rep(NA_integer_, length(valid))
      final_indices[valid] <- indices
      return(list(distance = final_result, index = final_indices))
    } else {
      return(final_result)
    }
  }, error = function(e) {
    warning("Error processing results: ", e$message)
    return(rep(NA_real_, length(valid)))
  })
}

#' Points on land
#'
#' @description
#' Classifies a set of geographic coordinates (longitude and latitude) as "land" or "sea"
#' according to their relative position to a coastline. A point is considered to be on land
#' if its longitude is greater than that of its nearest point on the coastline.
#'
#' @param x_point Numeric vector of longitudes (in decimal degrees).
#' @param y_point Numeric vector of latitudes (in decimal degrees).
#' @param coastline Data frame with coastline coordinates. Must have columns named `Long` and `Lat`.
#'   If `NULL`, uses internal dataset `peru_coastline`.
#' @param parallel Logical. If `TRUE`, performs the calculation in parallel using multiple cores.
#' @param cores Number of cores to use for parallel processing.
#' @param distance_type Type of geodesic distance to use in the calculation.
#' @param window Geographic window in degrees to reduce the number of coastline points to consider.
#' @param unit Unit of measurement for distance: "km" or "nm".
#'
#' @return Text vector of the same length as `x_point`, indicating whether each point
#'   is on "land" or "sea". NA values are maintained as NA.
#'
#' @examples
#' \dontrun{
#' data_hauls <- process_hauls(data_hauls = calas_bitacora)
#' result <- land_points(
#'   x_point = data_hauls$lon_final,
#'   y_point = data_hauls$lat_final
#' )
#' table(result)
#' }
#'
#' @export
land_points <- function(x_point,
                          y_point,
                          coastline = NULL,
                          parallel = FALSE,
                          cores = 4,
                          distance_type = "haversine",
                          window = 0.5,
                          unit = "nm") {
  if (!is.numeric(x_point) || !is.numeric(y_point)) {
    stop("`x_point` and `y_point` must be numeric vectors.")
  }

  if (length(x_point) != length(y_point)) {
    stop("`x_point` and `y_point` must have the same length.")
  }

  if (is.null(coastline)) {
    coastline <- peru_coastline
  }

  if (!is.data.frame(coastline)) {
    stop("`coastline` must be a data.frame.")
  }

  if (!all(c("Long", "Lat") %in% names(coastline))) {
    stop("`coastline` must contain columns named 'Long' and 'Lat'.")
  }

  if (!is.logical(parallel)) {
    stop("`parallel` must be TRUE or FALSE.")
  }

  if (!is.numeric(cores) || cores < 1) {
    stop("`cores` must be a positive integer.")
  }

  if (!distance_type %in% c("haversine", "manhattan", "grid")) {
    stop("`distance_type` must be one of: 'haversine', 'manhattan', 'grid'.")
  }

  if (!is.numeric(window) || window < 0) {
    stop("`window` must be a non-negative number.")
  }

  if (!unit %in% c("km", "nm")) {
    stop("`unit` must be 'km' or 'nm'.")
  }

  x_point <- safe_numeric_conversion(x_point)
  y_point <- safe_numeric_conversion(y_point)
  coastline$Long <- safe_numeric_conversion(coastline$Long)
  coastline$Lat <- safe_numeric_conversion(coastline$Lat)

  n <- length(x_point)

  process_point <- function(i) {
    if (is.na(x_point[i]) || is.na(y_point[i])) return(NA_character_)

    result <- calculate_distances_vectorized(
      lon_point = x_point[i],
      lat_point = y_point[i],
      coast_lon = coastline$Long,
      coast_lat = coastline$Lat,
      distance_type = distance_type,
      window = window,
      unit = unit
    )

    coast_idx <- result$indices[1]

    if (coast_idx >= 2 && coast_idx < nrow(coastline)) {
      p1 <- c(coastline$Long[coast_idx - 1], coastline$Lat[coast_idx - 1])
      p2 <- c(coastline$Long[coast_idx + 1], coastline$Lat[coast_idx + 1])
      p  <- c(x_point[i], y_point[i])

      direction <- (p2[1] - p1[1]) * (p[2] - p1[2]) - (p2[2] - p1[2]) * (p[1] - p1[1])

      if (direction > 0) return("sea") else return("land")
    }

    return("unknown")
  }

  if (parallel) {
    future::plan(future::multisession, workers = cores)
    result <- future.apply::future_lapply(seq_len(n), process_point)
    future::plan(future::sequential)
  } else {
    result <- lapply(seq_len(n), process_point)
  }

  return(unlist(result))
}
