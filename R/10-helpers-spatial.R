# Internal function: Convert to radians if needed
convert_to_radians <- function(x) {
  if (max(abs(x), na.rm = TRUE) > 2 * pi) x * pi / 180 else x
}

#' Calculate distance using Haversine formula
#'
#' @description
#' Internal function to calculate distances using the Haversine formula with WGS84 ellipsoid.
#' Implements the algorithm described in Sinnott (1984) for computing great circle distances.
#'
#' @param lon1 Origin longitude.
#' @param lat1 Origin latitude.
#' @param lon2 Destination longitude vector.
#' @param lat2 Destination latitude vector.
#' @param unit Output unit: "nm" or "km".
#'
#' @details
#' The Haversine formula calculates the shortest distance between two points on a sphere
#' given their latitude and longitude.
#'
#' @references
#' Sinnott, R.W. (1984). Virtues of the Haversine. Sky and Telescope, 68(2), 159.
#'
#' Veness, C. (2002-2022). Calculate distance, bearing and more between Latitude/Longitude points.
#' Movable Type Scripts. <https://www.movable-type.co.uk/scripts/latlong.html>
#'
#' @return List with minimum distance and corresponding index.
#' @keywords internal
calculate_haversine_distance_wgs84 <- function(lon1, lat1, lon2, lat2, unit) {
  lon1 <- convert_to_radians(lon1)
  lat1 <- convert_to_radians(lat1)
  lon2 <- convert_to_radians(lon2)
  lat2 <- convert_to_radians(lat2)

  R <- ifelse(unit == "nm", 3440, 6371)

  distances <- vapply(seq_along(lon2), function(i) {
    dlon <- lon2[i] - lon1
    dlat <- lat2[i] - lat1
    a_calc <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2[i]) * sin(dlon / 2)^2
    c <- 2 * atan2(sqrt(a_calc), sqrt(1 - a_calc))
    R * c
  }, numeric(1))

  min_idx <- which.min(distances)
  min_dist <- distances[min_idx]

  return(list(distance = min_dist, index = min_idx))
}

#' Calculate Manhattan distance between coordinates
#'
#' @description
#' Internal function to calculate Manhattan distance (L1 norm) between coordinates,
#' adapted for geographic coordinate systems.
#'
#' @param lon1 Origin longitude.
#' @param lat1 Origin latitude.
#' @param lon2 Destination longitude vector.
#' @param lat2 Destination latitude vector.
#' @param unit Distance unit: "nm" or "km".
#'
#' @details
#' The Manhattan distance calculates the sum of absolute differences between coordinates,
#' adapted for geographic coordinates using spherical projection approximations.
#' This implementation accounts for Earth's curvature by applying latitude corrections
#' to longitude differences.
#'
#' @note
#' This implementation adapts the standard Manhattan distance metric for geographic
#' coordinate systems by incorporating Earth's radius and latitude corrections.
#'
#' @return List with minimum distance and corresponding index.
#' @keywords internal
calculate_manhattan_distance <- function(lon1, lat1, lon2, lat2, unit) {

  lon1 <- convert_to_radians(lon1)
  lat1 <- convert_to_radians(lat1)
  lon2 <- convert_to_radians(lon2)
  lat2 <- convert_to_radians(lat2)

  R <- ifelse(unit == "nm", 3440, 6371)

  distances <- vapply(seq_along(lon2), function(i) {
    dx <- abs(lon2[i] - lon1) * cos((lat1 + lat2[i]) / 2) * R
    dy <- abs(lat2[i] - lat1) * R
    dx + dy
  }, numeric(1))

  min_idx <- which.min(distances)
  min_dist <- distances[min_idx]

  return(list(distance = min_dist, index = min_idx))
}

#' Calculate Euclidean distance
#'
#' @description
#' Internal function to calculate Euclidean distance between coordinates,
#' adapted for geographic coordinate systems.
#'
#' @param lon1 Origin longitude.
#' @param lat1 Origin latitude.
#' @param lon2 Destination longitude vector.
#' @param lat2 Destination latitude vector.
#' @param unit Distance unit.
#'
#' @details
#' The Euclidean distance calculates the straight-line distance between two points,
#' adapted for geographic coordinates using spherical projection approximations.
#' This implementation accounts for Earth's curvature by applying latitude corrections.
#'
#' @note
#' This implementation adapts the standard Euclidean distance metric for geographic
#' coordinate systems by incorporating Earth's radius and latitude corrections.
#'
#' @return List with minimum distance and corresponding index.
#' @keywords internal
calculate_euclidean_distance <- function(lon1, lat1, lon2, lat2, unit) {
  lon1 <- convert_to_radians(lon1)
  lat1 <- convert_to_radians(lat1)
  lon2 <- convert_to_radians(lon2)
  lat2 <- convert_to_radians(lat2)

  R <- ifelse(unit == "nm", 3440, 6371)

  distances <- vapply(seq_along(lon2), function(i) {
    dx <- (lon2[i] - lon1) * cos((lat1 + lat2[i]) / 2)
    dy <- lat2[i] - lat1
    sqrt(dx^2 + dy^2) * R
  }, numeric(1))

  min_idx <- which.min(distances)
  min_dist <- distances[min_idx]

  return(list(distance = min_dist, index = min_idx))
}


#' Calculate grid distance
#'
#' @description
#' Internal function to calculate distance with fixed grid resolution,
#' using coordinate discretization followed by Haversine distance calculation.
#'
#' @param lon1 Origin longitude.
#' @param lat1 Origin latitude.
#' @param lon2 Destination longitude vector.
#' @param lat2 Destination latitude vector.
#' @param resolution Grid resolution in degrees.
#' @param unit Distance unit.
#'
#' @details
#' This method rounds coordinates to the nearest grid point based on the specified resolution,
#' then calculates the Haversine distance between the origin and each grid-aligned destination point.
#' This approach is useful for applications requiring consistent spatial discretization.
#'
#' @note
#' This implementation combines coordinate discretization with the Haversine formula
#' for applications requiring grid-based spatial analysis.
#'
#' @return List with minimum distance and index.
#' @keywords internal
calculate_grid_distance <- function(lon1, lat1, lon2, lat2, resolution = 0.25, unit = "km") {
  lon1_grid <- round(lon1 / resolution) * resolution
  lat1_grid <- round(lat1 / resolution) * resolution

  distances <- vapply(seq_along(lon2), function(i) {
    lon2_grid <- round(lon2[i] / resolution) * resolution
    lat2_grid <- round(lat2[i] / resolution) * resolution

    lon1_rad <- convert_to_radians(lon1_grid)
    lat1_rad <- convert_to_radians(lat1_grid)
    lon2_rad <- convert_to_radians(lon2_grid)
    lat2_rad <- convert_to_radians(lat2_grid)

    R <- ifelse(unit == "nm", 3440, 6371)

    dlat <- lat2_rad - lat1_rad
    dlon <- lon2_rad - lon1_rad
    a <- sin(dlat / 2)^2 + cos(lat1_rad) * cos(lat2_rad) * sin(dlon / 2)^2
    c <- 2 * atan2(sqrt(a), sqrt(1 - a))
    R * c
  }, numeric(1))

  min_idx <- which.min(distances)
  min_dist <- distances[min_idx]

  return(list(distance = min_dist, index = min_idx))
}

#' Calculate vectorized distances to coastline
#'
#' @description
#' Internal function to calculate distances between multiple points and a coastline.
#' Supports different distance calculation methods and spatial filtering.
#'
#' @param lon_point Vector of point longitudes.
#' @param lat_point Vector of point latitudes.
#' @param coast_lon Vector of coastline longitudes.
#' @param coast_lat Vector of coastline latitudes.
#' @param distance_type Distance calculation method.
#' @param window Spatial filter window in degrees.
#' @param unit Distance unit.
#' @param resolution Grid resolution (only used if distance_type = "grid").
#'
#' @return List with distance vectors and corresponding indices.
#' @export
calculate_distances_vectorized <- function(lon_point, lat_point, coast_lon, coast_lat,
                                           distance_type, window, unit, resolution = 0.25) {
  n_points <- length(lon_point)
  final_distances <- numeric(n_points)
  final_indices <- integer(n_points)

  for (i in seq_len(n_points)) {
    if (window > 0) {
      filter_idx <- coast_lat >= (lat_point[i] - window) &
                    coast_lat <= (lat_point[i] + window)
      if (sum(filter_idx) < 2) filter_idx <- rep(TRUE, length(coast_lat))
      filtered_coast_lon <- coast_lon[filter_idx]
      filtered_coast_lat <- coast_lat[filter_idx]
    } else {
      filtered_coast_lon <- coast_lon
      filtered_coast_lat <- coast_lat
    }

    result <- switch(
      distance_type,
      "haversine" = calculate_haversine_distance_wgs84(
        lon_point[i], lat_point[i], filtered_coast_lon, filtered_coast_lat, unit
      ),
      "manhattan" = calculate_manhattan_distance(
        lon_point[i], lat_point[i], filtered_coast_lon, filtered_coast_lat, unit
      ),
      "euclidean" = calculate_euclidean_distance(
        lon_point[i], lat_point[i], filtered_coast_lon, filtered_coast_lat, unit
      ),
      "grid" = calculate_grid_distance(
        lon_point[i], lat_point[i], filtered_coast_lon, filtered_coast_lat,
        resolution = resolution, unit = unit
      ),
      stop("Invalid distance type: ", distance_type)
    )

    final_distances[i] <- result$distance

    if (window > 0) {
      original_indices <- which(filter_idx)
      final_indices[i] <- original_indices[result$index]
    } else {
      final_indices[i] <- result$index
    }
  }

  return(list(distances = final_distances, indices = final_indices))
}
