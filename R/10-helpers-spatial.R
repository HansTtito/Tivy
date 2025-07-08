#' Calculate distance using Haversine formula
#'
#' @description
#' Internal function to calculate distances using the Haversine formula with WGS84 ellipsoid.
#' Used by spatial analysis functions.
#'
#' @param lon1 Origin longitude.
#' @param lat1 Origin latitude.
#' @param lon2 Destination longitude vector.
#' @param lat2 Destination latitude vector.
#' @param unit Output unit: "nm" or "km".
#'
#' @return List with minimum distance and corresponding index.
#' @keywords internal
calculate_haversine_distance_wgs84 <- function(lon1, lat1, lon2, lat2, unit) {
  
  if (max(abs(lon1), abs(lat1), abs(lon2), abs(lat2), na.rm = TRUE) > 2*pi) {
    lon1 <- lon1 * pi/180
    lat1 <- lat1 * pi/180
    lon2 <- lon2 * pi/180
    lat2 <- lat2 * pi/180
  }
  
  a <- 6378137
  b <- 6356752.3142
  
  factor <- ifelse(unit == "nm", 0.000539957, 0.001)
  
  R <- sqrt(((a^2 * cos(lat1))^2 + (b^2 * sin(lat1))^2) /
              ((a * cos(lat1))^2 + (b * sin(lat1))^2))
  
  distances <- vapply(seq_along(lon2), function(i) {
    dlon <- lon2[i] - lon1
    dlat <- lat2[i] - lat1
    a_calc <- sin(dlat/2)^2 + cos(lat1) * cos(lat2[i]) * sin(dlon/2)^2
    c <- 2 * atan2(sqrt(a_calc), sqrt(1-a_calc))
    R * c * factor
  }, numeric(1))
  
  min_idx <- which.min(distances)
  min_dist <- distances[min_idx]
  
  return(list(distance = min_dist, index = min_idx))
}

#' Calculate Manhattan distance between coordinates
#'
#' @description
#' Internal function to calculate Manhattan distance (city blocks) between coordinates.
#'
#' @param lon1 Origin longitude.
#' @param lat1 Origin latitude.
#' @param lon2 Destination longitude vector.
#' @param lat2 Destination latitude vector.
#' @param unit Distance unit: "nm" or "km".
#'
#' @return List with minimum distance and corresponding index.
#' @keywords internal
calculate_manhattan_distance <- function(lon1, lat1, lon2, lat2, unit) {
  if (max(abs(lon1), abs(lat1), abs(lon2), abs(lat2)) > 2*pi) {
    lon1 <- lon1 * pi/180
    lat1 <- lat1 * pi/180
    lon2 <- lon2 * pi/180
    lat2 <- lat2 * pi/180
  }

  R <- ifelse(unit == "nm", 3440, 6371)

  distances <- vapply(seq_along(lon2), function(i) {
    dx <- abs(lon2[i] - lon1) * cos((lat1 + lat2[i])/2) * R
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
#' Internal function to calculate Euclidean distance between coordinates.
#'
#' @param lon1 Origin longitude.
#' @param lat1 Origin latitude.
#' @param lon2 Destination longitude vector.
#' @param lat2 Destination latitude vector.
#' @param unit Distance unit.
#'
#' @return List with minimum distance and index.
#' @keywords internal
calculate_euclidean_distance <- function(lon1, lat1, lon2, lat2, unit) {
  
  if (max(abs(lon1), abs(lat1), abs(lon2), abs(lat2), na.rm = TRUE) > 2*pi) {
    lon1 <- lon1 * pi/180
    lat1 <- lat1 * pi/180
    lon2 <- lon2 * pi/180
    lat2 <- lat2 * pi/180
  }
  
  R <- ifelse(unit == "nm", 3440, 6371)
  
  distances <- vapply(seq_along(lon2), function(i) {
    dx <- abs(lon2[i] - lon1) * cos((lat1 + lat2[i])/2) * R
    dy <- abs(lat2[i] - lat1) * R
    dx + dy
  }, numeric(1))
  
  min_idx <- which.min(distances)
  min_dist <- distances[min_idx]
  
  return(list(distance = min_dist, index = min_idx))
}

#' Calculate grid distance
#'
#' @description
#' Internal function to calculate distance with fixed grid resolution.
#'
#' @param lon1 Origin longitude.
#' @param lat1 Origin latitude.
#' @param lon2 Destination longitude vector.
#' @param lat2 Destination latitude vector.
#' @param resolution Grid resolution in degrees.
#' @param unit Distance unit.
#'
#' @return List with minimum distance and index.
#' @keywords internal
calculate_grid_distance <- function(lon1, lat1, lon2, lat2, resolution = 0.25, unit = "km") {
  
  lon1_grid <- round(lon1 / resolution) * resolution
  lat1_grid <- round(lat1 / resolution) * resolution
  
  distances <- vapply(seq_along(lon2), function(i) {
    lon2_grid <- round(lon2[i] / resolution) * resolution
    lat2_grid <- round(lat2[i] / resolution) * resolution
    
    if (max(abs(lon1_grid), abs(lat1_grid), abs(lon2_grid), abs(lat2_grid)) > 2*pi) {
      lon1_grid <- lon1_grid * pi/180
      lat1_grid <- lat1_grid * pi/180
      lon2_grid <- lon2_grid * pi/180
      lat2_grid <- lat2_grid * pi/180
    }
    
    R <- ifelse(unit == "nm", 3440, 6371)
    
    dlat <- lat2_grid - lat1_grid
    dlon <- lon2_grid - lon1_grid
    a <- sin(dlat/2)^2 + cos(lat1_grid) * cos(lat2_grid) * sin(dlon/2)^2
    c <- 2 * atan2(sqrt(a), sqrt(1-a))
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
#'
#' @return List with distance vectors and corresponding indices.
#' @export
calculate_distances_vectorized <- function(lon_point, lat_point, coast_lon, coast_lat, 
                                           distance_type, window, unit) {
  
  n_points <- length(lon_point)
  final_distances <- numeric(n_points)
  final_indices <- integer(n_points)
  
  for (i in seq_len(n_points)) {
    if (window > 0) {
      filter_idx <- coast_lat >= (lat_point[i] - window) & 
                    coast_lat <= (lat_point[i] + window)
      
      if (sum(filter_idx) < 2) {
        filter_idx <- rep(TRUE, length(coast_lat))
      }
      
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
      "euclidean" = calculate_euclidean_distance(
        lon_point[i], lat_point[i], filtered_coast_lon, filtered_coast_lat, unit
      ),
      "grid" = calculate_grid_distance(
        lon_point[i], lat_point[i], filtered_coast_lon, filtered_coast_lat, unit = unit
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