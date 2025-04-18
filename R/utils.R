#' Calculate distance using the Haversine formula with the WGS84 ellipsoid
#'
#' @param lon1 Longitude of the origin point (in degrees or radians).
#' @param lat1 Latitude of the origin point (in degrees or radians).
#' @param lon2 Vector of destination longitudes (in degrees or radians).
#' @param lat2 Vector of destination latitudes (in degrees or radians).
#' @param unit Output unit: "nm" for nautical miles, "km" for kilometers.
#'
#' @return List with the minimum distance and the corresponding index.
#' @keywords internal
calculate_haversine_distance_wgs84 <- function(lon1, lat1, lon2, lat2, unit) {
  # Convert degrees to radians (checking if they're already in radians)
  if (max(abs(lon1), abs(lat1), abs(lon2), abs(lat2)) > 2*pi) {
    lon1 <- lon1 * pi/180
    lat1 <- lat1 * pi/180
    lon2 <- lon2 * pi/180
    lat2 <- lat2 * pi/180
  }

  # Earth's equatorial and polar radius according to WGS84
  a <- 6378137  # Equatorial radius in meters
  b <- 6356752.3142  # Polar radius in meters

  # Conversion factor according to unit
  factor <- ifelse(unit == "nm", 0.000539957, 0.001)  # to nautical miles or km

  # Mean radius at the given latitude
  R <- sqrt(((a^2 * cos(lat1))^2 + (b^2 * sin(lat1))^2) /
              ((a * cos(lat1))^2 + (b * sin(lat1))^2))

  # Vectorize distance calculations (without for loop)
  distances <- vapply(seq_along(lon2), function(i) {
    dlon <- lon2[i] - lon1
    dlat <- lat2[i] - lat1
    a_calc <- sin(dlat/2)^2 + cos(lat1) * cos(lat2[i]) * sin(dlon/2)^2
    c <- 2 * atan2(sqrt(a_calc), sqrt(1-a_calc))
    R * c * factor
  }, numeric(1))

  # Find the minimum and its index
  min_idx <- which.min(distances)
  min_dist <- distances[min_idx]

  # Return both the minimum distance and its index
  return(list(distance = min_dist, index = min_idx))
}



#' Calculate Manhattan distance (city blocks) between coordinates
#'
#' @param lon1 Longitude of the origin point.
#' @param lat1 Latitude of the origin point.
#' @param lon2 Vector of destination longitudes.
#' @param lat2 Vector of destination latitudes.
#' @param unit Distance unit: "nm" or "km".
#'
#' @return List with the minimum distance and the corresponding index.
#' @keywords internal
calculate_manhattan_distance <- function(lon1, lat1, lon2, lat2, unit) {
  # Check if conversion to radians is needed
  if (max(abs(lon1), abs(lat1), abs(lon2), abs(lat2)) > 2*pi) {
    lon1 <- lon1 * pi/180
    lat1 <- lat1 * pi/180
    lon2 <- lon2 * pi/180
    lat2 <- lat2 * pi/180
  }

  R <- ifelse(unit == "nm", 3440, 6371)  # Nautical miles or kilometers

  # Vectorize calculation
  distances <- vapply(seq_along(lon2), function(i) {
    # Calculate distance in x direction (longitude)
    dx <- abs(lon2[i] - lon1) * cos((lat1 + lat2[i])/2) * R
    # Calculate distance in y direction (latitude)
    dy <- abs(lat2[i] - lat1) * R
    dx + dy
  }, numeric(1))

  # Find the minimum and its index
  min_idx <- which.min(distances)
  min_dist <- distances[min_idx]

  return(list(distance = min_dist, index = min_idx))
}



#' Calculate grid distance with fixed resolution
#'
#' @param lon1 Longitude of the base point.
#' @param lat1 Latitude of the base point.
#' @param lon2 Vector of destination longitudes.
#' @param lat2 Vector of destination latitudes.
#' @param resolution Grid resolution (default 0.25 degrees).
#' @param unit Distance unit: "nm" or "km".
#'
#' @return List with minimum distance and its index.
#' @keywords internal
calculate_grid_distance <- function(lon1, lat1, lon2, lat2, resolution = 0.25, unit = "km") {
  # Round to the nearest grid resolution
  lon1_grid <- round(lon1 / resolution) * resolution
  lat1_grid <- round(lat1 / resolution) * resolution

  # Vectorize calculation
  distances <- vapply(seq_along(lon2), function(i) {
    lon2_grid <- round(lon2[i] / resolution) * resolution
    lat2_grid <- round(lat2[i] / resolution) * resolution

    # Convert to radians if necessary
    if (max(abs(lon1_grid), abs(lat1_grid), abs(lon2_grid), abs(lat2_grid)) > 2*pi) {
      lon1_grid <- lon1_grid * pi/180
      lat1_grid <- lat1_grid * pi/180
      lon2_grid <- lon2_grid * pi/180
      lat2_grid <- lat2_grid * pi/180
    }

    R <- ifelse(unit == "nm", 3440, 6371)  # Nautical miles or kilometers

    dlat <- lat2_grid - lat1_grid
    dlon <- lon2_grid - lon1_grid
    a <- sin(dlat/2)^2 + cos(lat1_grid) * cos(lat2_grid) * sin(dlon/2)^2
    c <- 2 * atan2(sqrt(a), sqrt(1-a))
    R * c
  }, numeric(1))

  # Find the minimum and its index
  min_idx <- which.min(distances)
  min_dist <- distances[min_idx]

  return(list(distance = min_dist, index = min_idx))
}



#' Calculate distances between multiple points and a coastline (vectorized)
#'
#' @param lon_point Vector of longitudes of points to compare.
#' @param lat_point Vector of latitudes of points to compare.
#' @param coast_lon Vector of coastline longitudes.
#' @param coast_lat Vector of coastline latitudes.
#' @param distance_type Type of distance: "haversine", "manhattan" or "grid".
#' @param window Window size (in degrees) to filter the coast. If 0, the entire coast is used.
#' @param unit Distance unit: "nm" or "km".
#'
#' @return List with vectors of minimum distances and corresponding indices.
#' @keywords internal
calculate_distances_vectorized <- function(lon_point, lat_point, coast_lon, coast_lat, distance_type, window, unit) {
  n_points <- length(lon_point)
  final_distances <- numeric(n_points)
  final_indices <- integer(n_points)

  # Processing by point, but with internal vectorized calculations
  for (i in seq_len(n_points)) {
    # Spatial filtering by geographic window (optional but efficient)
    if (window > 0) {
      filter_idx <-
        coast_lat >= (lat_point[i] - window) &
        coast_lat <= (lat_point[i] + window)

      # If there are no points in the window, use the entire coast
      if (sum(filter_idx) < 2) {
        filter_idx <- rep(TRUE, length(coast_lat))
      }

      filtered_coast_lon <- coast_lon[filter_idx]
      filtered_coast_lat <- coast_lat[filter_idx]
    } else {
      # No spatial filtering
      filtered_coast_lon <- coast_lon
      filtered_coast_lat <- coast_lat
    }

    # Vectorized calculation according to the chosen method
    result <- switch(
      distance_type,
      "haversine" = calculate_haversine_distance_wgs84(
        lon_point[i], lat_point[i], filtered_coast_lon, filtered_coast_lat, unit
      ),
      "manhattan" = calculate_manhattan_distance(
        lon_point[i], lat_point[i], filtered_coast_lon, filtered_coast_lat, unit
      ),
      "grid" = calculate_grid_distance(
        lon_point[i], lat_point[i], filtered_coast_lon, filtered_coast_lat, unit
      ),
      stop("Invalid distance type")
    )

    # Store results
    final_distances[i] <- result$distance

    # Convert local index to global if filtering was used
    if (window > 0) {
      original_indices <- which(filter_idx)
      final_indices[i] <- original_indices[result$index]
    } else {
      final_indices[i] <- result$index
    }
  }

  return(list(distances = final_distances, indices = final_indices))
}


#' Calculate the approximate longitude of the coast at a specific latitude
#'
#' This function finds the approximate longitude of a coastline at a given latitude,
#' by looking for nearby coastal points and calculating their average longitude.
#'
#' @param coastline A dataframe containing coastline data with at least two columns:
#'        'Lat' (latitude) and 'Long' (longitude) in decimal degrees.
#' @param latitude Numeric value representing the latitude in decimal degrees for which
#'        the coast longitude is to be found.
#'
#' @return A numeric value representing the average longitude in decimal degrees of the
#'        coastal points near the specified latitude. If no nearby points are found,
#'        returns -75 as an approximation for the Peruvian coast and displays a warning.
#'
#' @details
#' The function looks for coastal points within a range of 0.1 degrees of the specified
#' latitude. This threshold may need adjustments depending on the density of points in the dataset.
#'
#' @note
#' - Requires that the input dataframe has the correct structure with columns 'Lat' and 'Long'.
#' - The default value of -75 for the Peruvian coast is only a general approximation and
#'   may not be suitable for other regions.
#' - This function does not perform interpolations and may give inaccurate results on coasts with
#'   very irregular shapes or deep bays.
#' - Works best when coastal data has a uniform distribution of points.
#' @keywords internal
calculate_coast_longitude <- function(coastline, latitude) {
  # Check that the coastline dataframe contains the necessary columns
  if (!all(c("Lat", "Long") %in% names(coastline))) {
    stop("The 'coastline' dataframe must contain the columns 'Lat' and 'Long'")
  }

  # Check that latitude is a numeric value
  if (!is.numeric(latitude)) {
    stop("The 'latitude' parameter must be a numeric value")
  }

  # Find the coastal points closest to the given latitude
  nearby_idx <- which(abs(coastline$Lat - latitude) < 0.1)
  if (length(nearby_idx) == 0) {
    warning("No coastal points found near latitude ", latitude, ". Using approximation.")
    return(-75)  # Approximate value for the Peruvian coast
  }
  # Calculate the average longitude of these points
  coast_longitude <- mean(coastline$Long[nearby_idx])
  return(coast_longitude)
}


#' Generate static plot of polygons on a map
#'
#' @description
#' Creates a static plot using `ggplot2` that displays geographic polygons on a coastline base.
#'
#' @param polygons List of polygons. Each polygon must contain a `coords` matrix with longitude and latitude columns.
#' @param coastline Data frame with the coastline, with columns `Long` and `Lat`.
#' @param title Main title of the plot.
#' @param colors Vector of colors to fill the polygons. If `NULL`, colors are automatically assigned.
#' @param show_legend Logical. If `TRUE`, the legend is displayed.
#' @param name_legend Name of the legend (optional). If NULL, legend has no title. Default is "Comunicados".
#' @param labels Vector of labels for the polygons (optional).
#' @param add_grid Logical. If `TRUE`, adds a geographic grid to the plot.
#' @param theme `ggplot2` theme to use. Default, `theme_minimal()`.
#'
#' @return A `ggplot` object ready to be plotted.
#' @importFrom RColorBrewer brewer.pal
#' @keywords internal
plot_static <- function(polygons, coastline, title, colors, show_legend = TRUE, name_legend = NULL,
                          labels = NULL, add_grid = TRUE, theme = ggplot2::theme_minimal()) {

  p <- ggplot2::ggplot()

  # Coastline
  p <- p + ggplot2::geom_path(data = coastline, ggplot2::aes(x = .data[["Long"]], y = .data[["Lat"]]),
                              color = "darkgrey", linewidth = 0.5)

  if(is.null(colors)){
    colors <- RColorBrewer::brewer.pal(n = length(polygons), name = "Set3")
  }

  # Build data.frame for all polygons together
  df_list <- list()
  for (i in seq_along(polygons)) {
    polygon <- polygons[[i]]
    color_idx <- (i - 1) %% length(colors) + 1

    label <- if (!is.null(labels) && length(labels) >= i) {
      labels[i]
    } else {
      polygon$announcement
    }

    coords_df <- as.data.frame(polygon$coords)
    colnames(coords_df) <- c("Long", "Lat")
    coords_df$group <- label

    coords_df$id <- polygon$id

    df_list[[i]] <- coords_df
  }

  df_polygons <- do.call(rbind, df_list)

  # Polygons with fill by announcement
  p <- p + ggplot2::geom_polygon(
    data = df_polygons,
    ggplot2::aes(x = .data[["Long"]], y = .data[["Lat"]], fill = .data[["group"]], group = .data[["id"]]),
    alpha = 0.7,
    color = "black"
  )

  # Colors
  if (!is.null(name_legend)) {
    p <- p + ggplot2::scale_fill_manual(values = colors, name = name_legend)
  } else {
    p <- p + ggplot2::scale_fill_manual(values = colors, guide = ggplot2::guide_legend(title = NULL))
  }

  # Labels and theme
  p <- p + ggplot2::labs(
    title = title,
    x = "Longitude",
    y = "Latitude"
  ) + theme

  # Grid
  if (add_grid) {
    p <- p + ggplot2::coord_quickmap() +
      ggplot2::scale_x_continuous(breaks = seq(-82, -70, by = 2)) +
      ggplot2::scale_y_continuous(breaks = seq(-20, -8, by = 2)) +
      ggplot2::theme(panel.grid = ggplot2::element_line(color = "lightgrey", linetype = "dashed"))
  } else {
    p <- p + ggplot2::coord_quickmap()
  }

  # Legend
  if (!show_legend) {
    p <- p + ggplot2::theme(legend.position = "none")
  } else {
    p <- p + ggplot2::theme(
      legend.position = "right",
      legend.background = ggplot2::element_rect(fill = "white", color = "grey80"),
      legend.margin = ggplot2::margin(5, 5, 5, 5)
    )
  }

  return(p)
}



#' Generate interactive plot of polygons with leaflet
#'
#' @description
#' Creates an interactive map using `leaflet`, showing polygons with popup information.
#'
#' @param polygons List of polygons. Each must have fields such as `coords`, `announcement`, dates and coordinates.
#' @param coastline Data frame with the coastline (columns `Long` and `Lat`).
#' @param title Title to display at the top of the map.
#' @param colors Vector of colors. If `NULL`, they are automatically assigned with `RColorBrewer::brewer.pal`.
#' @param show_legend Logical. If `TRUE`, the layers control (legend) is displayed.
#' @param labels Optional vector of names to display in the legend and map labels.
#' @param base_layers Logical. If `TRUE`, includes base layers such as satellite and ocean maps.
#' @param minimap Logical. If `TRUE`, displays a minimap in the lower right corner.
#'
#' @return A `leaflet` object with the interactive map.
#' @importFrom RColorBrewer brewer.pal
#'
#' @keywords internal
plot_interactive <- function(polygons, coastline, title, colors, show_legend = TRUE,
                               labels = NULL, base_layers = TRUE, minimap = TRUE) {

  if(is.null(colors)){
    colors <- RColorBrewer::brewer.pal(n = length(polygons), name = "Set3")
  }

  # Create the base map
  map <- leaflet::leaflet() %>%
    leaflet::addTiles(group = "OpenStreetMap")

  # Add additional base layers if requested
  if (base_layers) {
    map <- map %>%
      leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap, group = "Ocean") %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group = "Simple") %>%
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Satellite") %>%
      leaflet::addLayersControl(
        baseGroups = c("OpenStreetMap", "Ocean", "Simple", "Satellite"),
        position = "topright"
      )
  }

  # Add title
  if (!is.null(title)) {
    map <- map %>%
      leaflet::addControl(
        html = paste0("<h4>", title, "</h4>"),
        position = "topleft"
      )
  }

  # Add the coastline
  map <- map %>%
    leaflet::addPolylines(
      data = coastline,
      lng = ~Long,
      lat = ~Lat,
      color = "darkgrey",
      weight = 1.5,
      opacity = 0.8,
      group = "Coastline"
    )

  # Create layer group for the legend
  overlay_groups <- c()

  # Add each polygon
  for (i in seq_along(polygons)) {
    polygon <- polygons[[i]]
    color_idx <- (i - 1) %% length(colors) + 1

    # If there are labels, use them for the legend
    label <- if (!is.null(labels) && length(labels) >= i) {
      labels[i]
    } else {
      # Extract announcement name as default label
      polygon$announcement
    }

    # Save for the legend
    overlay_groups <- c(overlay_groups, label)

    # Prepare more detailed popup
    popup_content <- paste0(
      "<div style='max-width: 300px;'>",
      "<h4>", label, "</h4>",
      "<strong>Announcement: </strong>", polygon$announcement, "<br>",
      "<strong>Start date: </strong>", format(polygon$start_date, "%d/%m/%Y %H:%M"), "<br>",
      "<strong>End date: </strong>", format(polygon$end_date, "%d/%m/%Y %H:%M"), "<br>",
      "<strong>Start Lat: </strong>", polygon$Start_Lat, "<br>",
      "<strong>Start Lon: </strong>", polygon$Start_Long, "<br>",
      "<strong>End Lat: </strong>", polygon$End_Lat, "<br>",
      "<strong>End Lon: </strong>", polygon$End_Long, "<br>",
      "<strong>Start Nautical Miles: </strong>", polygon$StartNauticalMiles, "<br>",
      "<strong>End Nautical Miles: </strong>", polygon$EndNauticalMiles, "<br>",
      "</div>"
    )

    # Convert coordinates to the format expected by leaflet
    leaflet_coords <- list(
      lng = polygon$coords[, 1],
      lat = polygon$coords[, 2]
    )

    map <- map %>%
      leaflet::addPolygons(
        lng = leaflet_coords$lng,
        lat = leaflet_coords$lat,
        fillColor = colors[color_idx],
        fillOpacity = 0.7,
        color = "black",
        weight = 1,
        popup = popup_content,
        group = label,
        label = label,
        highlightOptions = leaflet::highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.8,
          bringToFront = TRUE
        )
      )
  }

  # Add layer control for polygons if legend is requested
  if (show_legend && length(overlay_groups) > 0) {
    map <- map %>%
      leaflet::addLayersControl(
        baseGroups = if (base_layers) c("OpenStreetMap", "Ocean", "Simple", "Satellite") else "OpenStreetMap",
        overlayGroups = overlay_groups,
        position = "topright",
        options = leaflet::layersControlOptions(collapsed = FALSE)
      )
  }

  # Add scale
  map <- map %>%
    leaflet::addScaleBar(position = "bottomleft", options = leaflet::scaleBarOptions(imperial = FALSE))

  # Add minimap if requested
  if (minimap && requireNamespace("leaflet", quietly = TRUE)) {
    map <- map %>%
      leaflet::addMiniMap(
        tiles = leaflet::providers$CartoDB.Positron,
        toggleDisplay = TRUE,
        position = "bottomright"
      )
  }

  # Calculate map bounds
  all_longs <- unlist(lapply(polygons, function(p) p$coords[, 1]))
  all_lats <- unlist(lapply(polygons, function(p) p$coords[, 2]))

  # Adjust view to data bounds (with a margin)
  map <- map %>%
    leaflet::fitBounds(
      lng1 = min(all_longs) - 0.5,
      lat1 = min(all_lats) - 0.5,
      lng2 = max(all_longs) + 0.5,
      lat2 = max(all_lats) + 0.5
    )

  return(map)
}



#' Find the parallel line closest to a distance in nautical miles
#'
#' @description
#' This function searches within a list of lines parallel to the coast for the one that best
#' approximates a specific distance in nautical miles.
#'
#' @param miles Numeric value indicating the distance in nautical miles to be found.
#' @param coast_parallels List of data frames with parallel lines at different distances.
#'        Each element must have a name indicating the distance (e.g. "l5", "l10").
#'
#' @return A list with two elements:
#' \itemize{
#'   \item df: The data frame with the parallel line closest to the requested distance.
#'   \item actual_mile: The numeric value of the mile actually found.
#' }
#'
#' @keywords internal
find_parallel_line <- function(miles, coast_parallels) {
  # Extract mile numbers from list names
  available_miles <- as.numeric(gsub("l", "", names(coast_parallels)))

  # Find the closest mile
  idx_close <- which.min(abs(available_miles - miles))
  close_mile <- available_miles[idx_close]

  # Return the corresponding dataframe
  return(list(
    df = coast_parallels[[paste0("l", close_mile)]],
    actual_mile = close_mile
  ))
}


#' Interpolates a point for a specific latitude on a parallel line
#'
#' @description
#' This function calculates the longitude corresponding to a specific latitude on a line
#' parallel to the coast, interpolating between existing points when necessary.
#'
#' @param line Data frame representing a line parallel to the coast.
#' @param latitude Numeric value of the latitude for which the longitude is to be found.
#' @param lat_name Name of the column containing latitudes in the data frame.
#' @param lon_name Name of the column containing longitudes in the data frame.
#'
#' @return A vector with two elements:
#' \itemize{
#'   \item lon: Interpolated longitude for the given latitude.
#'   \item lat: The same latitude provided as input.
#' }
#'
#' @details
#' The function sorts the line by latitude, finds the points that enclose the desired
#' latitude and performs a linear interpolation. If suitable points for interpolation
#' are not found, it returns the closest point.
#'
#' @keywords internal
interpolate_point <- function(line, latitude, lat_name, lon_name) {
  # Sort by latitude to ensure correct interpolation
  line <- line[order(line[[lat_name]]), ]

  # Find the points that enclose the desired latitude
  idx_inf <- max(which(line[[lat_name]] <= latitude), na.rm = TRUE)
  idx_sup <- min(which(line[[lat_name]] >= latitude), na.rm = TRUE)

  # Handle special cases
  if (length(idx_inf) == 0 || is.infinite(idx_inf)) {
    # If there are no points below, use the closest point
    idx_close <- which.min(abs(line[[lat_name]] - latitude))
    return(c(
      lon = line[[lon_name]][idx_close],
      lat = line[[lat_name]][idx_close]
    ))
  }

  if (length(idx_sup) == 0 || is.infinite(idx_sup)) {
    # If there are no points above, use the closest point
    idx_close <- which.min(abs(line[[lat_name]] - latitude))
    return(c(
      lon = line[[lon_name]][idx_close],
      lat = line[[lat_name]][idx_close]
    ))
  }

  # If the latitude exactly matches a point, use it directly
  if (idx_inf == idx_sup) {
    return(c(
      lon = line[[lon_name]][idx_inf],
      lat = line[[lat_name]][idx_inf]
    ))
  }

  # Interpolate between the two points
  lat_inf <- line[[lat_name]][idx_inf]
  lat_sup <- line[[lat_name]][idx_sup]
  lon_inf <- line[[lon_name]][idx_inf]
  lon_sup <- line[[lon_name]][idx_sup]

  # Calculate proportion for interpolation
  prop <- (latitude - lat_inf) / (lat_sup - lat_inf)
  longitude <- lon_inf + prop * (lon_sup - lon_inf)

  return(c(
    lon = longitude,
    lat = latitude
  ))
}



#' Extract all points of a parallel line between two latitudes
#'
#' @description
#' This function obtains all points of a line parallel to the coast that are between
#' two given latitudes, including interpolated points at the exact limits.
#'
#' @param line Data frame representing a line parallel to the coast.
#' @param lat_min Numeric value of the minimum latitude (southernmost).
#' @param lat_max Numeric value of the maximum latitude (northernmost).
#' @param lat_name Name of the column containing latitudes in the data frame.
#' @param lon_name Name of the column containing longitudes in the data frame.
#'
#' @return A matrix of points with columns corresponding to longitude and latitude.
#'
#' @details
#' The function first filters the points that are within the latitude range.
#' If necessary, it interpolates additional points for the exact limits of
#' lat_min and lat_max. If there are not enough points in the range, it creates a straight
#' segment between the interpolated points at the limits.
#'
#' @keywords internal
extract_points_between_latitudes <- function(line, lat_min, lat_max, lat_name, lon_name) {
  # Sort by latitude
  line <- line[order(line[[lat_name]]), ]

  # Filter points within the latitude range
  filtered_points <- line[line[[lat_name]] >= lat_min & line[[lat_name]] <= lat_max, ]

  # If there are not enough points within the range, we'll have to interpolate
  if (nrow(filtered_points) < 2) {
    # Interpolate points at the limits
    min_point <- interpolate_point(line, lat_min, lat_name, lon_name)
    max_point <- interpolate_point(line, lat_max, lat_name, lon_name)

    # Create point matrix
    points <- rbind(
      min_point,
      max_point
    )
    colnames(points) <- c(lon_name, lat_name)

    return(points)
  }

  # Check if we need to add interpolated points at the exact limits
  first_point <- filtered_points[1, ]
  last_point <- filtered_points[nrow(filtered_points), ]

  limit_points <- NULL

  # If the first point is not exactly at lat_min, interpolate
  if (abs(first_point[[lat_name]] - lat_min) > 0.0001) {
    min_point <- interpolate_point(line, lat_min, lat_name, lon_name)
    limit_points <- rbind(limit_points, min_point)
  }

  # Middle points (the filtered ones)
  middle_points <- as.matrix(filtered_points[, c(lon_name, lat_name)])

  # If the last point is not exactly at lat_max, interpolate
  if (abs(last_point[[lat_name]] - lat_max) > 0.0001) {
    max_point <- interpolate_point(line, lat_max, lat_name, lon_name)
    limit_points <- rbind(limit_points, max_point)
  }

  # Combine all points
  if (!is.null(limit_points)) {
    colnames(limit_points) <- c(lon_name, lat_name)

    # Determine which points go before and which after
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
#' The east and west edges of the polygon will follow the shape of the lines parallel
#' to the coast at the specified nautical miles, while the north and south edges will be
#' straight lines at the given latitudes.
#'
#' @param data A data frame with coordinates.
#' @param coastline A data frame with the coastline.
#' @param coast_parallels List of data frames with lines parallel to the coast at different distances.
#'        Each element of the list must have a name indicating the distance (e.g. "l5", "l10").
#' @param column_names A vector with column names in the coast_parallels data frames.
#'        Must contain: c("lat", "lon", "dc") where "dc" is the distance to the coast.
#'
#' @return A list of polygons for visualization.
#' @keywords internal
prepare_polygons <- function(data, coastline, coast_parallels = NULL, column_names = c("lat", "lon", "dc")) {
  # Parameter validation
  if (missing(data)) {
    stop("The 'data' parameter is required.")
  }

  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame.")
  }

  if (!is.data.frame(coastline) || !all(c("Long", "Lat") %in% names(coastline))) {
    stop("'coastline' must be a data.frame with columns 'Long' and 'Lat'.")
  }

  # Check if there are coordinates to work with
  if (nrow(data) == 0) {
    stop("The data frame 'data' contains no rows.")
  }

  # Validate coast_parallels if provided
  if (!is.null(coast_parallels)) {
    if (!is.list(coast_parallels)) {
      stop("'coast_parallels' must be a list of data.frames.")
    }

    # Validate that each element of the list is a data.frame with the necessary columns
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

    # Check that we have at least one valid element
    valid_elements <- sapply(names(coast_parallels), function(name) {
      df <- coast_parallels[[name]]
      is.data.frame(df) && all(column_names %in% names(df))
    })

    if (!any(valid_elements)) {
      warning("No element in coast_parallels has the correct format. The original approximation method will be used.")
      coast_parallels <- NULL
    }
  }

  # Prepare the data for visualization
  prepared_data <- data

  # Convert text coordinates to numeric if necessary
  if (any(c("StartLatitude", "EndLatitude") %in% names(data))) {
    if (!"lat_ini" %in% names(data)) {
      prepared_data$lat_ini <- Tivy::dms_to_decimal(data$StartLatitude)
    }
    if (!"lat_fin" %in% names(data)) {
      prepared_data$lat_fin <- Tivy::dms_to_decimal(data$EndLatitude)
    }
  }

  if (any(c("StartLongitude", "EndLongitude") %in% names(data))) {
    if (!"lon_ini" %in% names(data)) {
      prepared_data$lon_ini <- Tivy::dms_to_decimal(data$StartLongitude)
    }
    if (!"lon_fin" %in% names(data)) {
      prepared_data$lon_fin <- Tivy::dms_to_decimal(data$EndLongitude)
    }
  }

  # Create list to store polygons
  polygons <- list()

  # Process each row to create polygons
  for (i in 1:nrow(prepared_data)) {
    # Check if we have nautical miles instead of longitudes
    if (!is.na(data$StartNauticalMiles[i]) && !is.na(data$EndNauticalMiles[i]) &&
        (is.na(prepared_data$lon_ini[i]) || is.na(prepared_data$lon_fin[i]))) {

      # Get decimal latitudes and make sure they are in the correct order
      lat_north <- max(prepared_data$lat_ini[i], prepared_data$lat_fin[i])
      lat_south <- min(prepared_data$lat_ini[i], prepared_data$lat_fin[i])

      # Get miles
      miles_ini <- data$StartNauticalMiles[i]  # Closer to the coast
      miles_fin <- data$EndNauticalMiles[i]     # Further from the coast

      # Consider using preexisting coast_parallels
      if (!is.null(coast_parallels)) {
        lat_name <- column_names[1] # Latitude column
        lon_name <- column_names[2] # Longitude column

        # Find parallel lines for the initial and final miles
        line_ini_info <- find_parallel_line(miles_ini, coast_parallels)
        line_fin_info <- find_parallel_line(miles_fin, coast_parallels)

        line_ini <- line_ini_info$df  # Line closest to the coast
        line_fin <- line_fin_info$df  # Line furthest from the coast

        # If we find lines for both miles
        if (!is.null(line_ini) && !is.null(line_fin) &&
            nrow(line_ini) > 0 && nrow(line_fin) > 0) {

          # Interpolate exact points at the corners
          point_ne <- interpolate_point(line_ini, lat_north, lat_name, lon_name)
          point_se <- interpolate_point(line_ini, lat_south, lat_name, lon_name)
          point_nw <- interpolate_point(line_fin, lat_north, lat_name, lon_name)
          point_sw <- interpolate_point(line_fin, lat_south, lat_name, lon_name)

          # Extract points from the west line (furthest) between latitudes
          west_points <- extract_points_between_latitudes(
            line_fin, lat_south, lat_north, lat_name, lon_name
          )

          # Extract points from the east line (closest) between latitudes
          east_points <- extract_points_between_latitudes(
            line_ini, lat_south, lat_north, lat_name, lon_name
          )

          # Create coordinates for the complete polygon
          # 1. Start at the northeast corner and go north along the east edge
          # 2. Then go from north to south along the west edge
          # 3. Return to the starting point to close the polygon

          # Sort west points from north to south
          west_points <- west_points[order(west_points[, 2], decreasing = TRUE), ]

          # Sort east points from south to north
          east_points <- east_points[order(east_points[, 2]), ]

          # Combine all points to form the polygon
          coords <- rbind(
            east_points,          # East edge (south to north)
            west_points,         # West edge (north to south)
            east_points[1, ]      # Close the polygon by returning to the first point
          )

          # Create polygon object
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
          next  # Continue to the next iteration
        }
      }

      # If we get here, it's because we couldn't create a detailed polygon with parallel lines
      # Fall back to the original method with approximation
      coast_lon_lat_north <- calculate_coast_longitude(coastline, lat_north)
      coast_lon_lat_south <- calculate_coast_longitude(coastline, lat_south)

      # Conversion factor adjusted by each latitude
      factor_lat_north <- cos(lat_north * pi/180)
      factor_lat_south <- cos(lat_south * pi/180)

      # Convert miles to degrees for each latitude
      offset_ini_lat_north <- miles_ini / 60 / factor_lat_north
      offset_fin_lat_north <- miles_fin / 60 / factor_lat_north
      offset_ini_lat_south <- miles_ini / 60 / factor_lat_south
      offset_fin_lat_south <- miles_fin / 60 / factor_lat_south

      # Create polygon coordinates with 4 corners (original method)
      coords <- rbind(
        c(coast_lon_lat_north - offset_ini_lat_north, lat_north),  # Northeast corner
        c(coast_lon_lat_north - offset_fin_lat_north, lat_north),  # Northwest corner
        c(coast_lon_lat_south - offset_fin_lat_south, lat_south),  # Southwest corner
        c(coast_lon_lat_south - offset_ini_lat_south, lat_south),  # Southeast corner
        c(coast_lon_lat_north - offset_ini_lat_north, lat_north)   # Close the polygon
      )

      # Create polygon object (original method)
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

      # Case with explicit coordinates: create rectangular polygon
      coords <- rbind(
        c(prepared_data$lon_ini[i], prepared_data$lat_ini[i]),  # Northwest corner
        c(prepared_data$lon_fin[i], prepared_data$lat_ini[i]),  # Northeast corner
        c(prepared_data$lon_fin[i], prepared_data$lat_fin[i]),  # Southeast corner
        c(prepared_data$lon_ini[i], prepared_data$lat_fin[i]),  # Southwest corner
        c(prepared_data$lon_ini[i], prepared_data$lat_ini[i])   # Close the polygon
      )

      # Create polygon object
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

  # Filter NA polygons
  polygons <- polygons[!sapply(polygons, is.null)]

  if (length(polygons) == 0) {
    stop("No valid polygons could be created with the provided data.")
  }

  return(polygons)
}



#' Process a block of rows for length weighting
#'
#' Helper function that processes a set of rows to calculate weighted length.
#' Can be used directly to process data subsets.
#'
#' @param df Data frame to process
#' @param length_cols Names of the length columns
#' @param catch_col Name of the catch column
#' @param a Coefficient of the length-weight relationship
#' @param b Exponent of the length-weight relationship
#' @param silence_warnings Whether to suppress warnings
#' @return Data frame with weighted columns added with the prefix "pond_"
#'
#' @keywords internal
process_block <- function(df, length_cols, catch_col, a, b, silence_warnings = FALSE) {
  # Get list of length
  size_values <- as.numeric(sub(".*_", "", length_cols))
  if (all(is.na(size_values))) {
    size_values <- as.numeric(length_cols)
  }

  # If there are still NAs, use sequence
  if (any(is.na(size_values))) {
    warning("Could not extract numerical length values, using sequence 1:n")
    size_values <- seq_along(length_cols)
  }

  # Counter for warnings
  warning_counter <- list(
    catch_na = 0,
    freq_zero = 0,
    weight_zero = 0,
    length_zero = 0
  )

  # Process each row
  result <- df
  for (i in 1:nrow(df)) {
    catch_i <- df[i, catch_col]
    frequencies_i <- as.numeric(df[i, length_cols])

    # Count possible warnings
    if (is.na(catch_i) || catch_i <= 0) warning_counter$catch_na <- warning_counter$catch_na + 1
    if (sum(frequencies_i, na.rm = TRUE) == 0) warning_counter$freq_zero <- warning_counter$freq_zero + 1
    if (any(size_values <= 0, na.rm = TRUE)) warning_counter$length_zero <- warning_counter$length_zero + 1

    # Calculate weighting (silencing individual warnings)
    weighted <- Tivy::weighting(
      frequency = frequencies_i,
      catch = catch_i,
      length = size_values,
      a = a,
      b = b,
      silence_warnings = TRUE
    )

    # Add results
    for (j in seq_along(length_cols)) {
      result[i, paste0("pond_", size_values[j])] <- weighted[j]
    }
  }

  # Show warning summary at the end
  if (silence_warnings == FALSE) {
    warnings_text <- character(0)
    if (warning_counter$catch_na > 0)
      warnings_text <- c(warnings_text,
                         paste0(warning_counter$catch_na, " rows with catch NA or <= 0"))
    if (warning_counter$freq_zero > 0)
      warnings_text <- c(warnings_text,
                         paste0(warning_counter$freq_zero, " rows with sum of frequencies = 0"))
    if (warning_counter$length_zero > 0)
      warnings_text <- c(warnings_text,
                         paste0("There are length <= 0 that could produce invalid results"))

    if (length(warnings_text) > 0) {
      warning("Summary of problems found: ", paste(warnings_text, collapse = "; "))
    }
  }

  return(result)
}