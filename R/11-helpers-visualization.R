#' Generate static plot of polygons on a map
#'
#' @description
#' Creates a static plot using ggplot2 that displays geographic polygons on a coastline base.
#' Colors are assigned by groups (announcements/labels), not individual polygons.
#'
#' @param polygons List of polygons. Each polygon must contain a coords matrix with longitude and latitude columns.
#' @param coastline Data frame with the coastline, with columns Long and Lat.
#' @param title Main title of the plot.
#' @param colors Vector of colors to fill the polygons. If NULL, colors are automatically assigned by groups.
#' @param show_legend Logical. If TRUE, the legend is displayed.
#' @param name_legend Name of the legend (optional). If NULL, legend has no title.
#' @param labels Vector of labels for the polygons (optional).
#' @param add_grid Logical. If TRUE, adds a geographic grid to the plot.
#' @param theme ggplot2 theme to use.
#'
#' @return A ggplot object ready to be plotted.
#' @keywords internal
#' @importFrom RColorBrewer brewer.pal
#' @import ggplot2
plot_zones_static <- function(polygons, coastline, title, colors, show_legend = TRUE, name_legend = NULL,
                          labels = NULL, add_grid = TRUE, theme = ggplot2::theme_minimal()) {

  p <- ggplot2::ggplot()

  p <- p + ggplot2::geom_path(data = coastline, ggplot2::aes(x = .data[["Long"]], y = .data[["Lat"]]),
                              color = "darkgrey", linewidth = 0.5)

  unique_groups <- character()
  for (i in seq_along(polygons)) {
    polygon <- polygons[[i]]
    
    group_label <- if (!is.null(labels) && length(labels) >= i) {
      labels[i]
    } else {
      polygon$announcement
    }
    
    unique_groups <- c(unique_groups, group_label)
  }
  
  unique_groups <- unique(unique_groups)
  n_groups <- length(unique_groups)

  if(is.null(colors)){
    colors <- generate_robust_colors(n_groups)
  }
  
  if (length(colors) < n_groups) {
    warning("Not enough colors provided. Expanding color palette.")
    colors <- generate_robust_colors(n_groups)
  }

  color_mapping <- setNames(colors[1:n_groups], unique_groups)

  df_list <- list()
  for (i in seq_along(polygons)) {
    polygon <- polygons[[i]]

    group_label <- if (!is.null(labels) && length(labels) >= i) {
      labels[i]
    } else {
      polygon$announcement
    }

    coords_df <- as.data.frame(polygon$coords)
    colnames(coords_df) <- c("Long", "Lat")
    coords_df$group <- group_label
    coords_df$id <- polygon$id

    df_list[[i]] <- coords_df
  }

  df_polygons <- do.call(rbind, df_list)

  p <- p + ggplot2::geom_polygon(
    data = df_polygons,
    ggplot2::aes(x = .data[["Long"]], y = .data[["Lat"]], fill = .data[["group"]], group = .data[["id"]]),
    alpha = 0.7,
    color = "black"
  )

  p <- p + ggplot2::scale_fill_manual(
    values = color_mapping,
    name = name_legend,
    guide = if(is.null(name_legend)) ggplot2::guide_legend(title = NULL) else ggplot2::guide_legend()
  )

  p <- p + ggplot2::labs(
    title = title,
    x = "Longitude",
    y = "Latitude"
  ) + theme

  if (add_grid) {
    p <- p + ggplot2::coord_quickmap() +
      ggplot2::scale_x_continuous(breaks = seq(-82, -70, by = 2)) +
      ggplot2::scale_y_continuous(breaks = seq(-20, -8, by = 2)) +
      ggplot2::theme(panel.grid = ggplot2::element_line(color = "lightgrey", linetype = "dashed"))
  } else {
    p <- p + ggplot2::coord_quickmap()
  }

  if (!show_legend) {
    p <- p + ggplot2::theme(legend.position = "none")
  } else {
    p <- p + ggplot2::theme(
      legend.position = "right",
      legend.background = ggplot2::element_rect(fill = "white", color = "grey80"),
      legend.margin = ggplot2::margin(5, 5, 5, 5)
    )
  }

  if (n_groups > 15) {
    message(sprintf("Note: Displaying %d groups. Consider grouping similar categories for better visualization.", n_groups))
  }

  return(p)
}

#' Generate robust color palette for any number of groups
#'
#' @description
#' Creates a color palette that can handle any number of groups without errors.
#' Uses RColorBrewer when possible, falls back to other methods for large numbers.
#'
#' @param n_groups Integer. Number of groups/colors needed.
#'
#' @return Character vector of colors.
#' @keywords internal
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices hsv
generate_robust_colors <- function(n_groups) {
  
  if (n_groups == 0) {
    return(character(0))
  }
  
  if (n_groups == 1) {
    return("#1f77b4")
  }
  
  if (n_groups <= 12) {
    n_brewer <- max(3, n_groups)
    colors <- RColorBrewer::brewer.pal(n_brewer, "Set3")[1:n_groups]
    return(colors)
  }
  
  if (n_groups <= 29) {
    pal1 <- RColorBrewer::brewer.pal(12, "Set3")
    pal2 <- RColorBrewer::brewer.pal(8, "Dark2") 
    pal3 <- RColorBrewer::brewer.pal(9, "Pastel1")
    
    all_colors <- c(pal1, pal2, pal3)
    return(all_colors[1:n_groups])
  }
  
  if (n_groups > 29) {
    warning(sprintf("Large number of groups (%d). Using algorithmic color generation. Consider grouping categories.", n_groups))
    
    hues <- seq(0, 1, length.out = n_groups + 1)[1:n_groups]
    sats <- rep(c(0.7, 0.9, 0.5), length.out = n_groups)
    vals <- rep(c(0.8, 0.6, 0.9), length.out = n_groups)
    
    colors <- grDevices::hsv(h = hues, s = sats, v = vals)
    return(colors)
  }
}

#' Generate interactive plot of polygons with leaflet
#'
#' @description
#' Creates an interactive map using leaflet, showing polygons with popup information.
#'
#' @param polygons List of polygons. Each must have fields such as coords, announcement, dates and coordinates.
#' @param coastline Data frame with the coastline (columns Long and Lat).
#' @param title Title to display at the top of the map.
#' @param colors Vector of colors. If NULL, they are automatically assigned with RColorBrewer::brewer.pal.
#' @param show_legend Logical. If TRUE, the layers control (legend) is displayed.
#' @param labels Optional vector of names to display in the legend and map labels.
#' @param base_layers Logical. If TRUE, includes base layers such as satellite and ocean maps.
#' @param minimap Logical. If TRUE, displays a minimap in the lower right corner.
#'
#' @return A leaflet object with the interactive map.
#' @keywords internal
#' @importFrom RColorBrewer brewer.pal
#' @import leaflet
plot_zones_interactive <- function(polygons, coastline, title, colors, show_legend = TRUE,
                               labels = NULL, base_layers = TRUE, minimap = TRUE) {

  if(is.null(colors)){
    colors <- RColorBrewer::brewer.pal(n = length(polygons), name = "Set3")
  }

  map <- leaflet::leaflet() %>%
    leaflet::addTiles(group = "OpenStreetMap")

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

  if (!is.null(title)) {
    map <- map %>%
      leaflet::addControl(
        html = paste0("<h4>", title, "</h4>"),
        position = "topleft"
      )
  }

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

  overlay_groups <- c()

  for (i in seq_along(polygons)) {
    polygon <- polygons[[i]]
    color_idx <- (i - 1) %% length(colors) + 1

    label <- if (!is.null(labels) && length(labels) >= i) {
      labels[i]
    } else {
      polygon$announcement
    }

    overlay_groups <- c(overlay_groups, label)

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

  if (show_legend && length(overlay_groups) > 0) {
    map <- map %>%
      leaflet::addLayersControl(
        baseGroups = if (base_layers) c("OpenStreetMap", "Ocean", "Simple", "Satellite") else "OpenStreetMap",
        overlayGroups = overlay_groups,
        position = "topright",
        options = leaflet::layersControlOptions(collapsed = FALSE)
      )
  }

  map <- map %>%
    leaflet::addScaleBar(position = "bottomleft", options = leaflet::scaleBarOptions(imperial = FALSE))

  if (minimap && requireNamespace("leaflet", quietly = TRUE)) {
    map <- map %>%
      leaflet::addMiniMap(
        tiles = leaflet::providers$CartoDB.Positron,
        toggleDisplay = TRUE,
        position = "bottomright"
      )
  }

  all_longs <- unlist(lapply(polygons, function(p) p$coords[, 1]))
  all_lats <- unlist(lapply(polygons, function(p) p$coords[, 2]))

  map <- map %>%
    leaflet::fitBounds(
      lng1 = min(all_longs) - 0.5,
      lat1 = min(all_lats) - 0.5,
      lng2 = max(all_longs) + 0.5,
      lat2 = max(all_lats) + 0.5
    )

  return(map)
}

#' Helper function to create base plot for juvenile analysis
#' @keywords internal
create_juvenile_base_plot <- function(data_long, x_var, fill_var, use_facet_wrap, group_by_type) {
  if (is.null(fill_var)) {
    p <- ggplot2::ggplot(
      data_long,
      ggplot2::aes(
        x = .data[[x_var]],
        y = .data[["percentage"]],
        fill = .data[["type"]]
      )
    )
  } else {
    if (use_facet_wrap) {
      p <- ggplot2::ggplot(
        data_long,
        ggplot2::aes(
          x = .data[[x_var]],
          y = .data[["percentage"]],
          fill = .data[[fill_var]]
        )
      )
    } else if (group_by_type) {
      p <- ggplot2::ggplot(
        data_long,
        ggplot2::aes(
          x = .data[[x_var]],
          y = .data[["percentage"]],
          fill = .data[[fill_var]],
          group = interaction(.data[["type"]], .data[[fill_var]])
        )
      )
    } else {
      p <- ggplot2::ggplot(
        data_long,
        ggplot2::aes(
          x = .data[[x_var]],
          y = .data[["percentage"]],
          fill = interaction(.data[[fill_var]], .data[["type"]])
        )
      )
    }
  }
  return(p)
}

#' Helper function to add geoms to juvenile plots
#' @keywords internal
add_juvenile_geoms <- function(p, plot_type, bar_position, fill_var, use_facet_wrap, group_by_type) {
  if (plot_type == "bars") {
    if (!is.null(fill_var) && !use_facet_wrap && group_by_type) {
      if (bar_position == "dodge") {
        p <- p + ggplot2::geom_bar(
          stat = "identity",
          position = ggplot2::position_dodge2(width = 0.9, preserve = "single"),
          ggplot2::aes(alpha = .data[["type"]])
        )
        p <- p + ggplot2::scale_alpha_manual(values = c(1, 0.6), name = "Type")
      } else {
        p <- p + ggplot2::geom_bar(
          stat = "identity",
          position = bar_position,
          ggplot2::aes(alpha = .data[["type"]])
        )
        p <- p + ggplot2::scale_alpha_manual(values = c(1, 0.6), name = "Type")
      }
    } else {
      position_func <- if (bar_position == "dodge") {
        ggplot2::position_dodge(width = 0.9)
      } else {
        bar_position
      }
      p <- p + ggplot2::geom_bar(stat = "identity", position = position_func)
    }
  } else if (plot_type == "lines") {
    if (is.null(fill_var)) {
      p <- p + ggplot2::aes(group = .data[["type"]], color = .data[["type"]])
    } else {
      p <- p + ggplot2::aes(group = .data[[fill_var]], color = .data[[fill_var]])
    }
    p <- p + ggplot2::geom_line(size = 1) + ggplot2::geom_point(size = 2)
  } else if (plot_type == "points") {
    if (is.null(fill_var)) {
      p <- p + ggplot2::aes(color = .data[["type"]])
    } else {
      p <- p + ggplot2::aes(color = .data[[fill_var]])
    }
    p <- p + ggplot2::geom_point(size = 3, alpha = 0.7)
  } else if (plot_type == "mixed") {
    if (is.null(fill_var)) {
      p <- p + ggplot2::aes(group = .data[["type"]], color = .data[["type"]])
    } else {
      p <- p + ggplot2::aes(group = .data[[fill_var]], color = .data[[fill_var]])
    }
    
    position_func <- if (bar_position == "dodge") {
      ggplot2::position_dodge(width = 0.9)
    } else {
      bar_position
    }
    
    p <- p + 
      ggplot2::geom_bar(stat = "identity", alpha = 0.7, position = position_func) +
      ggplot2::geom_point(size = 2, position = position_func)
  }
  
  return(p)
}

#' Helper function to add faceting to juvenile plots
#' @keywords internal
add_juvenile_faceting <- function(p, facet_var, use_facet_wrap, facet_cols, fill_var) {
  if (is.null(fill_var) && is.null(facet_var)) {
    p <- p + ggplot2::facet_wrap(~ type, ncol = facet_cols)
  } else if (!is.null(facet_var)) {
    if (use_facet_wrap) {
      facet_formula <- stats::as.formula(paste("~ type +", facet_var))
      p <- p + ggplot2::facet_wrap(facet_formula, ncol = facet_cols)
    } else {
      facet_formula <- stats::as.formula(paste("~", facet_var))
      p <- p + ggplot2::facet_wrap(facet_formula, ncol = facet_cols)
    }
  } else if (use_facet_wrap && !is.null(fill_var)) {
    p <- p + ggplot2::facet_wrap(~ type, ncol = facet_cols)
  }
  
  return(p)
}

#' Helper function to customize axes in juvenile plots
#' @keywords internal
customize_juvenile_axes <- function(p, data_long, x_var, x_date_breaks, y_limits) {
  if (is.factor(data_long[[x_var]]) || is.character(data_long[[x_var]])) {
    p <- p + ggplot2::scale_x_discrete(drop = FALSE)
  } else if (inherits(data_long[[x_var]], "Date")) {
    if (!is.null(x_date_breaks)) {
      p <- p + ggplot2::scale_x_date(date_breaks = x_date_breaks, date_labels = "%d %b %Y")
    } else {
      p <- p + ggplot2::scale_x_date(date_breaks = "1 day", date_labels = "%d %b %Y")
    }
  } else if (is.numeric(data_long[[x_var]])) {
    unique_vals <- length(unique(data_long[[x_var]]))
    if (unique_vals <= 20) {
      p <- p + ggplot2::scale_x_continuous(breaks = unique(data_long[[x_var]]))
    }
  }
  
  if (!is.null(y_limits)) {
    p <- p + ggplot2::ylim(y_limits)
  }
  
  return(p)
}

#' Helper function to apply theme to juvenile plots
#' @keywords internal
apply_juvenile_theme <- function(p, theme_style, legend_position, rotate_x_labels) {
  if (theme_style == "classic") {
    p <- p + ggplot2::theme_classic()
  } else if (theme_style == "minimal") {
    p <- p + ggplot2::theme_minimal()
  } else if (theme_style == "light") {
    p <- p + ggplot2::theme_light()
  } else if (theme_style == "dark") {
    p <- p + ggplot2::theme_dark()
  }
  
  p <- p + ggplot2::theme(
    legend.position = legend_position,
    panel.grid.minor = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(fill = "lightgray", color = NA),
    strip.text = ggplot2::element_text(face = "bold")
  )
  
  if (rotate_x_labels) {
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }
  
  return(p)
}

#' Helper function to add reference line to juvenile plots
#' @keywords internal
add_juvenile_reference_line <- function(p, reference_line, data_long, x_var) {
  if (inherits(data_long[[x_var]], "Date") || is.numeric(data_long[[x_var]])) {
    x_point <- mean(data_long[[x_var]], na.rm = TRUE)
  } else {
    unique_levels <- unique(as.character(data_long[[x_var]]))
    x_point <- unique_levels[ceiling(length(unique_levels) / 2)]
  }
  
  p <- p + 
    ggplot2::geom_hline(
      yintercept = reference_line,
      linetype = "dashed",
      color = "red",
      size = 1
    ) +
    ggplot2::annotate(
      "text",
      x = x_point,
      y = reference_line + 2,
      label = paste("Limit:", reference_line, "%"),
      hjust = 0,
      color = "red",
      fontface = "bold"
    )
  
  return(p)
}