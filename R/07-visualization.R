#' Plot fishing zones
#'
#' @description
#' Creates visualizations of fishing zones using either ggplot2 (static) or 
#' leaflet (interactive). This function consolidates all zone plotting functionality.
#'
#' @param data Data frame with fishing zone coordinates and metadata.
#' @param coastline Data frame with coastline coordinates (columns "Long" and "Lat").
#'   If NULL, uses internal dataset.
#' @param parallels List of data frames with coast-parallel lines.
#' @param type Plot type: "static" for ggplot2 or "interactive" for leaflet.
#' @param title Plot title.
#' @param colors Vector of colors for zones. If NULL, auto-generated.
#' @param show_legend Logical. Show legend/layer control.
#' @param legend_title Legend title.
#' @param zone_labels Vector of custom labels for zones.
#' @param add_grid Logical. Add coordinate grid (static only).
#' @param base_layers Logical. Include multiple base map layers (interactive only).
#' @param minimap Logical. Add minimap (interactive only).
#'
#' @return ggplot object (static) or leaflet object (interactive).
#'
#' @examples
#' \dontrun{
#' plot_fishing_zones(
#'   data = zone_data,
#'   coastline = coastline_data,
#'   type = "static",
#'   title = "Fishing Zones",
#'   show_legend = TRUE
#' )
#' 
#' plot_fishing_zones(
#'   data = zone_data,
#'   coastline = coastline_data,
#'   type = "interactive",
#'   base_layers = TRUE,
#'   minimap = TRUE
#' )
#' }
#'
#' @export
#' @import ggplot2 leaflet
#' @importFrom RColorBrewer brewer.pal
plot_fishing_zones <- function(data,
                              coastline = NULL,
                              parallels = NULL,
                              type = "static",
                              title = NULL,
                              colors = NULL,
                              show_legend = FALSE,
                              legend_title = NULL,
                              zone_labels = NULL,
                              add_grid = FALSE,
                              base_layers = FALSE,
                              minimap = FALSE) {
  
  if (!type %in% c("static", "interactive")) {
    stop("type must be either 'static' or 'interactive'")
  }
  
  if (is.null(coastline)) {
    coastline <- peru_coastline
  }
  
  if (!is.data.frame(coastline) || !all(c("Long", "Lat") %in% names(coastline))) {
    stop("Coastline must be a data.frame with 'Long' and 'Lat' columns")
  }
  
  if (is.null(parallels)) {
    parallels <- peru_coast_parallels
  }
    
  polygons <- prepare_polygons(data = data, coastline = coastline, coast_parallels = parallels)
  
  if (type == "static") {
    return(plot_zones_static(
      polygons = polygons,
      coastline = coastline,
      title = title,
      colors = colors,
      show_legend = show_legend,
      name_legend = legend_title,
      labels = zone_labels,
      add_grid = add_grid
    ))
  } else {
    return(plot_zones_interactive(
      polygons = polygons,
      coastline = coastline,
      title = title,
      colors = colors,
      show_legend = show_legend,
      labels = zone_labels,
      base_layers = base_layers,
      minimap = minimap
    ))
  }
}

#' Plot juvenile analysis
#'
#' @description
#' Creates comprehensive visualizations for juvenile fish analysis including 
#' bar charts, line plots, and comparative analyses. This function consolidates
#' all juvenile plotting functionality.
#'
#' @param data Data frame with juvenile analysis data.
#' @param x_var Column name for x-axis variable.
#' @param fill_var Column name for fill/color variable.
#' @param length_cols Vector of length frequency column names.
#' @param a Length-weight relationship coefficient.
#' @param b Length-weight relationship exponent.
#' @param x_date_breaks Date breaks for x-axis (e.g., "1 day", "1 month").
#' @param plot_type Plot type: "bars", "lines", "points", "mixed".
#' @param title Plot title.
#' @param subtitle Plot subtitle.
#' @param sort_by Sorting method: "x", "number", "weight".
#' @param color_palette Custom color palette.
#' @param facet_var Variable for faceting.
#' @param facet_cols Number of facet columns.
#' @param bar_position Bar position: "dodge", "stack", "fill".
#' @param y_limits Y-axis limits.
#' @param use_facet_wrap Use facet wrap for juvenile type.
#' @param group_by_type Group by juvenile type when not faceting.
#' @param reference_line Reference line value (e.g., legal limit).
#' @param theme_style Theme style: "classic", "minimal", "light", "dark".
#' @param legend_position Legend position.
#' @param rotate_x_labels Rotate x-axis labels.
#' @param na_to_zero Convert NA values to zeros.
#'
#' @return ggplot object.
#'
#' @examples
#' \dontrun{
#' plot_juvenile_analysis(
#'   data = fishery_data,
#'   x_var = "date",
#'   length_cols = paste0("length_", seq(8, 15, 0.5))
#' )
#' 
#' plot_juvenile_analysis(
#'   data = fishery_data,
#'   x_var = "date",
#'   fill_var = "vessel",
#'   length_cols = length_columns,
#'   plot_type = "mixed",
#'   reference_line = 10,
#'   title = "Juvenile Analysis by Vessel and Date"
#' )
#' }
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_bar geom_line geom_point facet_wrap
#' @importFrom ggplot2 scale_fill_manual scale_color_manual labs theme_minimal
#' @importFrom dplyr arrange mutate
#' @importFrom tidyr pivot_longer replace_na complete
#' @importFrom rlang syms sym
plot_juvenile_analysis <- function(data, 
                                   x_var, 
                                   fill_var = NULL, 
                                   length_cols = NULL,
                                   a = 0.0012, 
                                   b = 3.1242, 
                                   x_date_breaks = NULL,
                                   plot_type = "bars", 
                                   title = NULL, 
                                   subtitle = NULL, 
                                   sort_by = "x",
                                   color_palette = NULL, 
                                   facet_var = NULL, 
                                   facet_cols = 2,
                                   bar_position = "dodge", 
                                   y_limits = c(0, 100),
                                   use_facet_wrap = TRUE, 
                                   group_by_type = TRUE,
                                   reference_line = NULL, 
                                   theme_style = "light",
                                   legend_position = "bottom", 
                                   rotate_x_labels = TRUE,
                                   na_to_zero = FALSE) {
  
  if (!is.data.frame(data)) {
    stop("Data must be a data.frame")
  }

  if (nrow(data) == 0) {
  stop("Cannot plot: 'data' is empty.")
  }
  
  if (!x_var %in% colnames(data)) {
    stop("X variable '", x_var, "' not found in data")
  }
  
  if (!is.null(fill_var) && !fill_var %in% colnames(data)) {
    stop("Fill variable '", fill_var, "' not found in data")
  }
  
  if (!is.null(facet_var) && !facet_var %in% colnames(data)) {
    stop("Facet variable '", facet_var, "' not found in data")
  }
  
  if (!plot_type %in% c("bars", "lines", "points", "mixed")) {
    stop("Plot type must be one of: 'bars', 'lines', 'points', 'mixed'")
  }
  
  if (is.null(length_cols)) {
    stop("Length columns must be provided")
  }
  
  if (!all(length_cols %in% colnames(data))) {
    stop("Some length columns not found in data")
  }
  
  group_cols <- x_var
  if (!is.null(fill_var)) {
    group_cols <- c(group_cols, fill_var)
  }
  if (!is.null(facet_var) && !(facet_var %in% group_cols)) {
    group_cols <- c(group_cols, facet_var)
  }
  
  juvenile_data <- summarize_juveniles_by_group(
    data = data,
    group_cols = group_cols,
    length_cols = length_cols,
    a = a,
    b = b
  )
  
  data_long <- juvenile_data %>%
    tidyr::pivot_longer(
      cols = c("perc_juv_number", "perc_juv_weight"),
      names_to = "type",
      values_to = "percentage"
    ) %>%
    dplyr::mutate(
      type = factor(
        .data[["type"]],
        levels = c("perc_juv_number", "perc_juv_weight"),
        labels = c("By number", "By weight")
      )
    )
  
  if (!is.null(fill_var) || !is.null(facet_var)) {
    group_vars <- c(x_var, "type")
    if (!is.null(fill_var)) {
      group_vars <- c(group_vars, fill_var)
    }
    if (!is.null(facet_var) && !(facet_var %in% group_vars)) {
      group_vars <- c(group_vars, facet_var)
    }
    
    data_long <- data_long %>%
      tidyr::complete(!!!rlang::syms(group_vars),
                      fill = list(percentage = NA_real_))
    
    if (na_to_zero) {
      data_long <- data_long %>%
        dplyr::mutate(percentage = tidyr::replace_na(.data$percentage, 0))
    }
  }
  
  if (is.character(data_long[[x_var]]) &&
      (grepl("fecha|date", x_var, ignore.case = TRUE))) {
    if (any(grepl("-|/", data_long[[x_var]]))) {
      converted_date <- convert_to_date(data_long[[x_var]], output_type = "date")
      if (!is.null(converted_date)) {
        data_long[[x_var]] <- converted_date
      }
    }
  }
  
  if (sort_by == "number") {
    order <- juvenile_data %>%
      dplyr::arrange(dplyr::desc(.data[["perc_juv_number"]])) %>%
      dplyr::pull(!!rlang::sym(x_var))
    if (!is.numeric(data_long[[x_var]]) && !inherits(data_long[[x_var]], "Date")) {
      data_long[[x_var]] <- factor(data_long[[x_var]], levels = unique(order))
    }
  } else if (sort_by == "weight") {
    order <- juvenile_data %>%
      dplyr::arrange(dplyr::desc(.data[["perc_juv_weight"]])) %>%
      dplyr::pull(!!rlang::sym(x_var))
    if (!is.numeric(data_long[[x_var]]) && !inherits(data_long[[x_var]], "Date")) {
      data_long[[x_var]] <- factor(data_long[[x_var]], levels = unique(order))
    }
  }
  
  p <- create_juvenile_base_plot(data_long, x_var, fill_var, use_facet_wrap, group_by_type)
  p <- add_juvenile_geoms(p, plot_type, bar_position, fill_var, use_facet_wrap, group_by_type)
  p <- add_juvenile_faceting(p, facet_var, use_facet_wrap, facet_cols, fill_var)
  p <- customize_juvenile_axes(p, data_long, x_var, x_date_breaks, y_limits)
  
  if (!is.null(color_palette)) {
    p <- p + ggplot2::scale_fill_manual(values = color_palette)
    if (plot_type %in% c("lines", "points", "mixed")) {
      p <- p + ggplot2::scale_color_manual(values = color_palette)
    }
  }
  
  if (is.null(title)) {
    title <- paste("Juvenile percentage by", x_var)
  }
  
  p <- p + ggplot2::labs(
    title = title,
    subtitle = subtitle,
    y = "Juvenile percentage (%)",
    x = x_var,
    fill = if (is.null(fill_var)) "Type" else fill_var,
    color = if (plot_type %in% c("bars") || is.null(fill_var)) NULL else fill_var
  )
  
  p <- apply_juvenile_theme(p, theme_style, legend_position, rotate_x_labels)
  
  if (!is.null(reference_line)) {
    p <- add_juvenile_reference_line(p, reference_line, data_long, x_var)
  }
  
  return(p)
}

#' Create fishery dashboard
#'
#' @description
#' Generates a comprehensive dashboard for fishery analysis including juvenile analysis,
#' catch trends, spatial distribution, and summary statistics. This function consolidates
#' all dashboard functionality.
#'
#' @param data Data frame with complete fishery data.
#' @param date_col Date column name. If NULL, auto-detect.
#' @param length_cols Length frequency column names. If NULL, auto-detect.
#' @param a Length-weight coefficient.
#' @param b Length-weight exponent.
#' @param latitude_col Latitude column name. If NULL, auto-detect.
#' @param longitude_col Longitude column name. If NULL, auto-detect.
#' @param catch_col Catch column name. If NULL, auto-detect.
#' @param juvenile_col Juvenile percentage column name. If NULL, auto-detect.
#' @param map_xlim Map longitude limits.
#' @param map_ylim Map latitude limits.
#' @param color_palette Custom color palette.
#' @param date_breaks Date axis breaks.
#' @param sort_comparison Sort comparison plot.
#' @param comparison_title Comparison plot title.
#' @param catch_title Catch plot title.
#' @param map_title Map plot title.
#' @param trend_title Trend plot title.
#'
#' @return List with individual plots and combined dashboard (if patchwork available).
#'
#' @examples
#' \dontrun{
#' dashboard <- create_fishery_dashboard(
#'   data = complete_fishery_data,
#'   date_col = "date",
#'   length_cols = paste0("length_", seq(8, 15, 0.5)),
#'   catch_col = "total_catch",
#'   latitude_col = "latitude",
#'   longitude_col = "longitude"
#' )
#' 
#' dashboard$comparison
#' dashboard$catch_trends
#' dashboard$spatial_map
#' dashboard$trends
#' dashboard$dashboard
#' }
#'
#' @export
#' @importFrom dplyr group_by reframe arrange mutate filter starts_with all_of
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_area geom_line geom_point geom_sf labs theme_minimal
#' @importFrom ggplot2 scale_color_gradient2 scale_size_continuous coord_sf geom_smooth
#' @importFrom ggplot2 facet_wrap scale_color_manual scale_y_continuous theme element_text
#' @importFrom scales comma
#' @importFrom stats as.formula
create_fishery_dashboard <- function(data,
                                     date_col = NULL,
                                     length_cols = NULL,
                                     a = 0.0001,
                                     b = 2.984,
                                     latitude_col = NULL,
                                     longitude_col = NULL,
                                     catch_col = NULL,
                                     juvenile_col = NULL,
                                     map_xlim = c(-85, -70),
                                     map_ylim = c(-20, 0),
                                     color_palette = NULL,
                                     date_breaks = NULL,
                                     sort_comparison = FALSE,
                                     comparison_title = NULL,
                                     catch_title = NULL,
                                     map_title = NULL,
                                     trend_title = NULL) {
  
  if (!is.data.frame(data)) {
    stop("data must be a data.frame")
  }
  
  if (is.null(date_col)) {
    date_patterns <- c("fecha", "date", "dia", "day")
    date_idx <- find_column(date_patterns, colnames(data))
    if (!is.null(date_idx)) {
      date_col <- colnames(data)[date_idx]
    } else {
      stop("Date column not found. Please specify 'date_col' parameter.")
    }
  }
  
  if (is.null(length_cols)) {
    length_cols <- suppressWarnings(find_columns_by_pattern(data, pattern = "weighted_", sort = TRUE))
    if (length(length_cols) == 0) {
      length_cols <- suppressWarnings(find_columns_by_pattern(data, pattern = "pond_", sort = TRUE))
    }
    if (length(length_cols) == 0) {
      length_cols <- suppressWarnings(find_columns_by_pattern(data, pattern = "length_", sort = TRUE))
    }
    if (length(length_cols) == 0) {
      numeric_cols <- colnames(data)[grep("^[0-9]+(\\.[0-9]+)?$", colnames(data))]
      if (length(numeric_cols) > 0) {
        length_cols <- numeric_cols[order(safe_numeric_conversion(numeric_cols))]
      }
    }
    if (length(length_cols) == 0) {
      stop("Length columns not found. Please specify 'length_cols' parameter.")
    }
  }
  
  if (is.null(latitude_col)) {
    lat_patterns <- c("lat", "latitude", "latitud")
    lat_idx <- find_column(lat_patterns, colnames(data))
    if (!is.null(lat_idx)) {
      latitude_col <- colnames(data)[lat_idx]
    }
  }
  
  if (is.null(longitude_col)) {
    lon_patterns <- c("lon", "longitude", "longitud")
    lon_idx <- find_column(lon_patterns, colnames(data))
    if (!is.null(lon_idx)) {
      longitude_col <- colnames(data)[lon_idx]
    }
  }
  
  if (is.null(catch_col)) {
    catch_patterns <- c("catch", "captura", "desembar", "peso", "weight")
    catch_idx <- find_column(catch_patterns, colnames(data))
    if (!is.null(catch_idx)) {
      catch_col <- colnames(data)[catch_idx]
    }
  }
  
  if (is.null(juvenile_col)) {
    juv_patterns <- c("juv", "perc_juv", "juvenil", "juvenile")
    juv_idx <- find_column(juv_patterns, colnames(data))
    if (!is.null(juv_idx)) {
      juvenile_col <- colnames(data)[juv_idx]
    }
  }
  
  if (is.null(date_breaks)) {
    date_breaks <- "1 day"
  }
  
  if (is.null(color_palette)) {
    color_palette <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD",
                       "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF")
  }
  
  juvenile_data <- summarize_juveniles_by_group(
    data = data,
    group_cols = date_col,
    length_cols = length_cols,
    a = a,
    b = b
  )
  
  if (is.null(comparison_title)) {
    comparison_title <- "Juvenile Analysis: Number vs Weight"
  }
  if (is.null(catch_title)) {
    catch_title <- "Cumulative Catch Over Time"
  }
  if (is.null(map_title)) {
    map_title <- "Spatial Distribution of Juveniles"
  }
  if (is.null(trend_title)) {
    trend_title <- "Juvenile Percentage Trends"
  }
  
  p1 <- plot_juvenile_analysis(
    data = data,
    x_var = date_col,
    plot_type = "bars",
    length_cols = length_cols,
    a = a,
    b = b,
    x_date_breaks = date_breaks,
    title = comparison_title,
    sort_by = if(sort_comparison) "weight" else "x",
    color_palette = c("#3498DB", "#E74C3C")
  )
  
  catch_data_cumulative <- juvenile_data %>%
    dplyr::arrange(.data[[date_col]]) %>%
    dplyr::mutate(
      cumulative_total_weight = cumsum(.data[["total_weight"]]),
      cumulative_juvenile_weight = cumsum(.data[["juvenil_weight"]])
    )
  
  plot_data <- catch_data_cumulative %>%
    dplyr::select(dplyr::all_of(c(date_col, "cumulative_total_weight", "cumulative_juvenile_weight"))) %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("cumulative"),
      names_to = "type",
      values_to = "weight"
    ) %>%
    dplyr::mutate(
      type = dplyr::case_when(
        .data$type == "cumulative_total_weight" ~ "Total weight",
        .data$type == "cumulative_juvenile_weight" ~ "Juvenile weight",
        TRUE ~ .data$type
      )
    )
  
  p2 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[date_col]], y = .data$weight, 
                                                 fill = .data$type, color = .data$type)) +
    ggplot2::geom_area(alpha = 0.3, position = "identity") +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(
      title = catch_title,
      x = "Date",
      y = "Cumulative catch (t)",
      fill = "Type",
      color = "Type"
    ) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_date(date_breaks = date_breaks, date_labels = "%d %b %Y") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  p3 <- NULL
  if (!is.null(latitude_col) && !is.null(longitude_col) &&
      latitude_col %in% colnames(data) && longitude_col %in% colnames(data)) {
    
    map_data <- data %>%
      dplyr::group_by(.data[[longitude_col]], .data[[latitude_col]]) %>%
      dplyr::filter(!is.na(.data[[longitude_col]]) & !is.na(.data[[latitude_col]]))
    
    if (requireNamespace("rnaturalearth", quietly = TRUE)) {
      base_map <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
      
      p3 <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = base_map, fill = "lightgrey", color = "white") +
        ggplot2::geom_point(
          data = map_data,
          ggplot2::aes(
            x = .data[[longitude_col]],
            y = .data[[latitude_col]],
            size = .data[[catch_col]],
            color = .data[[juvenile_col]]
          ),
          alpha = 0.7
        ) +
        ggplot2::scale_color_gradient2(
          low = "#3498DB",
          mid = "#FFCC00",
          high = "#E74C3C",
          midpoint = 10,
          name = "% Juveniles"
        ) +
        ggplot2::scale_size_continuous(
          range = c(2, 8),
          name = "Catch (t)"
        ) +
        ggplot2::coord_sf(
          xlim = map_xlim,
          ylim = map_ylim
        ) +
        ggplot2::labs(title = map_title) +
        ggplot2::theme_minimal()
    } else {
      warning("rnaturalearth package not available. Skipping spatial map.")
    }
  }
  
  data_long <- juvenile_data %>%
    tidyr::pivot_longer(
      cols = c("perc_juv_number", "perc_juv_weight"),
      names_to = "type",
      values_to = "percentage"
    ) %>%
    dplyr::mutate(
      type = factor(
        .data[["type"]],
        levels = c("perc_juv_number", "perc_juv_weight"),
        labels = c("By number", "By weight")
      )
    )
  
  p4 <- ggplot2::ggplot(data_long, ggplot2::aes(x = .data[[date_col]], y = .data[["percentage"]], 
                                                 color = .data[["type"]])) +
    ggplot2::geom_point(size = 3, alpha = 0.7) +
    ggplot2::facet_wrap(stats::as.formula(paste("~", "type")), scales = "free_x") +
    ggplot2::geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +
    ggplot2::labs(
      title = trend_title,
      x = "Date",
      y = "Juvenile percentage (%)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c("#3498DB", "#E74C3C")) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  result <- list(
    comparison = p1,
    catch_trends = p2,
    spatial_map = p3,
    trends = p4
  )
  
  if (requireNamespace("patchwork", quietly = TRUE)) {
    plots <- list(p1, p2, p3, p4)
    plots <- plots[!sapply(plots, is.null)]
    
    result$dashboard <- patchwork::wrap_plots(
      plots,
      ncol = 2,
      widths = rep(1, length(plots))
    )
  }
  
  return(result)
}