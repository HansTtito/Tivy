#' Plot fishing suspension zone polygons with ggplot2
#'
#' @description
#' Creates static visualizations of polygons for fishing suspension zones
#' using ggplot2. Returns a ggplot2 object that can be modified later.
#'
#' @param data A data frame with latitude and longitude coordinates for start and end points,
#'        which can be in text format (like "12°30'S") or decimal degrees.
#' @param coastline `data.frame` with coastline coordinates. Must have columns named `Long` and `Lat`. If `NULL` (default), uses internal dataset `Tivy::peru_coastline`.
#' @param parallels A list containing data frames of lines parallel to the coast. If `NULL` (default), uses internal dataset `Tivy::peru_coast_parallels`.
#' @param title Title for the plot. Default NULL
#' @param colors Vector of colors for the polygons. Default NULL
#' @param show_legend Logical. If TRUE, shows the legend. Default FALSE.
#' @param name_legend Name of the legend (optional). If NULL, legend has no title.
#' @param labels Vector of texts to label each polygon in the legend.
#' @param add_grid Logical. If TRUE, adds a grid to the plot. Default FALSE.
#'
#' @return A ggplot2 object that can be modified with additional layers.
#' @import ggplot2
#'
#' @examples
#' # Create basic plot
#'
#' pdf_urls <- c(
#'   "https://consultasenlinea.produce.gob.pe/produce/descarga/comunicados/dgsfs/1542_comunicado1.pdf",
#'   "https://consultasenlinea.produce.gob.pe/produce/descarga/comunicados/dgsfs/1478_comunicado1.pdf",
#'   "https://consultasenlinea.produce.gob.pe/produce/descarga/comunicados/dgsfs/1468_comunicado1.pdf"
#' )
#' results <- extract_announcement_data(vector_pdf_names = pdf_urls)
#'
#' g <- plot_polygons_ggplot(data = results)
#'
#' # Customize later
#' g +
#'   ggplot2::xlim(-80, -70) +
#'   ggplot2::theme_minimal() +
#'   ggplot2::labs(title = "Custom title")
#'
#' @export
plot_polygons_ggplot <- function(data,
                                 coastline = NULL,
                                 parallels = NULL,
                                 title = NULL,
                                 colors = NULL,
                                 show_legend = FALSE,
                                 name_legend = NULL,
                                 labels = NULL,
                                 add_grid = FALSE) {

    # Load default coastline if NULL
  if (is.null(coastline)) {
    if (!requireNamespace("Tivy", quietly = TRUE)) {
      stop("Default coastline data (Tivy::peru_coastline) is not available. Please provide a coastline.")
    }
    coastline <- Tivy::peru_coastline
  }

  if (!is.data.frame(coastline)) stop("The 'coastline' parameter must be a data.frame.")

  # Load default parallels if NULL
  if (is.null(parallels)) {
    if (!requireNamespace("Tivy", quietly = TRUE)) {
      stop("Default parallels data (Tivy::peru_coast_parallels) is not available. Please provide a parallels coast data.")
    }
    parallels <- Tivy::peru_coast_parallels
  }

  if (!is.list(parallels)) stop("The 'parallels' parameter must be a list.")


  # Prepare the polygons (all the data processing is the same)
  polygons <- prepare_polygons(data = data, coastline = coastline, coast_parallels = parallels)

  # Create static visualization with ggplot2
  return(plot_static(
    polygons,
    coastline = coastline,
    title = title,
    colors = colors,
    show_legend = show_legend,
    name_legend = name_legend,
    labels = labels,
    add_grid = add_grid
  ))
}

#' Plot fishing suspension zone polygons with leaflet
#'
#' @description
#' Creates interactive visualizations of polygons for fishing suspension zones
#' using leaflet. Returns a leaflet object that can be modified later.
#'
#' @param data A data frame with latitude and longitude coordinates for start and end points,
#'        which can be in text format (like "12°30'S") or decimal degrees.
#' @param coastline `data.frame` with coastline coordinates. Must have columns named `Long` and `Lat`. If `NULL` (default), uses internal dataset `Tivy::peru_coastline`.
#' @param parallels A list containing data frames of lines parallel to the coast. If `NULL` (default), uses internal dataset `Tivy::peru_coast_parallels`.
#' @param title Title for the plot. Default NULL
#' @param colors Vector of colors for the polygons. Default NULL
#' @param show_legend Logical. If TRUE, shows the legend. Default FALSE.
#' @param labels Vector of texts to label each polygon in the legend.
#' @param base_layers Logical. If TRUE, adds multiple base maps. Default FALSE.
#' @param minimap Logical. If TRUE, adds a minimap to the interactive map. Default FALSE
#'
#' @return A leaflet object that can be modified with additional layers.
#' @import leaflet
#'
#' @examples
#' # Create basic map
#'
#' pdf_urls <- c(
#'   "https://consultasenlinea.produce.gob.pe/produce/descarga/comunicados/dgsfs/1542_comunicado1.pdf",
#'   "https://consultasenlinea.produce.gob.pe/produce/descarga/comunicados/dgsfs/1478_comunicado1.pdf",
#'   "https://consultasenlinea.produce.gob.pe/produce/descarga/comunicados/dgsfs/1468_comunicado1.pdf"
#' )
#'
#' results <- extract_announcement_data(vector_pdf_names = pdf_urls)
#'
#' m <- plot_polygons_leaflet(data = results)
#'
#' # Customize later
#' m |>
#'   leaflet::addMarkers(lng = -77.1, lat = -12.0, popup = "Lima") |>
#'   leaflet::addCircleMarkers(lng = -76.3, lat = -13.4, radius = 5, color = "red")
#'
#' @export
plot_polygons_leaflet <- function(data,
                                  coastline = NULL,
                                  parallels = NULL,
                                  title = NULL,
                                  colors = NULL,
                                  show_legend = FALSE,
                                  labels = NULL,
                                  base_layers = FALSE,
                                  minimap = FALSE) {

    # Load default coastline if NULL
  if (is.null(coastline)) {
    if (!requireNamespace("Tivy", quietly = TRUE)) {
      stop("Default coastline data (Tivy::peru_coastline) is not available. Please provide a coastline.")
    }
    coastline <- Tivy::peru_coastline
  }

  if (!is.data.frame(coastline)) stop("The 'coastline' parameter must be a data.frame.")

  # Load default parallels if NULL
  if (is.null(parallels)) {
    if (!requireNamespace("Tivy", quietly = TRUE)) {
      stop("Default parallels data (Tivy::peru_coast_parallels) is not available. Please provide a parallels coast data.")
    }
    parallels <- Tivy::peru_coast_parallels
  }

  if (!is.list(parallels)) stop("The 'parallels' parameter must be a list.")

  # Prepare the polygons (all the data processing is the same)
  polygons <- prepare_polygons(data, coastline, parallels)

  # Create interactive visualization with leaflet
  return(plot_interactive(
    polygons, coastline, title, colors,
    show_legend = show_legend,
    labels = labels,
    base_layers = base_layers,
    minimap = minimap
  ))
}


#' Plot fishing suspension polygons
#'
#' This function allows plotting fishing suspension polygons using
#' a base map of the Peruvian coast. Depending on the `type` parameter, it can
#' generate a static plot with `ggplot2` or an interactive plot with `leaflet`.
#'
#' @param data List of polygons with coordinates and metadata (such as announcement).
#' @param coastline `data.frame` with coastline coordinates. Must have columns named `Long` and `Lat`. If `NULL` (default), uses internal dataset `Tivy::peru_coastline`.
#' @param parallels A list containing data frames of lines parallel to the coast. If `NULL` (default), uses internal dataset `Tivy::peru_coast_parallels`.
#' @param type Type of plot to generate: `"static"` for ggplot2 or `"interactive"` for leaflet.
#' @param title Plot title.
#' @param colors Vector of colors to use to differentiate the polygons (by announcement or other category).
#' @param show_legend Logical. If `TRUE`, the legend is shown. Default is `FALSE`.
#' @param labels Optional vector of custom labels for each polygon. If not specified, the announcement name is used.
#' @param add_grid Logical. If `TRUE`, adds a reference grid to the static plot.
#' @param base_layers Logical. If `TRUE`, adds base layers (such as satellite or topography) to the interactive map.
#' @param minimap Logical. If `TRUE`, adds a minimap in the corner of the interactive map.
#'
#' @return An object of class `ggplot` if `type = "static"` or a `leaflet` object if `type = "interactive"`.
#'
#' @examples
#' # Create basic map
#'
#' pdf_urls <- c(
#'   "https://consultasenlinea.produce.gob.pe/produce/descarga/comunicados/dgsfs/1542_comunicado1.pdf",
#'   "https://consultasenlinea.produce.gob.pe/produce/descarga/comunicados/dgsfs/1478_comunicado1.pdf",
#'   "https://consultasenlinea.produce.gob.pe/produce/descarga/comunicados/dgsfs/1468_comunicado1.pdf"
#' )
#'
#' results <- extract_announcement_data(vector_pdf_names = pdf_urls)
#'
#' plot_polygons(data = results)
#' @export
plot_polygons <- function(data,
                          coastline = NULL,
                          parallels = NULL,
                          type = "static",
                          title = "Fishing suspension zones",
                          colors = NULL,
                          show_legend = FALSE,
                          labels = NULL,
                          add_grid = FALSE,
                          base_layers = FALSE,
                          minimap = FALSE) {

  if (type == "static") {
    return(plot_polygons_ggplot(
      data = data,
      coastline = coastline,
      parallels = parallels,
      title = title,
      colors = colors,
      show_legend = show_legend,
      labels = labels,
      add_grid = add_grid
    ))
  } else {
    return(plot_polygons_leaflet(
      data = data,
      coastline = coastline,
      parallels = parallels,
      title = title,
      colors = colors,
      show_legend = show_legend,
      labels = labels,
      base_layers = base_layers,
      minimap = minimap
    ))
  }
}



#' Plot juvenile percentages (by number and weight)
#'
#' Generates visualizations to explore juvenile percentages based on
#' a selected variable (e.g., date or vessel). The data should
#' come from the `juveniles_by_group` function, which summarizes the proportion of juveniles
#' by group according to length and catch.
#'
#' The function allows choosing between different chart types (bars, lines, points,
#' mixed or boxplots) and customizing options such as colors, sorting, and faceting.
#' It is also possible to add a reference line corresponding to the legal
#' juvenile limit.
#'
#' @param juvenile_data Data frame with results from `juveniles_by_group`.
#' @param var_x Name of the variable to use on the X axis of the plot (e.g., "unique_date").
#' @param fill_var (Optional) Variable to group and color the bars or lines (e.g., "vessel").
#' @param cols_length Vector of length columns to use for juvenile analysis.
#' @param a Parameter for the weight-length relationship (default 0.0001).
#' @param b Parameter for the weight-length relationship (default 2.984).
#' @param plot_type Type of plot to show: "bars" (default), "lines", "points", "mixed", or "boxplot".
#' @param title (Optional) Main title of the plot.
#' @param sort_by Criterion to sort the X axis: "x" (default, sort by var_x), "number", "weight" or "total".
#' @param palette (Optional) Vector of custom colors.
#' @param facet_var (Optional) Variable to divide the plot into subplots (facets).
#' @param ncol Number of columns for faceting (default 2).
#'
#' @return A `ggplot` object representing the generated plot. It can be customized or printed directly.
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_bar geom_line geom_point geom_boxplot
#'             facet_wrap scale_fill_manual scale_color_manual labs theme_minimal
#' @importFrom dplyr arrange mutate
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' # Data processing
#' data_hauls <- process_hauls(data_hauls = calas_bitacora)
#' data_fishing_trips <- process_fishing_trips(data_fishing_trips = faenas_bitacora)
#' hauls_length <- process_length(data_length = tallas_bitacora)
#'
#' data_length_fishing_trips <- merge(
#'    x = data_fishing_trips,
#'    y = hauls_length,
#'    by = "fishing_trip_code"
#' )
#'
#' data_total <- merge_length_fishing_trips_hauls(
#'    data_hauls = data_hauls,
#'    data_length_fishing_trips = data_length_fishing_trips
#' )
#'
#' final_data <- add_variables(data_total)
#' length_cols <- c("8", "8.5", "9", "9.5", "10", "10.5",
#'                  "11", "11.5", "12","12.5", "13",
#'                  "13.5", "14", "14.5", "15")
#'
#' results <- weight_length_df(df = final_data, length_cols = length_cols,
#'                                 catch_col = "catch_ANCHOVETA", a = 0.0001, b = 2.984)
#'
#' results$unique_date <- convert_to_date(results$start_date_haul, type = "date")
#'
#' juvenile_results <- juveniles_by_group(data = results,
#'                                            group_cols = c("unique_date"),
#'                                            cols_length = length_cols)
#'
#' # Bar chart of juvenile percentages by date
#' plot_juveniles(
#'   juvenile_data = juvenile_results,
#'   var_x = "unique_date"
#' )
plot_juveniles <- function(juvenile_data, var_x, fill_var = NULL, cols_length = NULL,
                           a = NULL, b = NULL,
                           plot_type = "bars", title = NULL, sort_by = "x",
                           palette = NULL, facet_var = NULL, ncol = 2,
                           position = "dodge", y_limits = c(0, 100)) {

  # Parameter validation
  if (!is.data.frame(juvenile_data))
    stop("juvenile_data must be a data.frame")

  if (!var_x %in% colnames(juvenile_data))
    stop(paste("The variable", var_x, "does not exist in the data.frame"))

  if (!is.null(fill_var) && !fill_var %in% colnames(juvenile_data))
    stop(paste("The variable", fill_var, "does not exist in the data.frame"))

  if (!is.null(facet_var) && !facet_var %in% colnames(juvenile_data))
    stop(paste("The variable", facet_var, "does not exist in the data.frame"))

  if (!plot_type %in% c("bars", "lines", "points", "mixed"))
    stop("plot_type must be one of: 'bars', 'lines', 'points' or 'mixed'")

  if (!sort_by %in% c("x", "number", "weight"))
    stop("sort_by must be one of: 'x', 'number', 'weight'")

  if (!position %in% c("dodge", "stack", "fill"))
    stop("position must be one of: 'dodge', 'stack', or 'fill'")

  if(!is.null(cols_length)){
    if(!all(cols_length %in% colnames(juvenile_data))){
      stop(paste("The columns", paste(cols_length, collapse = ", "), "do not exist in the data.frame"))
    }
  } else {
    stop("cols_length must be provided")
  }

  if(!is.null(a) && !is.null(b)){
    if(!is.numeric(a) || !is.numeric(b)){
      stop("a and b must be numeric")
    }
  } else {
    stop("a and b must be provided")
  }

  # Apply juveniles_by_group function to calculate percentages
  if(!is.null(fill_var)){
    juvenile_data <- juveniles_by_group(
      data = juvenile_data,
      group_cols = c(var_x, fill_var),
      cols_length = cols_length,
      a = a,
      b = b
    )
  } else {
    juvenile_data <- juveniles_by_group(
      data = juvenile_data,
      group_cols = c(var_x),
      cols_length = cols_length,
      a = a,
      b = b
    )
  }

  # Data preparation
  # Convert to long format to facilitate visualization
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

  # Remove NA percentages to avoid plotting issues
  data_long <- data_long %>%
    dplyr::filter(!is.na(.data[["percentage"]]))

  # Check if X variable is date type and convert if necessary
  if (is.character(data_long[[var_x]]) &&
      (grepl("fecha", var_x, ignore.case = TRUE) ||
       grepl("date", var_x, ignore.case = TRUE))) {
    # Try to convert to Date only if it looks like a date in text format
    if (any(grepl("-|/", data_long[[var_x]]))) {
      # Try several common date formats
      converted_date <- convert_to_date(data_long[[var_x]], type = "date")
      if (!is.null(converted_date)) {
        data_long[[var_x]] <- converted_date
      }
    }
  }

  # Sort data according to the sort_by parameter
  if (sort_by == "number") {
    order <- juvenile_data %>%
      dplyr::arrange(dplyr::desc(.data[["perc_juv_number"]])) %>%
      dplyr::pull(var_x)
    # Handle potential factor/character conversion
    if (!is.numeric(data_long[[var_x]]) && !inherits(data_long[[var_x]], "Date")) {
      data_long[[var_x]] <- factor(data_long[[var_x]], levels = unique(order))
    }
  } else if (sort_by == "weight") {
    order <- juvenile_data %>%
      dplyr::arrange(dplyr::desc(.data[["perc_juv_weight"]])) %>%
      dplyr::pull(var_x)
    # Handle potential factor/character conversion
    if (!is.numeric(data_long[[var_x]]) && !inherits(data_long[[var_x]], "Date")) {
      data_long[[var_x]] <- factor(data_long[[var_x]], levels = unique(order))
    }
  }

  # Create the plot with the appropriate mapping
  if (is.null(fill_var)) {
    # When no fill_var is provided, use 'type' as the fill
    p <- ggplot2::ggplot(
      data_long,
      ggplot2::aes(
        x = .data[[var_x]],
        y = .data[["percentage"]],
        fill = .data[["type"]]
      )
    )
    # For line and point plots, also use 'type' for grouping
    if (plot_type %in% c("lines", "points", "mixed")) {
      p <- p + ggplot2::aes(group = .data[["type"]], color = .data[["type"]])
    }
  } else {
    # When fill_var is provided, always facet by 'type' to distinguish between number and weight
    p <- ggplot2::ggplot(
      data_long,
      ggplot2::aes(
        x = .data[[var_x]],
        y = .data[["percentage"]],
        fill = .data[[fill_var]]
      )
    )
    # For line and point plots, also use fill_var for grouping and color
    if (plot_type %in% c("lines", "points", "mixed")) {
      p <- p + ggplot2::aes(group = .data[[fill_var]], color = .data[[fill_var]])
    }
    # Always facet by type when a fill variable is provided
    p <- p + ggplot2::facet_wrap(~ type, ncol = ncol)
  }

  # Add geoms according to plot type
  if (plot_type == "bars") {
    if (position == "dodge") {
      p <- p + ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(width = 0.9))
    } else {
      p <- p + ggplot2::geom_bar(stat = "identity", position = position)
    }

    # Add faceting if fill_var is NULL (otherwise it's already added)
    if (is.null(fill_var) && is.null(facet_var)) {
      p <- p + ggplot2::facet_wrap(~ type, ncol = ncol)
    }
  } else if (plot_type == "lines") {
    p <- p + ggplot2::geom_line(size = 1) +
      ggplot2::geom_point(size = 2)

    # Add faceting if fill_var is NULL and no facet_var (otherwise it's already added)
    if (is.null(fill_var) && is.null(facet_var)) {
      p <- p + ggplot2::facet_wrap(~ type, ncol = ncol)
    }
  } else if (plot_type == "points") {
    p <- p + ggplot2::geom_point(size = 3, alpha = 0.7)

    # Add faceting if fill_var is NULL and no facet_var (otherwise it's already added)
    if (is.null(fill_var) && is.null(facet_var)) {
      p <- p + ggplot2::facet_wrap(~ type, ncol = ncol)
    }
  } else if (plot_type == "mixed") {
    if (position == "dodge") {
      p <- p +
        ggplot2::geom_bar(stat = "identity", alpha = 0.7,
                          position = ggplot2::position_dodge(width = 0.9)) +
        ggplot2::geom_point(size = 2, position = ggplot2::position_dodge(width = 0.9))
    } else {
      p <- p +
        ggplot2::geom_bar(stat = "identity", alpha = 0.7, position = position) +
        ggplot2::geom_point(size = 2)
    }

    # Add faceting if fill_var is NULL and no facet_var (otherwise it's already added)
    if (is.null(fill_var) && is.null(facet_var)) {
      p <- p + ggplot2::facet_wrap(~ type, ncol = ncol)
    }
  }

  # Add additional faceting if provided
  if (!is.null(facet_var)) {
    # Create facet formula with appropriate variables
    if (is.null(fill_var)) {
      # When no fill_var, facet by type and facet_var
      facet_formula <- stats::as.formula(paste("~ type +", facet_var))
    } else {
      # When fill_var is present, we already facet by type, so just add facet_var
      facet_formula <- stats::as.formula(paste("~ type +", facet_var))
    }
    p <- p + ggplot2::facet_wrap(facet_formula, ncol = ncol)
  }

  # Set y-axis limits
  if (!is.null(y_limits)) {
    p <- p + ggplot2::ylim(y_limits)
  }

  # Add color palette if provided
  if (!is.null(palette)) {
    p <- p + ggplot2::scale_fill_manual(values = palette)

    # Add color scale for line and point plots
    if (plot_type %in% c("lines", "points", "mixed")) {
      p <- p + ggplot2::scale_color_manual(values = palette)
    }
  }

  # Add title and labels
  if (is.null(title)) {
    title <- paste("Juvenile percentage by", var_x)
  }

  p <- p + ggplot2::labs(
    title = title,
    y = "Juvenile percentage (%)",
    x = var_x,
    fill = if (is.null(fill_var)) "Type" else fill_var,
    color = if (plot_type %in% c("bars") || is.null(fill_var)) NULL else fill_var
  ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "lightgray", color = NA),
      strip.text = ggplot2::element_text(face = "bold")
    )

  return(p)
}


#' Create visual dashboard for juvenile analysis
#'
#' Generates a set of visualizations in dashboard format to analyze the proportion
#' of juveniles in fish catches. The dashboard includes:
#' \itemize{
#'   \item A comparison of juveniles by number and weight
#'   \item A cumulative catch graph
#'   \item A map of spatial distribution of juveniles
#'   \item A visualization of juvenile percentage trends
#' }
#'
#' @param data_total Data frame with catch, size, and location data.
#'   It must contain information about dates, length, coordinates, and catches.
#' @param col_date Name of the column containing dates. If NULL,
#'   the function will try to detect it automatically.
#' @param cols_length Vector with names of columns containing size
#'   frequencies. If NULL, the function will try to detect them automatically.
#' @param a Parameter 'a' of the length-weight relationship (W = a*L^b). The default value
#'   is 0.0001.
#' @param b Parameter 'b' of the length-weight relationship (W = a*L^b). The default value
#'   is 2.984.
#' @param col_latitude Name of the column containing latitude. If NULL,
#'   the function will try to detect it automatically.
#' @param col_longitude Name of the column containing longitude. If NULL,
#'   the function will try to detect it automatically.
#' @param col_catch Name of the column containing catches. If NULL,
#'   the function will try to detect it automatically.
#' @param col_juveniles Name of the column containing the percentage of juveniles.
#'   If NULL, the function will try to detect it automatically.
#' @param xlim Numeric vector of length 2 defining the limits of the x-axis for the map.
#'   Default c(-85, -70) to cover the Peruvian coast.
#' @param ylim Numeric vector of length 2 defining the limits of the y-axis for the map.
#'   Default c(-20, 0) to cover the Peruvian coast.
#' @param show_limit_juv Logical. If TRUE, a reference line for the
#'   juvenile limit will be shown in the appropriate graphs.
#' @param palette Custom color vector for the graphs. If NULL,
#'   a default palette will be used.
#' @param sort_comparison Logical. If TRUE, the percentage comparison chart
#'   will be sorted from highest to lowest. The default value is FALSE.
#' @param title_comp Title for the juvenile comparison graph.
#' @param title_catch Title for the cumulative catch graph.
#' @param title_map Title for the spatial distribution map.
#' @param title_relation Title for the smoothed juvenile percentage graph.
#'
#' @return A list with the following components:
#'   \item{comparison}{Bar chart comparing juveniles by number and weight}
#'   \item{cumulative_catch}{Area chart showing cumulative catch over time}
#'   \item{juveniles_map}{Map showing the spatial distribution of juveniles}
#'   \item{relation}{Chart of smoothed juvenile percentage trends}
#'   \item{dashboard}{If the patchwork package is installed, a combined dashboard of all charts}
#'
#' @export
#' @importFrom dplyr group_by reframe arrange mutate filter starts_with %>%
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_area geom_line geom_point geom_sf labs theme_minimal scale_color_gradient2 scale_size_continuous coord_sf geom_smooth facet_wrap scale_color_manual scale_y_continuous
#' @importFrom scales comma
#' @importFrom rnaturalearth ne_countries
#' @importFrom patchwork wrap_plots
#' @importFrom stats as.formula
#'
#' @examples
#' \dontrun{
#'
#' data_hauls <- process_hauls(data_hauls = calas_bitacora)
#' data_fishing_trips <- process_fishing_trips(data_fishing_trips = faenas_bitacora)
#' hauls_length <- process_length(data_length = tallas_bitacora)
#'
#' # Integrate data
#' data_length_fishing_trips <- merge(
#'    x = data_fishing_trips,
#'    y = hauls_length,
#'    by = 'fishing_trip_code'
#' )
#'
#' data_total <- merge_length_fishing_trips_hauls(
#'    data_hauls = data_hauls,
#'    data_length_fishing_trips = data_length_fishing_trips
#' )
#'
#' final_data <- add_variables(data_total)
#'
#' # Prepare size data
#' length_cols <- c("8", "8.5", "9", "9.5", "10", "10.5", "11", "11.5",
#'                 "12", "12.5", "13", "13.5", "14", "14.5", "15")
#'
#' # Weight length by catch
#' results <- weight_length_df(
#'   df = final_data,
#'   length_cols = length_cols,
#'   catch_col = "catch_ANCHOVETA",
#'   a = 0.0001,
#'   b = 2.984
#' )
#'
#' # Prepare data for analysis
#' results$unique_date <- convert_to_date(results$start_date_haul, type = "date")
#' results$catch_t <- results$catch_ANCHOVETA/1000  # Convert to tons
#'
#' # Create dashboard
#' dashboard <- juveniles_dashboard(
#'   data_total = results,
#'   col_date = "unique_date",
#'   cols_length = paste0("pond_", seq(8, 15, 0.5)),
#'   juv_limit = 12,
#'   a = 0.0001,
#'   b = 2.984,
#'   col_latitude = "lat_final",
#'   col_longitude = "lon_final",
#'   col_catch = "catch_t",
#'   col_juveniles = "juv",
#'   show_limit_juv = TRUE
#' )
#'
#' # View individual components
#' dashboard$comparison
#' dashboard$cumulative_catch
#' dashboard$juveniles_map
#' dashboard$relation
#'
#' # View complete dashboard
#' dashboard$dashboard
#'}
juveniles_dashboard <- function(
    data_total,
    col_date = NULL,
    cols_length = NULL,
    juv_limit = 12,
    a = 0.0001,
    b = 2.984,
    col_latitude = NULL,
    col_longitude = NULL,
    col_catch = NULL,
    col_juveniles = NULL,
    xlim = c(-85, -70),
    ylim = c(-20, 0),
    show_limit_juv = TRUE,
    palette = NULL,
    sort_comparison = FALSE,
    title_comp = NULL,
    title_catch = NULL,
    title_map = NULL,
    title_relation = NULL
) {

# Validate data
if (!is.data.frame(data_total)) {
  stop("data_total must be a data.frame")
}

  # Search and validate of columns
  col_date       <- get_or_detect_columns(data_total, col_date,       "fecha|date",                   "date")
  cols_length    <- get_or_detect_columns(data_total, cols_length,    "^talla|^len|^pond_|cm$",       "size (length)")
  col_latitude   <- get_or_detect_columns(data_total, col_latitude,   "^lat|^latitude",               "latitude")
  col_longitude  <- get_or_detect_columns(data_total, col_longitude,  "^lon|^longitude",              "longitude")
  col_catch      <- get_or_detect_columns(data_total, col_catch,      "catch|captura|desembar",       "catch")
  col_juveniles  <- get_or_detect_columns(data_total, col_juveniles,  "juv|perc_juv",                 "juveniles")

  # Create default palette if not provided
  if (is.null(palette)) {
    palette <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD",
                "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF")
  }

  # Calculate juveniles by group
  juvenile_data <- juveniles_by_group(
    data = data_total,
    group_cols = col_date,
    cols_length = cols_length,
    juvLim = juv_limit,
    a = a,
    b = b
  )

  # Default titles if not specified
  if (is.null(title_comp))
    title_comp <- paste("Comparison of juveniles by number and weight")

  if (is.null(title_catch))
    title_catch <- "Cumulative anchovy catch"

  if (is.null(title_map))
    title_map <- "Spatial distribution of juveniles"

  if (is.null(title_relation))
    title_relation <- "Smoothed juvenile percentage"

  # 1. Comparison plot by date
  p1 <- plot_juveniles(
    juvenile_data = juvenile_data,
    var_x = col_date,
    plot_type = "bars",
    title = title_comp,
    sort_by = if(sort_comparison) "weight" else "x",
    palette = c("#3498DB", "#E74C3C")
  )

  # 2. Cumulative catch plot

  catch_data_acumulative <- juvenile_data %>%
  dplyr::arrange(.data$unique_date) %>%
  dplyr::mutate(
    cumulative_total_weight = cumsum(.data[["total_weight"]]),
    cumulative_juvenil_weight = cumsum(.data[["juvenil_weight"]])
  )

  # Create cumulative catch plot
  plot_data <- catch_data_acumulative %>%
    dplyr::select(.data[["unique_date"]], .data[["cumulative_total_weight"]], .data[["cumulative_juvenil_weight"]]) %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("cumulative"),
      names_to = "type",
      values_to = "weight"
    ) %>%
    dplyr::mutate(
      type = dplyr::case_when(
        type == "cumulative_total_weight" ~ "Total weight",
        type == "cumulative_juvenil_weight" ~ "Juvenile weight"
      )
    )

  # Graficar
  p2 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$unique_date, y = .data$weight, fill = .data$type, color = .data$type)) +
    ggplot2::geom_area(alpha = 0.3, position = "identity") +
    ggplot2::geom_line(size = 1.2) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(
      title = "Cumulative total and juvenile catch over time",
      x = "Date",
      y = "Cumulative catch (t)",
      fill = "Type",
      color = "Type"
    ) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::theme_minimal()


  # 3. Juveniles map
  p3 <- NULL
  if (!is.null(col_latitude) && !is.null(col_longitude) &&
      col_latitude %in% colnames(data_total) && col_longitude %in% colnames(data_total)) {

    # Prepare data for the map
    map_data <- data_total %>%
      dplyr::group_by(.data[[col_longitude]], .data[[col_latitude]]) %>%
      dplyr::filter(!is.na(.data[[col_longitude]]) & !is.na(.data[[col_latitude]]))

    # Create base map
    peru_map <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
    # Create juveniles map
    p3 <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = peru_map, fill = "lightgrey", color = "white") +
      ggplot2::geom_point(
        data = map_data,
        ggplot2::aes(
          x = .data[[col_longitude]],
          y = .data[[col_latitude]],
          size = .data[[col_catch]],
          col = .data[[col_juveniles]]
        ),
        alpha = 0.3
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
        xlim = xlim,  # Adjust according to Peruvian coast
        ylim = ylim    # Adjust according to Peruvian coast
      ) +
      ggplot2::labs(
        title = title_map,
        subtitle = paste("Juvenile limit:", juv_limit, "cm")
      ) +
      ggplot2::theme_minimal()
  }

  # 4. Scatter plot Total vs Percentage
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
      ),
      total = ifelse(.data$type == "By number", .data$total_number, .data$total_weight)
    )

  p4 <- ggplot2::ggplot(data_long, ggplot2::aes(x = .data[[col_date]], y = .data[["percentage"]], color = .data[["type"]])) +
    ggplot2::geom_point(size = 3, alpha = 0.7) +
    ggplot2::facet_wrap(stats::as.formula(paste("~", "type")), scales = "free_x") +
    ggplot2::geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +
    ggplot2::labs(
      title = title_relation,
      y = "Juvenile percentage (%)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c("#3498DB", "#E74C3C"))

  # Return the list of plots
  result <- list(
    comparison = p1,
    cumulative_catch = p2,
    juveniles_map = p3,
    relation = p4
  )

  # If patchwork is available, create a combined dashboard
  if (requireNamespace("patchwork", quietly = TRUE)) {
    plots <- list(p1, p2, p3, p4)
    # Filter out NULL plots
    plots <- plots[!sapply(plots, is.null)]
    # Combine plots
    # Use:
    result$dashboard <- patchwork::wrap_plots(
      plots,
      ncol = 2,
      widths = rep(1, length(plots))  # Force same width
    )
  }

  return(result)
}
