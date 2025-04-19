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
#' @param df Data frame containing juvenile data
#' @param var_x Name of the variable for the X axis
#' @param fill_var Optional. Name of the variable for fill color
#' @param cols_length Vector of column names containing length frequencies
#' @param a Parameter 'a' for the length-weight relationship
#' @param b Parameter 'b' for the length-weight relationship
#' @param step_x_date Optional. Interval for date breaks (e.g., "1 day", "1 month")
#' @param plot_type Type of chart: "bars", "lines", "points", or "mixed"
#' @param title Optional. Chart title
#' @param subtitle Optional. Chart subtitle
#' @param sort_by How to sort data: "x", "number", or "weight"
#' @param palette Optional. Vector of colors for the chart
#' @param facet_var Optional. Name of variable for additional faceting
#' @param ncol Number of columns in the facet grid
#' @param position Position adjustment for bars: "dodge", "stack", or "fill"
#' @param y_limits Optional. Numeric vector with min and max for Y axis
#' @param use_facet_wrap Logical. Whether to use facet_wrap to separate by type
#' @param group_by_type Logical. When use_facet_wrap=FALSE, whether to use transparency to distinguish by type
#' @param reference_line Optional. Value for reference line (e.g., legal limit)
#' @param theme_type Type of theme: "classic", "minimal", "light", or "dark"
#' @param legend_position Legend position: "bottom", "right", "left", or "top"
#' @param rotate_x_labels Logical. Whether to rotate X axis labels
#' @param na_to_zero Logical. Whether to convert NA values to zeros (to complete categories)
#'
#' @return A `ggplot` object representing the generated plot. It can be customized or printed directly.
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_bar geom_line geom_point geom_boxplot
#'             facet_wrap scale_fill_manual scale_color_manual labs theme_minimal
#' @importFrom dplyr arrange mutate
#' @importFrom tidyr pivot_longer
#' @importFrom rlang syms
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
#'
#' # Bar chart of juvenile percentages by date
#' plot_juveniles(
#'   df = results,
#'   var_x = "unique_date",
#'   cols_length = paste0("pond_", length_cols),
#'   a = 0.0001,
#'   b = 2.984
#' )
plot_juveniles <- function(df, var_x, fill_var = NULL, cols_length = NULL,
                           a = 0.0012, b = 3.1242, step_x_date = NULL,
                           plot_type = "bars", title = NULL, subtitle = NULL, sort_by = "x",
                           palette = NULL, facet_var = NULL, ncol = 2,
                           position = "dodge", y_limits = c(0, 100),
                           use_facet_wrap = TRUE, group_by_type = TRUE,
                           reference_line = NULL, theme_type = "light",
                           legend_position = "bottom", rotate_x_labels = TRUE,
                           na_to_zero = FALSE) {

  # validations
  if (!is.data.frame(df))
    stop("df must be a data.frame")

  if (!var_x %in% colnames(df))
    stop(paste("Variable", var_x, "doesn't exist in the data.frame"))

  if (!is.null(fill_var) && !fill_var %in% colnames(df))
    stop(paste("Variable", fill_var, "doesn't exist in the data.frame"))

  if (!is.null(facet_var) && !facet_var %in% colnames(df))
    stop(paste("Variable", facet_var, "doesn't exist in the data.frame"))

  if (!plot_type %in% c("bars", "lines", "points", "mixed"))
    stop("plot_type must be one of: 'bars', 'lines', 'points' or 'mixed'")

  if (!sort_by %in% c("x", "number", "weight"))
    stop("sort_by must be one of: 'x', 'number', or 'weight'")

  if (!position %in% c("dodge", "stack", "fill"))
    stop("position must be one of: 'dodge', 'stack', or 'fill'")

  if (!theme_type %in% c("classic", "minimal", "light", "dark"))
    stop("theme_type must be one of: 'classic', 'minimal', 'light', or 'dark'")

  if (!legend_position %in% c("bottom", "right", "left", "top", "none"))
    stop("legend_position must be one of: 'bottom', 'right', 'left', 'top', or 'none'")

  if(!is.null(cols_length)){
    if(!all(cols_length %in% colnames(df))){
      stop(paste("Columns", paste(cols_length, collapse = ", "), "don't exist in the data.frame"))
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

  # Apply juveniles_by_group to calculate percentages
  group_cols <- var_x

  if (!is.null(fill_var)) {
    group_cols <- c(group_cols, fill_var)
  }

  if (!is.null(facet_var) && !(facet_var %in% group_cols)) {
    group_cols <- c(group_cols, facet_var)
  }

  juvenile_data <- juveniles_by_group(
    data = df,
    group_cols = group_cols,
    cols_length = cols_length,
    a = a,
    b = b
  )

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

  # Complete missing combinations
  if (!is.null(fill_var) || !is.null(facet_var)) {
    # Identify all grouping variables
    group_vars <- c(var_x, "type")
    if (!is.null(fill_var)) {
      group_vars <- c(group_vars, fill_var)
    }
    if (!is.null(facet_var) && !(facet_var %in% group_vars)) {
      group_vars <- c(group_vars, facet_var)
    }

    # Complete all possible combinations
    data_long <- data_long %>%
      tidyr::complete(!!!rlang::syms(group_vars),
                      fill = list(percentage = NA_real_))

    # Optionally replace NAs with zeros
    # (only if we want to explicitly show bars with value 0)
    if (na_to_zero) {
      data_long <- data_long %>%
        dplyr::mutate(percentage = tidyr::replace_na(.data$percentage, 0))
    }
  }

  # Check if X variable is a date type and convert if necessary
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

  # Create the plot with appropriate mapping
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
    # When fill_var is provided, we have options for handling visualization
    if (use_facet_wrap) {
      # Option 1: Use fill_var for fill and facet by type
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
      # Facet by type when requested
      p <- p + ggplot2::facet_wrap(~ type, ncol = ncol)
    } else if (group_by_type) {
      # Option 2: Group by type and then fill_var (better visual separation)
      p <- ggplot2::ggplot(
        data_long,
        ggplot2::aes(
          x = .data[[var_x]],
          y = .data[["percentage"]],
          fill = .data[[fill_var]],
          group = interaction(.data[["type"]], .data[[fill_var]])
        )
      )

      # For line and point plots
      if (plot_type %in% c("lines", "points", "mixed")) {
        p <- p + ggplot2::aes(
          group = interaction(.data[["type"]], .data[[fill_var]]),
          color = interaction(.data[[fill_var]], .data[["type"]])
        )
      }
    } else {
      # Option 3: Use interaction between fill_var and type for the fill
      p <- ggplot2::ggplot(
        data_long,
        ggplot2::aes(
          x = .data[[var_x]],
          y = .data[["percentage"]],
          fill = interaction(.data[[fill_var]], .data[["type"]])
        )
      )

      # For line and point plots
      if (plot_type %in% c("lines", "points", "mixed")) {
        p <- p + ggplot2::aes(
          group = interaction(.data[[fill_var]], .data[["type"]]),
          color = interaction(.data[[fill_var]], .data[["type"]])
        )
      }
    }
  }

  # Add geoms according to plot type
  if (plot_type == "bars") {
    if (!is.null(fill_var) && !use_facet_wrap && group_by_type) {
      # Special case for stacked bars with type distinction
      if (position == "dodge") {
        p <- p + ggplot2::geom_bar(
          stat = "identity",
          position = ggplot2::position_dodge2(width = 0.9, preserve = "single"),
          ggplot2::aes(alpha = .data[["type"]])
        )
        # Add alpha scale for type
        p <- p + ggplot2::scale_alpha_manual(values = c(1, 0.6), name = "Type")
      } else {
        p <- p + ggplot2::geom_bar(
          stat = "identity",
          position = position,
          ggplot2::aes(alpha = .data[["type"]])
        )
        # Add alpha scale for type
        p <- p + ggplot2::scale_alpha_manual(values = c(1, 0.6), name = "Type")
      }
    } else if (position == "dodge") {
      p <- p + ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(width = 0.9))
    } else {
      p <- p + ggplot2::geom_bar(stat = "identity", position = position)
    }

    # Add faceting if needed (for NULL fill_var or when explicitly requested)
    if (is.null(fill_var) && is.null(facet_var)) {
      p <- p + ggplot2::facet_wrap(~ type, ncol = ncol)
    }
  } else if (plot_type == "lines") {
    p <- p + ggplot2::geom_line(size = 1) +
      ggplot2::geom_point(size = 2)

    # Add faceting if needed
    if (is.null(fill_var) && is.null(facet_var)) {
      p <- p + ggplot2::facet_wrap(~ type, ncol = ncol)
    }
  } else if (plot_type == "points") {
    p <- p + ggplot2::geom_point(size = 3, alpha = 0.7)

    # Add faceting if needed
    if (is.null(fill_var) && is.null(facet_var)) {
      p <- p + ggplot2::facet_wrap(~ type, ncol = ncol)
    }
  } else if (plot_type == "mixed") {
    if (!is.null(fill_var) && !use_facet_wrap && group_by_type) {
      # Special case already handled above
    } else if (position == "dodge") {
      p <- p +
        ggplot2::geom_bar(stat = "identity", alpha = 0.7,
                          position = ggplot2::position_dodge(width = 0.9)) +
        ggplot2::geom_point(size = 2, position = ggplot2::position_dodge(width = 0.9))
    } else {
      p <- p +
        ggplot2::geom_bar(stat = "identity", alpha = 0.7, position = position) +
        ggplot2::geom_point(size = 2)
    }

    # Add faceting if needed
    if (is.null(fill_var) && is.null(facet_var)) {
      p <- p + ggplot2::facet_wrap(~ type, ncol = ncol)
    }
  }

  # Ensure all X values are shown and handle axis tick formatting
  if (is.factor(data_long[[var_x]]) || is.character(data_long[[var_x]])) {
    # For categorical variables, explicitly show all values
    p <- p + ggplot2::scale_x_discrete(drop = FALSE)
  } else if (inherits(data_long[[var_x]], "Date")) {
    if(!is.null(step_x_date)){
      # For dates, use specific formatting
      p <- p + ggplot2::scale_x_date(date_breaks = step_x_date, date_labels = "%d %b %Y")
    } else {
      # For dates, use default formatting
      p <- p + ggplot2::scale_x_date(date_breaks = "1 day", date_labels = "%d %b %Y")
    }
  } else if (is.numeric(data_long[[var_x]])) {
    # For numeric variables, ensure a reasonable number of breaks
    unique_vals <- length(unique(data_long[[var_x]]))
    if (unique_vals <= 20) {
      p <- p + ggplot2::scale_x_continuous(breaks = unique(data_long[[var_x]]))
    }
  }

  # Add additional faceting if provided
  if (!is.null(facet_var)) {
    # Create facet formula with appropriate variables
    if (use_facet_wrap) {
      # Include type in faceting when facet_wrap is enabled
      facet_formula <- stats::as.formula(paste("~ type +", facet_var))
      p <- p + ggplot2::facet_wrap(facet_formula, ncol = ncol)
    } else {
      # Only facet by facet_var when facet_wrap is disabled
      facet_formula <- stats::as.formula(paste("~", facet_var))
      p <- p + ggplot2::facet_wrap(facet_formula, ncol = ncol)
    }
  }

  # Set Y-axis limits
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
    subtitle = subtitle,
    y = "Juvenile percentage (%)",
    x = var_x,
    fill = if (is.null(fill_var)) "Type" else fill_var,
    color = if (plot_type %in% c("bars") || is.null(fill_var)) NULL else fill_var
  )

  # Apply the selected theme
  if (theme_type == "classic") {
    p <- p + ggplot2::theme_classic()
  } else if (theme_type == "minimal") {
    p <- p + ggplot2::theme_minimal()
  } else if (theme_type == "light") {
    p <- p + ggplot2::theme_light()
  } else if (theme_type == "dark") {
    p <- p + ggplot2::theme_dark()
  }

  # Adjust theme style
  p <- p + ggplot2::theme(
    legend.position = legend_position,
    panel.grid.minor = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(fill = "lightgray", color = NA),
    strip.text = ggplot2::element_text(face = "bold")
  )

  # Rotate X axis labels if requested
  if (rotate_x_labels) {
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }

  # Add reference line if provided
  if (!is.null(reference_line)) {
    # Find a suitable x-position for the label
    if (inherits(data_long[[var_x]], "Date") || is.numeric(data_long[[var_x]])) {
      x_point <- mean(data_long[[var_x]], na.rm = TRUE)
    } else {
      # For factors or characters, get the middle level
      unique_levels <- unique(as.character(data_long[[var_x]]))
      x_point <- unique_levels[ceiling(length(unique_levels) / 2)]
    }

    p <- p + ggplot2::geom_hline(
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
  }

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
#' @param palette Custom color vector for the graphs. If NULL,
#'   a default palette will be used.
#' @param step_x_date Optional. Interval for date breaks (e.g., "1 day", "1 month").
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
    a = 0.0001,
    b = 2.984,
    col_latitude = NULL,
    col_longitude = NULL,
    col_catch = NULL,
    col_juveniles = NULL,
    xlim = c(-85, -70),
    ylim = c(-20, 0),
    palette = NULL,
    step_x_date = NULL,
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

  if(is.null(step_x_date)){
    step_x_date <- "1 day"
  }

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
    df = data_total,
    var_x = col_date,
    plot_type = "bars",
    cols_length = cols_length,
    a = a,
    b = b,
    step_x_date = step_x_date,
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
    ggplot2::scale_x_date(date_breaks = step_x_date, date_labels = "%d %b %Y") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))



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
        title = title_map
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
