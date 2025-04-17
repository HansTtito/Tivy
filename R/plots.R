#' Plot fishing suspension zone polygons with ggplot2
#'
#' @description
#' Creates static visualizations of polygons for fishing suspension zones
#' using ggplot2. Returns a ggplot2 object that can be modified later.
#'
#' @param data A data frame with latitude and longitude coordinates for start and end points,
#'        which can be in text format (like "12°30'S") or decimal degrees.
#' @param coastline A data frame containing the coastline for visualization.
#'        Must have columns 'Long' and 'Lat'. Default `Tivy::peru_coastline`.
#' @param parallels A data frame containing lines parallel to the coast. Default `Tivy::peru_coast_parallels`.
#' @param title Title for the plot. Default NULL
#' @param colors Vector of colors for the polygons. Default NULL
#' @param show_legend Logical. If TRUE, shows the legend. Default FALSE.
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
#' g <- plot_polygons_ggplot(data = results, coastline = Tivy::peru_coastline)
#'
#' # Customize later
#' g +
#'   ggplot2::xlim(-80, -70) +
#'   ggplot2::theme_minimal() +
#'   ggplot2::labs(title = "Custom title")
#'
#' @export
plot_polygons_ggplot <- function(data,
                                 coastline = peru_coastline,
                                 parallels = peru_coast_parallels,
                                 title = NULL,
                                 colors = NULL,
                                 show_legend = FALSE,
                                 labels = NULL,
                                 add_grid = FALSE) {

  # Prepare the polygons (all the data processing is the same)
  polygons <- prepare_polygons(data = data, coastline = coastline, coast_parallels = parallels)

  # Create static visualization with ggplot2
  return(plot_static(
    polygons,
    coastline = coastline,
    title = title,
    colors = colors,
    show_legend = show_legend,
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
#' @param coastline A data frame containing the coastline for visualization.
#'        Must have columns 'Long' and 'Lat'. Default `Tivy::peru_coastline`.
#' @param parallels A data frame containing lines parallel to the coast. Default `Tivy::peru_coast_parallels`.
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
#' m <- plot_polygons_leaflet(data = results, coastline = Tivy::peru_coastline)
#'
#' # Customize later
#' m |>
#'   leaflet::addMarkers(lng = -77.1, lat = -12.0, popup = "Lima") |>
#'   leaflet::addCircleMarkers(lng = -76.3, lat = -13.4, radius = 5, color = "red")
#'
#' @export
plot_polygons_leaflet <- function(data,
                                  coastline = peru_coastline,
                                  parallels = peru_coast_parallels,
                                  title = NULL,
                                  colors = NULL,
                                  show_legend = FALSE,
                                  labels = NULL,
                                  base_layers = FALSE,
                                  minimap = FALSE) {

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
#' @param coastline Data frame with the coastline to plot. Default `Tivy::peru_coastline`.
#' @param parallels A data frame containing lines parallel to the coast. Default `Tivy::peru_coast_parallels`.
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
#' plot_polygons(data = results, coastline = Tivy::peru_coastline)
#'
#' @export
plot_polygons <- function(data,
                          coastline = peru_coastline,
                          parallels = peru_coast_parallels,
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
#' @param plot_type Type of plot to show: "bars" (default), "lines", "points", "mixed", or "boxplot".
#' @param title (Optional) Main title of the plot.
#' @param juv_limit (Optional) Threshold value representing the legal limit for juvenile percentage; added as a horizontal line.
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
#' data_length_fishing_trips <- merge(x = data_fishing_trips, y = hauls_length, by = "fishing_trip_code")
#' data_total <- merge_length_fishing_trips_hauls(data_hauls = data_hauls, data_length_fishing_trips = data_length_fishing_trips)
#'
#' final_data <- add_variables(data_total)
#' length_cols <- c("8", "8.5", "9", "9.5", "10", "10.5", "11", "11.5", "12",
#'                  "12.5", "13", "13.5", "14", "14.5", "15")
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
#'   var_x = "unique_date",
#'   juv_limit = 12
#' )
plot_juveniles <- function(juvenile_data, var_x, fill_var = NULL,
                           plot_type = "bars", title = NULL,
                           juv_limit = NULL, sort_by = "x",
                           palette = NULL, facet_var = NULL, ncol = 2) {

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
        type,
        levels = c("perc_juv_number", "perc_juv_weight"),
        labels = c("By number", "By weight")
      )
    )

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
      dplyr::arrange(dplyr::desc(perc_juv_number)) %>%
      dplyr::pull(var_x)
    data_long[[var_x]] <- factor(data_long[[var_x]], levels = unique(order))
  } else if (sort_by == "weight") {
    order <- juvenile_data %>%
      dplyr::arrange(dplyr::desc(perc_juv_weight)) %>%
      dplyr::pull(var_x)
    data_long[[var_x]] <- factor(data_long[[var_x]], levels = unique(order))
  }

  # Create plot base
  if (is.null(fill_var)) {
    p <- ggplot2::ggplot(data_long, ggplot2::aes(x = .data[[var_x]], y = percentage, fill = type))
  } else {
    p <- ggplot2::ggplot(data_long, ggplot2::aes(x = .data[[var_x]], y = percentage,
                                                  fill = .data[[fill_var]], color = .data[[fill_var]]))
  }

  # Add geoms according to plot type
  if (plot_type == "bars") {
    if (is.null(fill_var)) {
      p <- p + ggplot2::geom_bar(stat = "identity", position = "dodge")
    } else {
      p <- p + ggplot2::geom_bar(stat = "identity", position = "dodge") +
        ggplot2::facet_wrap(~ type, ncol = ncol)
    }
  } else if (plot_type == "lines") {
    p <- p + ggplot2::geom_line(ggplot2::aes(group = .data[[if (is.null(fill_var)) "type" else fill_var]]),
                                size = 1) +
      ggplot2::geom_point(size = 2)

    if (is.null(fill_var)) {
      p <- p + ggplot2::facet_wrap(~ type, ncol = ncol)
    }
  } else if (plot_type == "points") {
    p <- p + ggplot2::geom_point(size = 3, alpha = 0.7)

    if (is.null(fill_var)) {
      p <- p + ggplot2::facet_wrap(~ type, ncol = ncol)
    }
  } else if (plot_type == "mixed") {
    p <- p + ggplot2::geom_bar(stat = "identity", alpha = 0.5, position = "dodge") +
      ggplot2::geom_point(size = 2, position = ggplot2::position_dodge(width = 0.9))

    if (is.null(fill_var)) {
      p <- p + ggplot2::facet_wrap(~ type, ncol = ncol)
    }
  }

  # Add legal juvenile limit line if provided
  if (!is.null(juv_limit)) {

    # Calculate midpoint of x-axis according to variable type
    if (inherits(data_long[[var_x]], "Date") || is.numeric(data_long[[var_x]])) {
      x_point <- mean(data_long[[var_x]], na.rm = TRUE)
    } else {
      # For factors or characters, get the middle level
      unique_levels <- unique(as.character(data_long[[var_x]]))
      x_point <- unique_levels[ceiling(length(unique_levels) / 2)]
    }

    p <- p + ggplot2::geom_hline(yintercept = juv_limit, linetype = "dashed",
                                 color = "red", linewidth = 1) +
      ggplot2::annotate("text", x = x_point, y = juv_limit, label = paste("Limit:", juv_limit, "%"),
                        hjust = 1.1, vjust = -0.5, color = "black")
  }

  # Add color palette if provided
  if (!is.null(palette)) {
    if (is.null(fill_var)) {
      p <- p + ggplot2::scale_fill_manual(values = palette)
    } else {
      p <- p + ggplot2::scale_fill_manual(values = palette) +
        ggplot2::scale_color_manual(values = palette)
    }
  }

  # Add additional faceting if provided
  if (!is.null(facet_var)) {
    if (is.null(fill_var)) {
      p <- p + ggplot2::facet_wrap(~ type + .data[[facet_var]], ncol = ncol)
    } else {
      p <- p + ggplot2::facet_wrap(~ .data[[facet_var]], ncol = ncol)
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
    color = if (is.null(fill_var)) NULL else fill_var
  ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
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
#' @param juv_limit Numeric value representing the legal limit for defining
#'   juveniles (in cm). The default value is 12.
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
#' @importFrom dplyr group_by summarise arrange mutate filter
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_area geom_line geom_point geom_sf labs theme_minimal scale_color_gradient2 scale_size_continuous coord_sf geom_smooth facet_wrap scale_color_manual scale_y_continuous
#' @importFrom scales comma
#' @importFrom rnaturalearth ne_countries
#' @importFrom patchwork wrap_plots
#'
#' @examples
#' \dontrun{
#'
#' data_hauls <- process_hauls(data_hauls = calas_bitacora)
#' data_fishing_trips <- process_fishing_trips(data_fishing_trips = faenas_bitacora)
#' hauls_length <- process_length(data_length = tallas_bitacora)
#'
#' # Integrate data
#' data_length_fishing_trips <- merge(x = data_fishing_trips, y = hauls_length, by = 'fishing_trip_code')
#' data_total <- merge_length_fishing_trips_hauls(data_hauls = data_hauls,
#'                                        data_length_fishing_trips = data_length_fishing_trips)
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
  # Initial validations
  if (!is.data.frame(data_total))
    stop("data_total must be a data.frame")

  # Try to automatically detect columns if not specified
  if (is.null(col_date)) {
    date_cols <- grep("fecha|date", colnames(data_total), ignore.case = TRUE)
    col_date <- if (length(date_cols) > 0) colnames(data_total)[date_cols[1]] else NULL
    if (is.null(col_date))
      stop("Could not automatically detect the date column. Please specify col_date.")
  }

  if (is.null(cols_length)) {
    # Try to detect size columns (assuming a common pattern)
    length_cols_idx <- grep("^talla|^len|^pond_|cm$", colnames(data_total), ignore.case = TRUE)
    cols_length <- if (length(length_cols_idx) > 0) colnames(data_total)[length_cols_idx] else NULL
    if (is.null(cols_length))
      stop("Could not automatically detect size columns. Please specify cols_length.")
  }

  if (is.null(col_latitude)) {
    lat_cols <- grep("^lat|^latitude", colnames(data_total), ignore.case = TRUE)
    col_latitude <- if (length(lat_cols) > 0) colnames(data_total)[lat_cols[1]] else NULL
  }

  if (is.null(col_longitude)) {
    lon_cols <- grep("^lon|^longitude", colnames(data_total), ignore.case = TRUE)
    col_longitude <- if (length(lon_cols) > 0) colnames(data_total)[lon_cols[1]] else NULL
  }

  if (is.null(col_catch)) {
    cap_cols <- grep("catch|captura|desembar", colnames(data_total), ignore.case = TRUE)
    col_catch <- if (length(cap_cols) > 0) colnames(data_total)[cap_cols[1]] else NULL
  }

  if (is.null(col_juveniles)) {
    juv_cols <- grep("juv|perc_juv", colnames(data_total), ignore.case = TRUE)
    col_juveniles <- if (length(juv_cols) > 0) colnames(data_total)[juv_cols[1]] else NULL
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
    juv_limit = juv_limit,
    sort_by = if(sort_comparison) "weight" else "x",
    palette = c("#3498DB", "#E74C3C")
  )

  # 2. Cumulative catch plot
  p2 <- NULL
  if (!is.null(col_date) && !is.null(col_catch) && col_catch %in% colnames(data_total)) {
    # Prepare data for cumulative catch
    catch_data <- data_total %>%
      dplyr::group_by(.data[[col_date]]) %>%
      dplyr::reframe(
        daily_catch = sum(.data[[col_catch]], na.rm = TRUE)
              ) %>%
      dplyr::arrange(.data[[col_date]]) %>%
      dplyr::mutate(cumulative_catch = cumsum(daily_catch))

    # Create cumulative catch plot
    p2 <- ggplot2::ggplot(catch_data) +
      ggplot2::geom_area(ggplot2::aes(x = .data[[col_date]], y = cumulative_catch),
                         fill = "#2CA02C", alpha = 0.7) +
      ggplot2::geom_line(ggplot2::aes(x = .data[[col_date]], y = cumulative_catch),
                         color = "#1F77B4", size = 1) +
      ggplot2::geom_point(ggplot2::aes(x = .data[[col_date]], y = cumulative_catch),
                          color = "#1F77B4", size = 2.5) +
      ggplot2::labs(
        title = title_catch,
        x = col_date,
        y = "Cumulative catch (t)"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::scale_y_continuous(labels = scales::comma)
  }

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
        type,
        levels = c("perc_juv_number", "perc_juv_weight"),
        labels = c("By number", "By weight")
      ),
      total = ifelse(type == "By number", total_number, total_weight)
    )

  p4 <- ggplot2::ggplot(data_long, ggplot2::aes(x = .data[[col_date]], y = percentage, color = type)) +
    ggplot2::geom_point(size = 3, alpha = 0.7) +
    ggplot2::facet_wrap(~ type, scales = "free_x") +
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