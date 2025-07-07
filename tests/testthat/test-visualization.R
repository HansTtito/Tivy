# Tests for visualization functions (07-visualization.R)

# Helper function to create sample data for visualization
create_sample_zone_data <- function() {
  data.frame(
    zone_id = c("Z001", "Z002", "Z003"),
    lat_min = c(-15.0, -14.0, -13.0),
    lat_max = c(-14.0, -13.0, -12.0),
    lon_min = c(-77.0, -76.5, -76.0),
    lon_max = c(-76.5, -76.0, -75.5),
    zone_name = c("Zone A", "Zone B", "Zone C"),
    stringsAsFactors = FALSE
  )
}

create_sample_fishery_data <- function() {
  data.frame(
    date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03", "2024-01-04")),
    vessel = c("A", "B", "A", "B"),
    latitude = c(-12.0, -12.5, -13.0, -13.5),
    longitude = c(-77.0, -77.5, -78.0, -78.5),
    total_catch = c(100, 150, 120, 180),
    juv = c(15.5, 8.2, 22.1, 12.8),
    `8` = c(10, 5, 8, 12),
    `9` = c(20, 15, 16, 18),
    `10` = c(30, 25, 24, 22),
    `11` = c(25, 20, 20, 15),
    `12` = c(15, 18, 18, 10),
    `13` = c(10, 12, 12, 8),
    `14` = c(5, 8, 8, 5),
    `15` = c(2, 5, 4, 3),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

create_sample_coastline <- function() {
  data.frame(
    Long = c(-78.0, -77.5, -77.0, -76.5, -76.0),
    Lat = c(-14.0, -13.5, -13.0, -12.5, -12.0)
  )
}

# Tests for plot_fishing_zones()
test_that("plot_fishing_zones creates static plots correctly", {
  skip_if_not_installed("ggplot2")
  
  zone_data <- create_sample_zone_data()
  coastline <- create_sample_coastline()
  
  result <- plot_fishing_zones(
    data = zone_data,
    coastline = coastline,
    type = "static",
    title = "Test Fishing Zones"
  )
  
  expect_s3_class(result, "ggplot")
  expect_true("title" %in% names(result$labels))
})

test_that("plot_fishing_zones creates interactive plots correctly", {
  skip_if_not_installed("leaflet")
  
  zone_data <- create_sample_zone_data()
  coastline <- create_sample_coastline()
  
  result <- plot_fishing_zones(
    data = zone_data,
    coastline = coastline,
    type = "interactive",
    base_layers = TRUE,
    minimap = TRUE
  )
  
  expect_s3_class(result, "leaflet")
})

test_that("plot_fishing_zones validates plot type", {
  zone_data <- create_sample_zone_data()
  
  expect_error(
    plot_fishing_zones(data = zone_data, type = "invalid"),
    "type must be either 'static' or 'interactive'"
  )
})

test_that("plot_fishing_zones validates coastline data", {
  zone_data <- create_sample_zone_data()
  invalid_coastline <- data.frame(x = 1, y = 2)  # Missing Long/Lat columns
  
  expect_error(
    plot_fishing_zones(data = zone_data, coastline = invalid_coastline),
    "Coastline must be a data.frame with 'Long' and 'Lat' columns"
  )
})

test_that("plot_fishing_zones handles custom parameters", {
  skip_if_not_installed("ggplot2")
  
  zone_data <- create_sample_zone_data()
  coastline <- create_sample_coastline()
  
  result <- plot_fishing_zones(
    data = zone_data,
    coastline = coastline,
    type = "static",
    colors = c("red", "blue", "green"),
    show_legend = TRUE,
    legend_title = "Custom Legend",
    add_grid = TRUE
  )
  
  expect_s3_class(result, "ggplot")
})

# Tests for plot_juvenile_analysis()
test_that("plot_juvenile_analysis creates plots correctly", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  
  fishery_data <- create_sample_fishery_data()
  length_cols <- c("8", "9", "10", "11", "12", "13", "14", "15")
  
  result <- plot_juvenile_analysis(
    data = fishery_data,
    x_var = "date",
    length_cols = length_cols,
    plot_type = "bars"
  )
  
  expect_s3_class(result, "ggplot")
})

test_that("plot_juvenile_analysis handles different plot types", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  
  fishery_data <- create_sample_fishery_data()
  length_cols <- c("8", "9", "10", "11", "12", "13", "14", "15")
  
  plot_types <- c("bars", "lines", "points", "mixed")
  
  for (plot_type in plot_types) {
    result <- plot_juvenile_analysis(
      data = fishery_data,
      x_var = "date",
      length_cols = length_cols,
      plot_type = plot_type
    )
    
    expect_s3_class(result, "ggplot")
  }
})

test_that("plot_juvenile_analysis validates inputs", {
  fishery_data <- create_sample_fishery_data()
  
  # Test invalid x_var
  expect_error(
    plot_juvenile_analysis(
      data = fishery_data,
      x_var = "nonexistent_column",
      length_cols = c("8", "9", "10")
    ),
    "X variable.*not found in data"
  )
  
  # Test invalid plot_type
  expect_error(
    plot_juvenile_analysis(
      data = fishery_data,
      x_var = "date",
      length_cols = c("8", "9", "10"),
      plot_type = "invalid"
    ),
    "Plot type must be one of"
  )
  
  # Test missing length_cols
  expect_error(
    plot_juvenile_analysis(
      data = fishery_data,
      x_var = "date",
      length_cols = NULL
    ),
    "Length columns must be provided"
  )
})

test_that("plot_juvenile_analysis handles faceting", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  
  fishery_data <- create_sample_fishery_data()
  length_cols <- c("8", "9", "10", "11", "12", "13", "14", "15")
  
  result <- plot_juvenile_analysis(
    data = fishery_data,
    x_var = "date",
    fill_var = "vessel",
    facet_var = "vessel",
    length_cols = length_cols,
    facet_cols = 2
  )
  
  expect_s3_class(result, "ggplot")
})

test_that("plot_juvenile_analysis handles custom styling", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  
  fishery_data <- create_sample_fishery_data()
  length_cols <- c("8", "9", "10", "11", "12", "13", "14", "15")
  
  result <- plot_juvenile_analysis(
    data = fishery_data,
    x_var = "date",
    length_cols = length_cols,
    title = "Custom Title",
    subtitle = "Custom Subtitle",
    color_palette = c("red", "blue"),
    y_limits = c(0, 50),
    reference_line = 25
  )
  
  expect_s3_class(result, "ggplot")
  expect_equal(result$labels$title, "Custom Title")
  expect_equal(result$labels$subtitle, "Custom Subtitle")
})

# Tests for create_fishery_dashboard()
test_that("create_fishery_dashboard creates dashboard correctly", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  
  fishery_data <- create_sample_fishery_data()
  length_cols <- c("8", "9", "10", "11", "12", "13", "14", "15")
  
  result <- create_fishery_dashboard(
    data = fishery_data,
    date_col = "date",
    length_cols = length_cols,
    latitude_col = "latitude",
    longitude_col = "longitude",
    catch_col = "total_catch",
    juvenile_col = "juv"
  )
  
  expect_type(result, "list")
  expect_true("comparison" %in% names(result))
  expect_true("catch_trends" %in% names(result))
  expect_true("trends" %in% names(result))
  
  # Check that plots are ggplot objects
  expect_s3_class(result$comparison, "ggplot")
  expect_s3_class(result$catch_trends, "ggplot")
  expect_s3_class(result$trends, "ggplot")
})

test_that("create_fishery_dashboard auto-detects columns", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  
  fishery_data <- create_sample_fishery_data()
  
  # Test auto-detection (should work with our sample data structure)
  result <- create_fishery_dashboard(
    data = fishery_data
    # All parameters NULL for auto-detection
  )
  
  expect_type(result, "list")
  expect_s3_class(result$comparison, "ggplot")
})

test_that("create_fishery_dashboard handles missing columns gracefully", {
  fishery_data <- data.frame(
    some_column = c(1, 2, 3)
  )
  
  # Should error when required columns not found
  expect_error(
    create_fishery_dashboard(data = fishery_data),
    "Date column not found"
  )
})

test_that("create_fishery_dashboard handles custom parameters", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  
  fishery_data <- create_sample_fishery_data()
  length_cols <- c("8", "9", "10", "11", "12", "13", "14", "15")
  
  result <- create_fishery_dashboard(
    data = fishery_data,
    date_col = "date",
    length_cols = length_cols,
    comparison_title = "Custom Comparison",
    catch_title = "Custom Catch",
    map_title = "Custom Map",
    trend_title = "Custom Trends",
    color_palette = c("red", "blue", "green"),
    date_breaks = "2 days"
  )
  
  expect_type(result, "list")
  expect_equal(result$comparison$labels$title, "Custom Comparison")
  expect_equal(result$catch_trends$labels$title, "Custom Catch")
})

test_that("create_fishery_dashboard handles spatial mapping", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  
  fishery_data <- create_sample_fishery_data()
  length_cols <- c("8", "9", "10", "11", "12", "13", "14", "15")
  
  result <- create_fishery_dashboard(
    data = fishery_data,
    date_col = "date",
    length_cols = length_cols,
    latitude_col = "latitude",
    longitude_col = "longitude",
    catch_col = "total_catch",
    juvenile_col = "juv",
    map_xlim = c(-79, -76),
    map_ylim = c(-14, -11)
  )
  
  # Map might be NULL if rnaturalearth is not available
  if (!is.null(result$spatial_map)) {
    expect_s3_class(result$spatial_map, "ggplot")
  }
})

# Tests for helper functions
test_that("visualization helper functions exist", {
  # Test that required helper functions exist
  expect_true(exists("prepare_polygons"))
  expect_true(exists("plot_zones_static"))
  expect_true(exists("plot_zones_interactive"))
  expect_true(exists("create_juvenile_base_plot"))
  expect_true(exists("add_juvenile_geoms"))
  expect_true(exists("add_juvenile_faceting"))
  expect_true(exists("customize_juvenile_axes"))
  expect_true(exists("apply_juvenile_theme"))
  expect_true(exists("add_juvenile_reference_line"))
})

# Integration test
test_that("visualization workflow integration", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  
  # Test that main visualization functions work together
  fishery_data <- create_sample_fishery_data()
  zone_data <- create_sample_zone_data()
  coastline <- create_sample_coastline()
  length_cols <- c("8", "9", "10", "11", "12", "13", "14", "15")
  
  # Create juvenile plot
  juvenile_plot <- plot_juvenile_analysis(
    data = fishery_data,
    x_var = "date",
    length_cols = length_cols
  )
  
  # Create zone plot
  zone_plot <- plot_fishing_zones(
    data = zone_data,
    coastline = coastline,
    type = "static"
  )
  
  # Create dashboard
  dashboard <- create_fishery_dashboard(
    data = fishery_data,
    date_col = "date",
    length_cols = length_cols
  )
  
  # Check that all outputs are valid
  expect_s3_class(juvenile_plot, "ggplot")
  expect_s3_class(zone_plot, "ggplot")
  expect_type(dashboard, "list")
  expect_true(length(dashboard) >= 3)
})