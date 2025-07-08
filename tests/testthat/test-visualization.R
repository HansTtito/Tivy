# Helper function to create sample data for visualization
create_sample_polygon_data <- function() {
  data.frame(
    StartDateTime = as.POSIXct(c("2024-01-15 08:00:00", "2024-01-20 10:00:00", "2024-02-01 12:00:00")),
    EndDateTime = as.POSIXct(c("2024-01-15 18:00:00", "2024-01-20 20:00:00", "2024-02-01 22:00:00")),
    StartLatitude = c("15°30'S", "16°00'S", "14°30'S"),
    EndLatitude = c("15°45'S", "16°15'S", "14°45'S"),
    StartLongitude = c("75°30'W", "76°00'W", "74°30'W"),
    EndLongitude = c("75°45'W", "76°15'W", "74°45'W"),
    StartNauticalMiles = c(5, 10, 8),
    EndNauticalMiles = c(15, 25, 18),
    file_name = c("announcement1.pdf", "announcement2.pdf", "announcement3.pdf"),
    announcement = c("Zone A closure", "Zone B restriction", "Zone C monitoring"),
    stringsAsFactors = FALSE
  )
}

#' Create sample fishery data for juvenile analysis plots
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

#' Create sample coastline data
create_sample_coastline <- function() {
  data.frame(
    Long = c(-78.0, -77.5, -77.0, -76.5, -76.0, -75.5, -75.0),
    Lat = c(-17.0, -15.5, -14.0, -12.5, -11.0, -9.5, -8.0)
  )
}

# Tests for plot_fishing_zones()
test_that("plot_fishing_zones creates static plots correctly", {
  skip_if_not_installed("ggplot2")
  
  zone_data <- create_sample_polygon_data()
  coastline <- create_sample_coastline()
  
  result <- plot_fishing_zones(
    data = zone_data,
    coastline = coastline,
    type = "static",
    title = "Test Fishing Zones"
  )
  
  expect_s3_class(result, "ggplot")
  expect_true(!is.null(result$labels$title))
})

test_that("plot_fishing_zones creates interactive plots correctly", {
  skip_if_not_installed("leaflet")
  
  zone_data <- create_sample_polygon_data()
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
  zone_data <- create_sample_polygon_data()
  
  expect_error(
    plot_fishing_zones(data = zone_data, type = "invalid"),
    "type must be either 'static' or 'interactive'"
  )
})

test_that("plot_fishing_zones validates coastline data", {
  zone_data <- create_sample_polygon_data()
  invalid_coastline <- data.frame(x = 1, y = 2)  # Missing Long/Lat columns
  
  expect_error(
    plot_fishing_zones(data = zone_data, coastline = invalid_coastline),
    "Coastline must be a data.frame with 'Long' and 'Lat' columns"
  )
})

test_that("plot_fishing_zones handles custom parameters", {
  skip_if_not_installed("ggplot2")
  
  zone_data <- create_sample_polygon_data()
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

test_that("plot_fishing_zones uses default coastline when NULL", {
  skip_if_not_installed("ggplot2")
  
  zone_data <- create_sample_polygon_data()
  
  # Should work with NULL coastline (uses default)
  result <- plot_fishing_zones(
    data = zone_data,
    coastline = NULL,
    type = "static"
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

test_that("plot_juvenile_analysis handles invalid length columns", {
  fishery_data <- create_sample_fishery_data()
  
  expect_error(
    plot_juvenile_analysis(
      data = fishery_data,
      x_var = "date",
      length_cols = c("nonexistent1", "nonexistent2")
    ),
    "Some length columns not found"
  )
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

test_that("create_fishery_dashboard validates input data", {
  expect_error(
    create_fishery_dashboard("not a data frame"),
    "data must be a data.frame"
  )
})

# Tests for helper functions
test_that("visualization helper functions exist", {
  # Test that required helper functions exist
  helper_functions <- c(
    "prepare_polygons",
    "plot_zones_static", 
    "plot_zones_interactive",
    "create_juvenile_base_plot",
    "add_juvenile_geoms",
    "add_juvenile_faceting",
    "customize_juvenile_axes",
    "apply_juvenile_theme",
    "add_juvenile_reference_line"
  )
  
  for (func in helper_functions) {
    expect_true(exists(func), info = paste("Function", func, "should exist"))
  }
})

test_that("find_column helper works in visualization context", {
  # Test column finding for visualization
  patterns <- c("fecha", "date", "dia")
  column_names <- c("date", "vessel", "catch")
  
  result <- find_column(patterns, column_names)
  
  expect_type(result, "integer")
  expect_equal(result, 1)  # Should find "date"
})

# Tests for edge cases in visualization
test_that("visualization functions handle empty data", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  
  empty_data <- data.frame(
    date = as.Date(character(0)),
    `8` = numeric(0),
    `9` = numeric(0),
    check.names = FALSE
  )
  
  # Should handle empty data gracefully
  expect_error(
    plot_juvenile_analysis(
      data = empty_data,
      x_var = "date",
      length_cols = c("8", "9")
    ),
    regexp = ".*"  # Some error expected with empty data
  )
})

test_that("visualization functions handle single data point", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  
  single_point_data <- data.frame(
    date = as.Date("2024-01-01"),
    `8` = 10,
    `9` = 20,
    `10` = 30,
    check.names = FALSE
  )
  
  result <- plot_juvenile_analysis(
    data = single_point_data,
    x_var = "date",
    length_cols = c("8", "9", "10")
  )
  
  expect_s3_class(result, "ggplot")
})

test_that("visualization functions handle NA values", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  
  data_with_na <- data.frame(
    date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
    `8` = c(10, NA, 15),
    `9` = c(20, 25, NA),
    `10` = c(30, 35, 40),
    check.names = FALSE
  )
  
  result <- plot_juvenile_analysis(
    data = data_with_na,
    x_var = "date",
    length_cols = c("8", "9", "10"),
    na_to_zero = TRUE
  )
  
  expect_s3_class(result, "ggplot")
})

# Integration test
test_that("visualization workflow integration", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  
  # Test that main visualization functions work together
  fishery_data <- create_sample_fishery_data()
  zone_data <- create_sample_polygon_data()
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

# Test patchwork integration if available
test_that("dashboard combines plots with patchwork if available", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  
  fishery_data <- create_sample_fishery_data()
  length_cols <- c("8", "9", "10", "11", "12", "13", "14", "15")
  
  dashboard <- create_fishery_dashboard(
    data = fishery_data,
    date_col = "date",
    length_cols = length_cols
  )
  
  # If patchwork is available, should have combined dashboard
  if (requireNamespace("patchwork", quietly = TRUE)) {
    expect_true("dashboard" %in% names(dashboard))
  }
})

# Test theme and styling functions
test_that("visualization themes work correctly", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  
  fishery_data <- create_sample_fishery_data()
  length_cols <- c("8", "9", "10", "11", "12")
  
  # Test different theme styles
  theme_styles <- c("classic", "minimal", "light", "dark")
  
  for (style in theme_styles) {
    result <- plot_juvenile_analysis(
      data = fishery_data,
      x_var = "date",
      length_cols = length_cols,
      theme_style = style
    )
    
    expect_s3_class(result, "ggplot")
  }
})

# Test color palette functionality
test_that("visualization color palettes work correctly", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  
  fishery_data <- create_sample_fishery_data()
  length_cols <- c("8", "9", "10", "11", "12")
  
  # Test custom color palette
  custom_colors <- c("#FF5733", "#33FF57", "#3357FF", "#FF33F5")
  
  result <- plot_juvenile_analysis(
    data = fishery_data,
    x_var = "date",
    length_cols = length_cols,
    color_palette = custom_colors
  )
  
  expect_s3_class(result, "ggplot")
})

# Test sorting functionality
test_that("visualization sorting works correctly", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  
  fishery_data <- create_sample_fishery_data()
  length_cols <- c("8", "9", "10", "11", "12")
  
  # Test different sorting methods
  sort_methods <- c("x", "number", "weight")
  
  for (method in sort_methods) {
    result <- plot_juvenile_analysis(
      data = fishery_data,
      x_var = "vessel",  # Use categorical variable for sorting
      length_cols = length_cols,
      sort_by = method
    )
    
    expect_s3_class(result, "ggplot")
  }
})

# Test reference line functionality
test_that("reference lines work correctly", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  
  fishery_data <- create_sample_fishery_data()
  length_cols <- c("8", "9", "10", "11", "12")
  
  result <- plot_juvenile_analysis(
    data = fishery_data,
    x_var = "date",
    length_cols = length_cols,
    reference_line = 15  # Add reference line at 15%
  )
  
  expect_s3_class(result, "ggplot")
  
  # Check that reference line was added
  expect_true(length(result$layers) > 1)  # Should have more than just the main geom
})

# Test legend positioning
test_that("legend positioning works correctly", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  
  fishery_data <- create_sample_fishery_data()
  length_cols <- c("8", "9", "10", "11", "12")
  
  # Test different legend positions
  legend_positions <- c("bottom", "top", "left", "right", "none")
  
  for (position in legend_positions) {
    result <- plot_juvenile_analysis(
      data = fishery_data,
      x_var = "date",
      length_cols = length_cols,
      legend_position = position
    )
    
    expect_s3_class(result, "ggplot")
  }
})

# Test error messages for visualization functions
test_that("visualization functions provide helpful error messages", {
  fishery_data <- create_sample_fishery_data()
  
  # Test helpful error for missing columns
  expect_error(
    plot_juvenile_analysis(
      data = fishery_data,
      x_var = "nonexistent",
      length_cols = c("8", "9")
    ),
    "not found in data"
  )
  
  # Test helpful error for invalid plot type
  expect_error(
    plot_juvenile_analysis(
      data = fishery_data,
      x_var = "date",
      length_cols = c("8", "9"),
      plot_type = "invalid_type"
    ),
    "Plot type must be one of"
  )
})

# Test performance with larger datasets (if needed)
test_that("visualization functions handle larger datasets", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  
  # Create larger dataset
  large_data <- data.frame(
    date = rep(seq(as.Date("2024-01-01"), as.Date("2024-01-30"), by = "day"), each = 3),
    vessel = rep(c("A", "B", "C"), 30),
    `8` = sample(1:50, 90, replace = TRUE),
    `9` = sample(1:50, 90, replace = TRUE),
    `10` = sample(1:50, 90, replace = TRUE),
    check.names = FALSE
  )
  
  result <- plot_juvenile_analysis(
    data = large_data,
    x_var = "date",
    length_cols = c("8", "9", "10")
  )
  
  expect_s3_class(result, "ggplot")
})