# Tests for helper and utility functions

# Test helper functions that appear across multiple files
test_that("find_column function works correctly", {
  patterns <- c("codigo.*faena", "trip.*code", "faena")
  column_names <- c("codigo_faena", "numero_cala", "especie")
  
  result <- find_column(patterns, column_names)
  
  expect_type(result, "integer")
  expect_equal(result, 1)  # Should find the first column
})

test_that("find_column handles no matches", {
  patterns <- c("nonexistent")
  column_names <- c("codigo_faena", "numero_cala", "especie")
  
  result <- find_column(patterns, column_names)
  
  expect_null(result)
})

test_that("find_column handles multiple matches with warning", {
  patterns <- c("codigo")
  column_names <- c("codigo_faena", "codigo_embarcacion", "especie")
  
  expect_warning(
    result <- find_column(patterns, column_names),
    "Multiple columns found"
  )
  
  expect_equal(result, 1)  # Should return first match
})

test_that("find_columns_by_pattern works correctly", {
  # Create sample data
  test_data <- data.frame(
    codigo_faena = 1,
    weighted_8 = 1,
    weighted_8.5 = 1,
    weighted_9 = 1,
    other_col = 1
  )
  
  result <- find_columns_by_pattern(test_data, pattern = "weighted_")
  
  expect_type(result, "character")
  expect_true(all(grepl("weighted_", result)))
  expect_equal(length(result), 3)
})

test_that("find_columns_by_pattern handles no matches", {
  test_data <- data.frame(
    codigo_faena = 1,
    numero_cala = 1
  )
  
  expect_warning(
    result <- find_columns_by_pattern(test_data, pattern = "weighted_"),
    "No columns matching pattern found"
  )
  
  expect_length(result, 0)
})

test_that("find_columns_by_pattern sorts correctly", {
  test_data <- data.frame(
    weighted_10 = 1,
    weighted_8 = 1,
    weighted_9 = 1,
    weighted_8.5 = 1
  )
  
  result <- find_columns_by_pattern(test_data, pattern = "weighted_", sort = TRUE)
  
  # Should be sorted numerically
  expect_equal(result[1], "weighted_8")
  expect_equal(result[2], "weighted_8.5")
  expect_equal(result[3], "weighted_9")
  expect_equal(result[4], "weighted_10")
})

# Tests for extract_numeric_values helper
test_that("extract_numeric_values works correctly", {
  length_cols <- c("8", "8.5", "9", "9.5", "10")
  
  result <- extract_numeric_values(length_cols)
  
  expect_type(result, "double")
  expect_equal(result, c(8, 8.5, 9, 9.5, 10))
})

test_that("extract_numeric_values handles prefixed columns", {
  length_cols <- c("length_8", "length_8.5", "length_9")
  
  result <- extract_numeric_values(length_cols)
  
  expect_type(result, "double")
  expect_equal(result, c(8, 8.5, 9))
})

test_that("extract_numeric_values handles invalid input", {
  length_cols <- c("invalid", "not_numeric", "abc")
  
  expect_warning(
    result <- extract_numeric_values(length_cols),
    "Could not extract numeric values"
  )
  
  expect_true(all(is.na(result)))
})

# Tests for convert_to_date helper
test_that("convert_to_date works with different formats", {
  # Test various date formats
  dates <- c("2024-01-15", "15/01/2024", "2024-01-15 08:30:00")
  
  for (date in dates) {
    result_date <- convert_to_date(date, output_type = "date")
    result_datetime <- convert_to_date(date, output_type = "datetime")
    
    expect_s3_class(result_date, "Date")
    expect_s3_class(result_datetime, "POSIXct")
  }
})

test_that("convert_to_date handles invalid dates", {
  invalid_dates <- c("invalid", "32/13/2024", "")
  
  for (date in invalid_dates) {
    expect_warning(
      result <- convert_to_date(date),
      "Could not parse date"
    )
  }
})

test_that("convert_to_date handles NA values", {
  result <- convert_to_date(NA)
  expect_true(is.na(result))
})

# Tests for process_weighting_block helper
test_that("process_weighting_block works correctly", {
  data <- data.frame(
    fishing_trip_code = c("F001", "F002"),
    `8` = c(10, 5),
    `9` = c(20, 15),
    `10` = c(30, 25),
    total_catch = c(1000, 800),
    check.names = FALSE
  )
  
  length_cols <- c("8", "9", "10")
  catch_col <- "total_catch"
  a <- 0.0001
  b <- 2.984
  
  result <- process_weighting_block(
    data = data,
    length_cols = length_cols,
    catch_col = catch_col,
    a = a,
    b = b,
    silence_warnings = TRUE
  )
  
  expect_s3_class(result, "data.frame")
  expect_true("weighted_8" %in% names(result))
  expect_true("weighted_9" %in% names(result))
  expect_true("weighted_10" %in% names(result))
})

# Tests for calculate_distances_vectorized helper
test_that("calculate_distances_vectorized works correctly", {
  lon_point <- c(-77.0, -76.5)
  lat_point <- c(-12.0, -11.5)
  coast_lon <- c(-77.5, -77.0, -76.5, -76.0)
  coast_lat <- c(-12.5, -12.0, -11.5, -11.0)
  
  result <- calculate_distances_vectorized(
    lon_point = lon_point,
    lat_point = lat_point,
    coast_lon = coast_lon,
    coast_lat = coast_lat,
    distance_type = "haversine",
    window = 1.0,
    unit = "nm"
  )
  
  expect_type(result, "list")
  expect_true("distances" %in% names(result))
  expect_true("indices" %in% names(result))
  expect_length(result$distances, 2)
  expect_length(result$indices, 2)
})

test_that("calculate_distances_vectorized handles different distance types", {
  lon_point <- -77.0
  lat_point <- -12.0
  coast_lon <- c(-77.5, -77.0, -76.5)
  coast_lat <- c(-12.5, -12.0, -11.5)
  
  distance_types <- c("haversine", "euclidean", "grid")
  
  for (dist_type in distance_types) {
    result <- calculate_distances_vectorized(
      lon_point = lon_point,
      lat_point = lat_point,
      coast_lon = coast_lon,
      coast_lat = coast_lat,
      distance_type = dist_type,
      window = 1.0,
      unit = "nm"
    )
    
    expect_type(result, "list")
    expect_true(result$distances > 0)
  }
})

# Tests for coordinate validation helpers
test_that("coordinate validation helpers work", {
  # Test valid coordinates
  valid_coords <- list(
    lat = c(-12.0, -11.5, -13.0),
    lon = c(-77.0, -76.5, -78.0)
  )
  
  expect_true(validate_coordinates(valid_coords$lat, valid_coords$lon))
  
  # Test invalid coordinates
  invalid_coords <- list(
    lat = c(-95.0, -11.5, -13.0),  # Invalid latitude
    lon = c(-77.0, -76.5, -78.0)
  )
  
  expect_false(validate_coordinates(invalid_coords$lat, invalid_coords$lon))
})

# Tests for data cleaning helpers
test_that("clean_species_names works correctly", {
  species <- c("  ANCHOVETA  ", "Sardina", "JUREL ", " caballa")
  
  result <- clean_species_names(species)
  
  expect_equal(result, c("ANCHOVETA", "SARDINA", "JUREL", "CABALLA"))
  expect_true(all(result == toupper(trimws(result))))
})

test_that("standardize_vessel_names works correctly", {
  vessels <- c("  Vessel A  ", "vessel-b", "VESSEL_C ")
  
  result <- standardize_vessel_names(vessels)
  
  expect_true(all(result == toupper(trimws(result))))
  expect_true(all(!grepl("^\\s|\\s$", result)))  # No leading/trailing spaces
})

# Tests for statistical helpers
test_that("safe_numeric_conversion works correctly", {
  # Test valid numeric strings
  valid_nums <- c("123", "45.67", "0", "-12.34")
  result <- safe_numeric_conversion(valid_nums, "test_column")
  
  expect_type(result, "double")
  expect_equal(result, c(123, 45.67, 0, -12.34))
  
  # Test invalid strings
  invalid_nums <- c("abc", "12.34.56", "", NA)
  expect_warning(
    result <- safe_numeric_conversion(invalid_nums, "test_column"),
    "Could not convert some values"
  )
})

test_that("calculate_percentiles works correctly", {
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  
  result <- calculate_percentiles(data, percentiles = c(0.25, 0.5, 0.75))
  
  expect_type(result, "double")
  expect_length(result, 3)
  expect_equal(names(result), c("25%", "50%", "75%"))
})

# Tests for file handling helpers
test_that("check_required_packages works correctly", {
  # Test with packages that should exist
  expect_silent(check_required_packages(c("base", "utils")))
  
  # Test with non-existent package
  expect_error(
    check_required_packages(c("base", "nonexistent_package_xyz")),
    "Required packages not available"
  )
})

test_that("create_safe_filename works correctly", {
  unsafe_names <- c("file with spaces.txt", "file/with\\slashes", "file:with*special?chars")
  
  for (name in unsafe_names) {
    result <- create_safe_filename(name)
    
    # Should not contain unsafe characters
    expect_false(grepl("[/\\\\:*?\"<>|]", result))
    expect_true(nchar(result) > 0)
  }
})

# Integration tests for helper functions
test_that("helper functions work together", {
  # Test workflow using multiple helpers
  sample_data <- data.frame(
    codigo_faena = c("F001", "F002", "F003"),
    weighted_8 = c("10", "5", "8"),  # String numbers
    weighted_9 = c("20", "15", "16"),
    species = c("  ANCHOVETA  ", "sardina", "JUREL "),
    stringsAsFactors = FALSE
  )
  
  # Find length columns
  length_cols <- find_columns_by_pattern(sample_data, "weighted_", sort = TRUE)
  expect_length(length_cols, 2)
  
  # Clean species names
  sample_data$species <- clean_species_names(sample_data$species)
  expect_equal(sample_data$species, c("ANCHOVETA", "SARDINA", "JUREL"))
  
  # Convert numeric columns
  for (col in length_cols) {
    sample_data[[col]] <- safe_numeric_conversion(sample_data[[col]], col)
  }
  
  expect_type(sample_data$weighted_8, "double")
  expect_type(sample_data$weighted_9, "double")
})