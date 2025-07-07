# Tests for coordinate and spatial functions

# Tests for dms_to_decimal()
test_that("dms_to_decimal converts coordinates correctly", {
  # Test standard DMS format
  coords <- c("15°30'25\"S", "75°45'30\"W")
  result <- dms_to_decimal(coords, hemisphere = "S")
  
  expect_type(result, "double")
  expect_length(result, 2)
  expect_true(all(!is.na(result)))
  
  # Check that southern coordinates are negative
  expect_true(result[1] < 0)
})

test_that("dms_to_decimal handles different formats", {
  # Test various coordinate formats
  coords <- c(
    "15°30'25\"S",    # Complete DMS
    "75°45'W",        # DM format
    "16 15 30 S",     # Space-separated
    "77 30 W"         # Space-separated DM
  )
  
  result <- dms_to_decimal(coords)
  
  expect_length(result, 4)
  expect_true(all(!is.na(result)))
  expect_true(all(result < 0))  # All should be negative for S/W
})

test_that("dms_to_decimal corrects coordinate errors", {
  # Test error correction
  coords_with_errors <- c("15°65'30\"S")  # Minutes > 60
  
  result <- dms_to_decimal(coords_with_errors, correct_errors = TRUE)
  
  expect_length(result, 1)
  expect_true(!is.na(result))
  
  # Test without error correction
  expect_warning(
    dms_to_decimal(coords_with_errors, correct_errors = FALSE),
    "Minutes out of range"
  )
})

test_that("dms_to_decimal handles invalid inputs", {
  # Test with invalid coordinates
  invalid_coords <- c("invalid", "", NA)
  
  result <- dms_to_decimal(invalid_coords)
  
  expect_length(result, 3)
  expect_true(all(is.na(result)))
})

test_that("dms_to_decimal handles already numeric inputs", {
  # Test with numeric input
  numeric_coords <- c(-15.5, -75.7)
  
  expect_warning(
    result <- dms_to_decimal(numeric_coords),
    "already numeric"
  )
  
  expect_equal(result, numeric_coords)
})

test_that("dms_to_decimal handles empty input", {
  expect_warning(
    result <- dms_to_decimal(character(0)),
    "coordinate vector is empty"
  )
  
  expect_length(result, 0)
})

# Tests for coast_distance()
test_that("coast_distance calculates distances correctly", {
  # Skip if peru_coastline is not available
  skip_if_not_installed("Tivy")
  
  # Test with sample coordinates
  lon <- c(-77.0, -76.5)
  lat <- c(-12.0, -11.5)
  
  result <- coast_distance(
    lon = lon,
    lat = lat,
    distance_type = "haversine",
    unit = "nm"
  )
  
  expect_type(result, "double")
  expect_length(result, 2)
  expect_true(all(!is.na(result)))
  expect_true(all(result > 0))  # Distances should be positive
})

test_that("coast_distance handles different distance types", {
  skip_if_not_installed("Tivy")
  
  lon <- -77.0
  lat <- -12.0
  
  # Test different distance calculation methods
  methods <- c("haversine", "euclidean", "grid")
  
  for (method in methods) {
    result <- coast_distance(
      lon = lon,
      lat = lat,
      distance_type = method,
      unit = "nm"
    )
    
    expect_type(result, "double")
    expect_length(result, 1)
    expect_true(!is.na(result))
    expect_true(result > 0)
  }
})

test_that("coast_distance handles different units", {
  skip_if_not_installed("Tivy")
  
  lon <- -77.0
  lat <- -12.0
  
  # Test different units
  result_nm <- coast_distance(lon = lon, lat = lat, unit = "nm")
  result_km <- coast_distance(lon = lon, lat = lat, unit = "km")
  
  expect_true(result_km > result_nm)  # km should be larger than nm
})

test_that("coast_distance handles invalid coordinates", {
  # Test with invalid coordinates
  lon <- c(NA, 200)  # Invalid longitude
  lat <- c(-12.0, -12.0)
  
  expect_warning(
    result <- coast_distance(lon = lon, lat = lat),
    "Longitude values outside"
  )
})

test_that("coast_distance handles mismatched vector lengths", {
  lon <- c(-77.0, -76.5)
  lat <- c(-12.0)  # Different length
  
  expect_error(
    coast_distance(lon = lon, lat = lat),
    "must have the same length"
  )
})

# Tests for land_points()
test_that("land_points classifies points correctly", {
  skip_if_not_installed("Tivy")
  
  # Test with coordinates that should be in the sea
  x_point <- c(-77.0, -76.5)
  y_point <- c(-12.0, -11.5)
  
  result <- land_points(
    x_point = x_point,
    y_point = y_point
  )
  
  expect_type(result, "character")
  expect_length(result, 2)
  expect_true(all(result %in% c("land", "sea", "unknown")))
})

test_that("land_points handles invalid inputs", {
  # Test with mismatched lengths
  expect_error(
    land_points(x_point = c(-77.0), y_point = c(-12.0, -11.5)),
    "same length"
  )
  
  # Test with non-numeric input
  expect_error(
    land_points(x_point = "invalid", y_point = -12.0),
    "must be numeric"
  )
})

# Tests for helper functions
test_that("find_column works correctly", {
  # Test column finding
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

test_that("find_column handles multiple matches", {
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
  expect_true(length(result) == 3)
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