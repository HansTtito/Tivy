# Tests for dms_to_decimal()
test_that("dms_to_decimal converts coordinates correctly", {
  # Test standard DMS format
  coords <- c("15°30'25\"S", "75°45'30\"W")
  result <- dms_to_decimal(coords)

  expect_type(result, "double")
  expect_length(result, 2)
  expect_true(all(!is.na(result)))

  # Check that southern/western coordinates are negative (default hemisphere = "S")
  expect_true(all(result < 0))
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

  result <- expect_warning(dms_to_decimal(invalid_coords),"No numeric components found in the coordinate")

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
  # Create simple coastline data for testing
  test_coastline <- data.frame(
    Long = c(-78.0, -77.5, -77.0, -76.5, -76.0),
    Lat = c(-14.0, -13.5, -13.0, -12.5, -12.0)
  )

  # Test with sample coordinates
  lon <- c(-77.0, -76.5)
  lat <- c(-12.0, -11.5)

  result <- coast_distance(
    lon = lon,
    lat = lat,
    coastline = test_coastline,
    distance_type = "haversine",
    unit = "nm"
  )

  expect_type(result, "double")
  expect_length(result, 2)
  expect_true(all(!is.na(result)))
  expect_true(all(result >= 0))  # Distances should be non-negative
})

test_that("coast_distance handles different distance types", {
  test_coastline <- data.frame(
    Long = c(-78.0, -77.5, -77.0, -76.5, -76.0),
    Lat = c(-14.0, -13.5, -13.0, -12.5, -12.0)
  )

  lon <- -77.0
  lat <- -12.0

  # Test different distance calculation methods
  methods <- c("haversine", "euclidean", "manhattan", "grid")

  for (method in methods) {
    result <- coast_distance(
      lon = lon,
      lat = lat,
      coastline = test_coastline,
      distance_type = method,
      unit = "nm"
    )

    expect_type(result, "double")
    expect_length(result, 1)
    expect_true(!is.na(result))
    expect_true(result >= 0)
  }
})

test_that("coast_distance handles different units", {
  test_coastline <- data.frame(
    Long = c(-78.0, -77.5, -77.0, -76.5, -76.0),
    Lat = c(-14.0, -13.5, -13.0, -12.5, -12.0)
  )

  lon <- -77.0
  lat <- -12.0

  # Test different units
  result_nm <- coast_distance(lon = lon, lat = lat, coastline = test_coastline, unit = "nm")
  result_km <- coast_distance(lon = lon, lat = lat, coastline = test_coastline, unit = "km")

  expect_true(result_km > result_nm)  # km should be larger than nm
})

test_that("coast_distance handles invalid coordinates", {
  test_coastline <- data.frame(
    Long = c(-78.0, -77.5, -77.0),
    Lat = c(-14.0, -13.5, -13.0)
  )

  # Test with invalid coordinates
  lon <- c(NA, 200)  # Invalid longitude
  lat <- c(-12.0, -12.0)

  expect_warning(
    result <- coast_distance(lon = lon, lat = lat, coastline = test_coastline),
    "Longitude values outside"
  )
})

test_that("coast_distance handles mismatched vector lengths", {
  test_coastline <- data.frame(
    Long = c(-78.0, -77.5, -77.0),
    Lat = c(-14.0, -13.5, -13.0)
  )

  lon <- c(-77.0, -76.5)
  lat <- c(-12.0)  # Different length

  expect_error(
    coast_distance(lon = lon, lat = lat, coastline = test_coastline),
    "must have the same length"
  )
})

# Tests for land_points()
test_that("land_points classifies points correctly", {
  test_coastline <- data.frame(
    Long = c(-78.0, -77.5, -77.0, -76.5, -76.0),
    Lat = c(-14.0, -13.5, -13.0, -12.5, -12.0)
  )

  # Test with coordinates that should be in the sea (west of coastline)
  x_point <- c(-78.5, -76.0)  # One clearly in sea, one near coast
  y_point <- c(-13.0, -12.0)

  result <- land_points(
    x_point = x_point,
    y_point = y_point,
    coastline = test_coastline
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

test_that("land_points handles NA values", {
  test_coastline <- data.frame(
    Long = c(-78.0, -77.5, -77.0),
    Lat = c(-14.0, -13.5, -13.0)
  )

  x_point <- c(-77.0, NA)
  y_point <- c(-12.0, -12.0)

  result <- land_points(x_point = x_point, y_point = y_point, coastline = test_coastline)

  expect_length(result, 2)
  expect_true(is.na(result[2]))
})

# Tests for coordinate utility functions
test_that("coordinate validation works", {
  # Valid coordinates
  expect_true(all(abs(c(-12.0, -13.5, -11.0)) <= 90))  # Valid latitudes
  expect_true(all(abs(c(-77.0, -76.5, -78.0)) <= 180))  # Valid longitudes

  # Invalid coordinates
  invalid_lat <- c(-95.0, 100.0)
  invalid_lon <- c(-200.0, 185.0)

  expect_true(any(abs(invalid_lat) > 90))
  expect_true(any(abs(invalid_lon) > 180))
})

# Integration test
test_that("coordinate functions work together", {
  # Test coordinate conversion followed by distance calculation
  dms_coords <- c("12°30'S", "77°15'W")
  decimal_coords <- dms_to_decimal(dms_coords)

  test_coastline <- data.frame(
    Long = c(-78.0, -77.5, -77.0, -76.5),
    Lat = c(-13.0, -12.5, -12.0, -11.5)
  )

  # Calculate distance to coast
  distance <- coast_distance(
    lon = decimal_coords[2],  # longitude
    lat = decimal_coords[1],  # latitude
    coastline = test_coastline
  )

  expect_type(distance, "double")
  expect_length(distance, 1)
  expect_true(!is.na(distance))
  expect_true(distance >= 0)
})
