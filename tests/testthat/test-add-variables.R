# Tests for add_variables function (03-add-variables.R)

# Helper function to create sample data with length columns
create_sample_data_with_lengths <- function() {
  data.frame(
    fishing_trip_code = c("F001", "F002", "F003"),
    haul_number = c(1, 2, 3),
    lon_initial = c(-77.0, -76.5, -76.0),
    lat_initial = c(-12.0, -11.5, -11.0),
    species = c("ANCHOVETA", "ANCHOVETA", "SARDINA"),
    `8` = c(10, 5, 0),
    `9` = c(20, 15, 8),
    `10` = c(30, 25, 12),
    `11` = c(25, 20, 15),
    `12` = c(15, 18, 20),
    `13` = c(10, 12, 18),
    `14` = c(5, 8, 15),
    `15` = c(2, 5, 10),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

# Sample coastline data
create_sample_coastline <- function() {
  data.frame(
    Long = c(-77.5, -77.0, -76.5, -76.0, -75.5),
    Lat = c(-12.5, -12.0, -11.5, -11.0, -10.5)
  )
}

test_that("add_variables calculates juvenile proportion correctly", {
  sample_data <- create_sample_data_with_lengths()
  coastline <- create_sample_coastline()
  
  result <- add_variables(
    data = sample_data,
    JuvLim = 12,
    coastline = coastline,
    suppress_warnings = TRUE
  )
  
  # Check that result is a data frame
  expect_s3_class(result, "data.frame")
  
  # Check that new columns were added
  expect_true("juv" %in% names(result))
  expect_true("sample" %in% names(result))
  expect_true("dc" %in% names(result))
  expect_true("dc_cat" %in% names(result))
  
  # Check that juvenile proportion is calculated
  expect_type(result$juv, "double")
  expect_true(all(result$juv >= 0 & result$juv <= 100, na.rm = TRUE))
  
  # Check that sample size is calculated
  expect_type(result$sample, "double")
  expect_true(all(result$sample >= 0, na.rm = TRUE))
})

test_that("add_variables handles missing coordinates", {
  sample_data <- data.frame(
    fishing_trip_code = c("F001", "F002"),
    # Missing lon_initial and lat_initial
    `8` = c(10, 5),
    `9` = c(20, 15),
    check.names = FALSE
  )
  
  expect_error(
    add_variables(sample_data),
    "Missing required columns"
  )
})

test_that("add_variables handles data without length columns", {
  sample_data <- data.frame(
    fishing_trip_code = c("F001", "F002"),
    lon_initial = c(-77.0, -76.5),
    lat_initial = c(-12.0, -11.5),
    species = c("ANCHOVETA", "SARDINA")
  )
  
  expect_warning(
    result <- add_variables(sample_data, suppress_warnings = FALSE),
    "No length columns with numeric names found"
  )
  
  expect_equal(result, sample_data)
})

test_that("add_variables categorizes distances correctly", {
  sample_data <- create_sample_data_with_lengths()
  coastline <- create_sample_coastline()
  
  result <- add_variables(
    data = sample_data,
    coastline = coastline,
    suppress_warnings = TRUE
  )
  
  # Check distance categories
  valid_categories <- c("05-15 nm", "15-30 nm", "30-50 nm", "50-100 nm", NA_character_)
  expect_true(all(result$dc_cat %in% valid_categories, na.rm = TRUE))
})

test_that("add_variables handles zero frequencies", {
  sample_data <- create_sample_data_with_lengths()
  # Set all frequencies to zero for one row
  sample_data[1, c("8", "9", "10", "11", "12", "13", "14", "15")] <- 0
  coastline <- create_sample_coastline()
  
  result <- add_variables(
    data = sample_data,
    coastline = coastline,
    suppress_warnings = TRUE
  )
  
  # Check that juvenile proportion is NA for zero frequency row
  expect_true(is.na(result$juv[1]))
  expect_equal(result$sample[1], 0)
})

test_that("add_variables handles different juvenile limits", {
  sample_data <- create_sample_data_with_lengths()
  coastline <- create_sample_coastline()
  
  # Test with different juvenile limits
  result_10 <- add_variables(sample_data, JuvLim = 10, coastline = coastline, suppress_warnings = TRUE)
  result_14 <- add_variables(sample_data, JuvLim = 14, coastline = coastline, suppress_warnings = TRUE)
  
  # Juvenile proportion should be higher with higher limit
  expect_true(all(result_14$juv >= result_10$juv, na.rm = TRUE))
})

test_that("add_variables handles custom coastline", {
  sample_data <- create_sample_data_with_lengths()
  custom_coastline <- data.frame(
    Long = c(-78, -77, -76),
    Lat = c(-13, -12, -11)
  )
  
  result <- add_variables(
    data = sample_data,
    coastline = custom_coastline,
    suppress_warnings = TRUE
  )
  
  expect_s3_class(result, "data.frame")
  expect_true("dc" %in% names(result))
})

test_that("add_variables handles invalid coastline", {
  sample_data <- create_sample_data_with_lengths()
  invalid_coastline <- "not a data frame"
  
  expect_error(
    add_variables(sample_data, coastline = invalid_coastline),
    "must be a data.frame"
  )
})