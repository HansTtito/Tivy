# Helper functions to create sample data
create_sample_hauls <- function() {
  data.frame(
    codigo_faena = c("F001", "F002", "F003"),
    numero_cala = c(1, 2, 3),
    fecha_inicio = c("2024-01-15 08:00", "2024-01-15 10:00", "2024-01-15 12:00"),
    latitud_inicial = c("15°30'S", "15°45'S", "16°00'S"),
    longitud_inicial = c("75°30'W", "75°45'W", "76°00'W"),
    especie = c("ANCHOVETA", "ANCHOVETA", "SARDINA"),
    captura = c(1500, 2000, 1200),
    fecha_registro = c("2024-01-15", "2024-01-15", "2024-01-15"),
    stringsAsFactors = FALSE
  )
}

create_sample_trips <- function() {
  data.frame(
    codigo_faena = c("F001", "F002", "F003"),
    embarcacion = c("VESSEL_A", "VESSEL_B", "VESSEL_C"),
    id_embarcacion = c("PE-123", "PE-456", "PE-789"),
    fecha_inicio_faena = c("2024-01-15 06:00", "2024-01-15 07:00", "2024-01-15 08:00"),
    fecha_fin_faena = c("2024-01-15 18:00", "2024-01-15 19:00", "2024-01-15 20:00"),
    armador = c("OWNER_A", "OWNER_B", "OWNER_C"),
    stringsAsFactors = FALSE
  )
}

create_sample_length <- function() {
  data.frame(
    codigo_faena = c("F001", "F001", "F001", "F002", "F002"),
    numero_cala = c(1, 1, 1, 2, 2),
    especie = c("ANCHOVETA", "ANCHOVETA", "ANCHOVETA", "ANCHOVETA", "ANCHOVETA"),
    talla = c(8, 9, 10, 8, 9),
    frecuencia = c(10, 25, 15, 8, 20),
    stringsAsFactors = FALSE
  )
}

# Tests for process_hauls()
test_that("process_hauls works correctly", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("stringi")
  
  sample_data <- create_sample_hauls()
  
  result <- process_hauls(
    data_hauls = sample_data,
    correct_coordinates = TRUE,
    verbose = FALSE
  )
  
  # Check that result is a data frame
  expect_s3_class(result, "data.frame")
  
  # Check that all required columns are present
  expected_cols <- c("fishing_trip_code", "haul_number", "start_date", 
                    "start_latitude", "start_longitude", "species", 
                    "catch", "registration_date")
  expect_true(all(expected_cols %in% names(result)))
  
  # Check that coordinates were converted to decimal
  expect_true(is.numeric(result$lat_initial))
  expect_true(is.numeric(result$lon_initial))
  
  # Check that dates were converted properly
  expect_s3_class(result$start_date_haul, "POSIXct")
  expect_s3_class(result$registration_date, "POSIXct")
  
  # Check data integrity
  expect_equal(nrow(result), nrow(sample_data))
  expect_equal(result$fishing_trip_code, sample_data$codigo_faena)
})

test_that("process_hauls handles missing critical columns", {
  incomplete_data <- data.frame(
    codigo_faena = c("F001", "F002"),
    # Missing other required columns
    stringsAsFactors = FALSE
  )
  
  expect_error(
    process_hauls(incomplete_data, verbose = FALSE),
    "Critical columns not found"
  )
})

test_that("process_hauls handles empty data", {
  empty_data <- data.frame()
  
  expect_error(
    process_hauls(empty_data, verbose = FALSE),
    "Input data frame is empty"
  )
})

test_that("process_hauls handles non-data.frame input", {
  expect_error(
    process_hauls("not a data frame", verbose = FALSE),
    "'data_hauls' must be a data frame"
  )
})

# Tests for process_fishing_trips()
test_that("process_fishing_trips works correctly", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("stringi")
  
  sample_data <- create_sample_trips()
  
  result <- expect_warning(
    process_fishing_trips(
    data_fishing_trips = sample_data,
    verbose = FALSE
  ),
  "Multiple columns found for pattern"
  )
  # Check structure
  expect_s3_class(result, "data.frame")
  
  # Check required columns
  expected_cols <- c("fishing_trip_code", "vessel", "id_vessel", 
                    "start_date_fishing_trip", "end_date_fishing_trip", "owner")
  expect_true(all(expected_cols %in% names(result)))
  
  # Check data types
  expect_s3_class(result$start_date_fishing_trip, "POSIXct")
  expect_s3_class(result$end_date_fishing_trip, "POSIXct")
  
  # Check data integrity
  expect_equal(nrow(result), nrow(sample_data))
})

test_that("process_fishing_trips handles missing critical columns", {
  incomplete_data <- data.frame(
    codigo_faena = c("F001"),
    # Missing other required columns
    stringsAsFactors = FALSE
  )
  
  expect_error(
    process_fishing_trips(incomplete_data, verbose = FALSE),
    "Critical columns not found"
  )
})

test_that("process_fishing_trips handles empty data", {
  empty_data <- data.frame()
  
  expect_error(
    process_fishing_trips(empty_data, verbose = FALSE),
    "Input data frame is empty"
  )
})

# Tests for process_length()
test_that("process_length works correctly", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("stringi")
  skip_if_not_installed("tidyr")
  
  sample_data <- create_sample_length()
  
  result <- process_length(
    data_length = sample_data,
    verbose = FALSE
  )
  
  # Check structure
  expect_s3_class(result, "data.frame")
  
  # Check that data was pivoted to wide format
  expect_true("8" %in% names(result))
  expect_true("9" %in% names(result))
  expect_true("10" %in% names(result))
  
  # Check fixed columns
  fixed_cols <- c("fishing_trip_code", "haul_number", "species")
  expect_true(all(fixed_cols %in% names(result)))
  
  # Check that frequencies were preserved
  expect_equal(result[result$fishing_trip_code == "F001" & 
                     result$haul_number == 1, "8"], 10)
  expect_equal(result[result$fishing_trip_code == "F001" & 
                     result$haul_number == 1, "9"], 25)
})

test_that("process_length handles missing required columns", {
  incomplete_data <- data.frame(
    codigo_faena = c("F001"),
    talla = c(8)
    # Missing other required columns
  )
  
  expect_error(
    process_length(incomplete_data, verbose = FALSE),
    "Required columns not found"
  )
})

test_that("process_length handles empty data", {
  empty_data <- data.frame()
  
  expect_error(
    process_length(empty_data, verbose = FALSE),
    "Input data frame is empty"
  )
})

# Tests for merge_length_fishing_trips_hauls()
test_that("merge_length_fishing_trips_hauls works correctly", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("stringi")
  skip_if_not_installed("tidyr")
  
  # Create processed sample data
  hauls <- process_hauls(create_sample_hauls(), verbose = FALSE)
  trips <- expect_warning(process_fishing_trips(create_sample_trips(), verbose = FALSE), "Multiple columns found for pattern")
  lengths <- process_length(create_sample_length(), verbose = FALSE)
  
  # Merge trips and lengths first
  length_trips <- merge(lengths, trips, by = "fishing_trip_code", all = TRUE)
  
  # Test the main merge function
  result <- merge_length_fishing_trips_hauls(
    data_hauls = hauls,
    data_length_fishing_trips = length_trips
  )
  
  # Check structure
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  
  # Check that data from all sources is present
  expect_true("vessel" %in% names(result))  # From trips
  expect_true("8" %in% names(result) || "9" %in% names(result))  # From lengths
  expect_true("lat_initial" %in% names(result))  # From hauls
})

test_that("merge_length_fishing_trips_hauls handles missing parameters", {
  expect_error(
    merge_length_fishing_trips_hauls(),
    "The 'data_hauls' parameter is required"
  )
  
  hauls <- process_hauls(create_sample_hauls(), verbose = FALSE)
  
  expect_error(
    merge_length_fishing_trips_hauls(data_hauls = hauls),
    "The 'data_length_fishing_trips' parameter is required"
  )
})

test_that("merge_length_fishing_trips_hauls handles empty data", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("stringi")
  skip_if_not_installed("tidyr")
  
  hauls <- process_hauls(create_sample_hauls(), verbose = FALSE)
  
  # Empty length_trips data
  empty_length_trips <- data.frame(
    fishing_trip_code = character(0),
    haul_number = integer(0),
    species = character(0)
  )
  
  expect_error(
    merge_length_fishing_trips_hauls(
      data_hauls = hauls,
      data_length_fishing_trips = empty_length_trips
    ),
    "'data_length_fishing_trips' is empty"
  )
})

# Tests for validation functions
test_that("validate_haul_data works correctly", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("stringi")
  
  sample_data <- create_sample_hauls()
  processed_data <- process_hauls(sample_data, verbose = FALSE)
  
  validation <- validate_haul_data(processed_data)
  
  # Check structure
  expect_type(validation, "list")
  expect_true("total_records" %in% names(validation))
  expect_true("quality_score" %in% names(validation))
  
  # Check values
  expect_equal(validation$total_records, nrow(processed_data))
  expect_true(validation$quality_score >= 0 && validation$quality_score <= 100)
})

test_that("validate_haul_data handles invalid input", {
  expect_error(
    validate_haul_data("not a data frame"),
    "Input must be a data frame"
  )
})

test_that("validate_fishing_trip_data works correctly", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("stringi")

  processed_data <- expect_warning(process_fishing_trips(create_sample_trips(), verbose = FALSE), "Multiple columns found for pattern")

  validation <- validate_fishing_trip_data(processed_data)
  
  # Check structure
  expect_type(validation, "list")
  expect_true("total_records" %in% names(validation))
  expect_true("quality_score" %in% names(validation))
  
  # Check values
  expect_equal(validation$total_records, nrow(processed_data))
})

test_that("validate_fishing_trip_data handles invalid input", {
  expect_error(
    validate_fishing_trip_data("not a data frame"),
    "Input must be a data frame"
  )
})

test_that("validate_length_data works correctly", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("stringi")
  skip_if_not_installed("tidyr")
  
  sample_data <- create_sample_length()
  processed_data <- process_length(sample_data, verbose = FALSE)
  
  validation <- validate_length_data(processed_data)
  
  # Check structure
  expect_type(validation, "list")
  expect_true("total_records" %in% names(validation))
  expect_true("quality_score" %in% names(validation))
  expect_true("total_length_classes" %in% names(validation))
})

test_that("validate_length_data handles invalid input", {
  expect_error(
    validate_length_data("not a data frame"),
    "Input must be a data frame"
  )
})

# Integration test
test_that("data processing workflow integration", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("stringi")
  skip_if_not_installed("tidyr")
  
  # Test complete workflow
  raw_hauls <- create_sample_hauls()
  raw_trips <- create_sample_trips()
  raw_lengths <- create_sample_length()
  
  # Process each dataset
  processed_hauls <- process_hauls(raw_hauls, verbose = FALSE)
  processed_trips <- expect_warning(process_fishing_trips(raw_trips, verbose = FALSE), "Multiple columns found for pattern")
  processed_lengths <- process_length(raw_lengths, verbose = FALSE)
  
  # Merge data
  length_trips <- merge(processed_lengths, processed_trips, by = "fishing_trip_code", all = TRUE)
  final_data <- merge_length_fishing_trips_hauls(
    data_hauls = processed_hauls,
    data_length_fishing_trips = length_trips
  )
  
  # Validate results
  haul_validation <- validate_haul_data(processed_hauls)
  trip_validation <- validate_fishing_trip_data(processed_trips)
  length_validation <- validate_length_data(processed_lengths)
  
  # Check that workflow completed successfully
  expect_s3_class(final_data, "data.frame")
  expect_true(nrow(final_data) > 0)
  expect_true(haul_validation$quality_score >= 0)
  expect_true(trip_validation$quality_score >= 0)
  expect_true(length_validation$quality_score >= 0)
})