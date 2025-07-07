# Tests for fishery statistics functions (04-fishery-statistics.R)

# Tests for calculate_fish_weight()
test_that("calculate_fish_weight works correctly", {
  lengths <- c(8, 10, 12, 15)
  a <- 0.0001
  b <- 2.984
  
  result <- calculate_fish_weight(length = lengths, a = a, b = b)
  
  expect_type(result, "double")
  expect_length(result, 4)
  expect_true(all(result > 0))
  expect_true(all(!is.na(result)))
  
  # Check that larger fish have more weight
  expect_true(result[4] > result[1])
})

test_that("calculate_fish_weight handles invalid inputs", {
  # Test non-numeric length
  expect_error(
    calculate_fish_weight(length = "invalid", a = 0.0001, b = 2.984),
    "'length' must be numeric"
  )
  
  # Test non-numeric parameters
  expect_error(
    calculate_fish_weight(length = 10, a = "invalid", b = 2.984),
    "'a' must be numeric"
  )
  
  expect_error(
    calculate_fish_weight(length = 10, a = 0.0001, b = "invalid"),
    "'b' must be numeric"
  )
})

test_that("calculate_fish_weight warns for problematic values", {
  # Test negative lengths
  expect_warning(
    calculate_fish_weight(length = c(-5, 10), a = 0.0001, b = 2.984),
    "Length values <= 0 detected"
  )
  
  # Test negative 'a' parameter
  expect_warning(
    calculate_fish_weight(length = 10, a = -0.0001, b = 2.984),
    "Value of 'a' <= 0"
  )
})

# Tests for weight_by_catch()
test_that("weight_by_catch works correctly", {
  frequency <- c(0, 4, 8, 16, 12, 23, 34, 55, 35, 24, 15, 6, 0)
  lengths <- seq(8, 20, by = 1)
  catch <- 1000
  a <- 0.0001
  b <- 2.984
  
  result <- weight_by_catch(
    frequency = frequency,
    catch = catch,
    length = lengths,
    a = a,
    b = b,
    silence_warnings = TRUE
  )
  
  expect_type(result, "double")
  expect_length(result, length(frequency))
  expect_true(all(result >= 0, na.rm = TRUE))
})

test_that("weight_by_catch handles invalid inputs", {
  frequency <- c(10, 20, 30)
  lengths <- c(8, 9, 10)
  
  # Test mismatched lengths
  expect_error(
    weight_by_catch(
      frequency = frequency,
      catch = 1000,
      length = c(8, 9),  # Different length
      a = 0.0001,
      b = 2.984
    ),
    "must have the same length"
  )
})

test_that("weight_by_catch handles edge cases", {
  frequency <- c(0, 0, 0)  # All zeros
  lengths <- c(8, 9, 10)
  
  expect_warning(
    result <- weight_by_catch(
      frequency = frequency,
      catch = 1000,
      length = lengths,
      a = 0.0001,
      b = 2.984,
      silence_warnings = FALSE
    ),
    "Sum of frequencies is zero"
  )
  
  expect_equal(result, c(0, 0, 0))
})

test_that("weight_by_catch handles invalid catch", {
  frequency <- c(10, 20, 30)
  lengths <- c(8, 9, 10)
  
  expect_warning(
    weight_by_catch(
      frequency = frequency,
      catch = -100,  # Negative catch
      length = lengths,
      a = 0.0001,
      b = 2.984,
      silence_warnings = FALSE
    ),
    "Catch value is NA or <= 0"
  )
})

# Tests for calculate_juvenile_percentage()
test_that("calculate_juvenile_percentage works correctly", {
  frequency <- c(0, 4, 8, 16, 12, 23, 34, 55, 35, 24, 15, 6, 0)
  lengths <- seq(8, 20, by = 1)
  juvenile_limit <- 12
  
  result <- calculate_juvenile_percentage(
    frequency = frequency,
    length = lengths,
    juvenile_limit = juvenile_limit,
    silence_warnings = TRUE
  )
  
  expect_type(result, "double")
  expect_length(result, 1)
  expect_true(result >= 0 && result <= 100)
  expect_true(!is.na(result))
})

test_that("calculate_juvenile_percentage handles edge cases", {
  frequency <- c(0, 0, 0)  # All zeros
  lengths <- c(8, 9, 10)
  
  expect_warning(
    result <- calculate_juvenile_percentage(
      frequency = frequency,
      length = lengths,
      juvenile_limit = 10,
      silence_warnings = FALSE
    ),
    "Sum of frequencies is zero"
  )
  
  expect_true(is.na(result))
})

test_that("calculate_juvenile_percentage validates inputs", {
  frequency <- c(10, 20, 30)
  lengths <- c(8, 9, 10)
  
  # Test mismatched lengths
  expect_error(
    calculate_juvenile_percentage(
      frequency = frequency,
      length = c(8, 9),  # Different length
      juvenile_limit = 10
    ),
    "must have the same length"
  )
  
  # Test non-numeric inputs
  expect_error(
    calculate_juvenile_percentage(
      frequency = "invalid",
      length = lengths,
      juvenile_limit = 10
    ),
    "'frequency' must be numeric"
  )
})

# Tests for get_length_range()
test_that("get_length_range works correctly", {
  frequency <- c(0, 0, 1, 2, 3, 4, 2, 1, 0)
  lengths <- c(5, 6, 7, 8, 9, 10, 11, 12, 13)
  
  min_length <- get_length_range(frequency, lengths, type = "min")
  max_length <- get_length_range(frequency, lengths, type = "max")
  
  expect_type(min_length, "double")
  expect_type(max_length, "double")
  expect_equal(min_length, 7)
  expect_equal(max_length, 12)
  expect_true(max_length >= min_length)
})

test_that("get_length_range handles no positive frequencies", {
  frequency <- c(0, 0, 0)
  lengths <- c(8, 9, 10)
  
  expect_warning(
    result <- get_length_range(frequency, lengths, type = "min"),
    "No positive frequencies found"
  )
  
  expect_true(is.na(result))
})

test_that("get_length_range validates type parameter", {
  frequency <- c(1, 2, 3)
  lengths <- c(8, 9, 10)
  
  expect_error(
    get_length_range(frequency, lengths, type = "invalid"),
    "'type' must be 'min' or 'max'"
  )
})

# Tests for convert_numbers_to_weight()
test_that("convert_numbers_to_weight works correctly", {
  data <- data.frame(
    fishing_trip_code = c("F001", "F002"),
    `8` = c(10, 5),
    `9` = c(20, 15),
    `10` = c(30, 25),
    check.names = FALSE
  )
  
  length_cols <- c("8", "9", "10")
  
  result <- convert_numbers_to_weight(
    data = data,
    length_cols = length_cols,
    a = 0.0001,
    b = 2.984
  )
  
  expect_s3_class(result, "data.frame")
  expect_true("weight_8" %in% names(result))
  expect_true("weight_9" %in% names(result))
  expect_true("weight_10" %in% names(result))
  
  # Check that weights are positive
  expect_true(all(result$weight_8 >= 0, na.rm = TRUE))
  expect_true(all(result$weight_9 >= 0, na.rm = TRUE))
  expect_true(all(result$weight_10 >= 0, na.rm = TRUE))
})

test_that("convert_numbers_to_weight handles missing columns", {
  data <- data.frame(
    fishing_trip_code = c("F001", "F002"),
    `8` = c(10, 5)
  )
  
  length_cols <- c("8", "9", "10")  # "9" and "10" don't exist
  
  expect_error(
    convert_numbers_to_weight(data, length_cols, a = 0.0001, b = 2.984),
    "Missing columns in data"
  )
})

# Tests for summarize_juveniles_by_group()
test_that("summarize_juveniles_by_group works correctly", {
  data <- data.frame(
    date = as.Date(c("2024-01-01", "2024-01-01", "2024-01-02", "2024-01-02")),
    vessel = c("A", "B", "A", "B"),
    `8` = c(10, 5, 8, 12),
    `9` = c(20, 15, 16, 18),
    `10` = c(30, 25, 24, 22),
    `11` = c(25, 20, 20, 15),
    `12` = c(15, 18, 18, 10),
    `13` = c(10, 12, 12, 8),
    `14` = c(5, 8, 8, 5),
    `15` = c(2, 5, 4, 3),
    check.names = FALSE
  )
  
  length_cols <- c("8", "9", "10", "11", "12", "13", "14", "15")
  
  result <- summarize_juveniles_by_group(
    data = data,
    group_cols = "date",
    length_cols = length_cols,
    juvenile_limit = 12,
    verbose = FALSE
  )
  
  expect_s3_class(result, "data.frame")
  expect_true("perc_juv_number" %in% names(result))
  expect_true("perc_juv_weight" %in% names(result))
  expect_true("total_number" %in% names(result))
  expect_true("total_weight" %in% names(result))
  
  expect_equal(nrow(result), 2)  # Two dates
})

test_that("summarize_juveniles_by_group auto-detects length columns", {
  data <- data.frame(
    date = as.Date(c("2024-01-01", "2024-01-02")),
    length_8 = c(10, 8),
    length_9 = c(20, 16),
    length_10 = c(30, 24)
  )
  
  result <- summarize_juveniles_by_group(
    data = data,
    group_cols = "date",
    length_cols = NULL,  # Auto-detect
    verbose = FALSE
  )
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
})

test_that("summarize_juveniles_by_group handles no length columns found", {
  data <- data.frame(
    date = as.Date(c("2024-01-01", "2024-01-02")),
    catch = c(100, 200)
  )
  
  expect_error(
    summarize_juveniles_by_group(
      data = data,
      group_cols = "date",
      length_cols = NULL,
      verbose = FALSE
    ),
    "No length columns found"
  )
})