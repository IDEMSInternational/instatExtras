test_that("cancor_coef extracts xcoef and ycoef correctly", {
  data <- list(xcoef = c(0.5, 0.3, -0.2), ycoef = c(0.8, -0.4, 0.6), other = 1)
  result <- cancor_coef(data)
  
  expect_named(result, c("xcoef", "ycoef"))
  expect_equal(result$xcoef, c(0.5, 0.3, -0.2))
  expect_equal(result$ycoef, c(0.8, -0.4, 0.6))
})

test_that("compare_columns correctly identifies differences", {
  x <- 1:10
  y <- 5:14
  
  expect_output(compare_columns(x, y), "Columns contain all the same values: FALSE")
  expect_output(compare_columns(x, y), "Values in first not in second: '1', '2', '3', '4'")
  expect_output(compare_columns(x, y), "Values in second not in first: '11', '12', '13', '14'")
  
  # Case when they are the same
  expect_output(compare_columns(x, x), "Columns contain all the same values: TRUE")
  
  expect_output(compare_columns(x, x, display_intersection = TRUE), "Columns contain all the same values: TRUE")
  expect_output(compare_columns(x, x, display_union = TRUE), "Columns contain all the same values: TRUE")
})

test_that("consecutive_sum computes cumulative sum correctly", {
  expect_equal(consecutive_sum(c(1, 2, 3, 4, 5)), c(1, 3, 6, 10, 15))
  expect_equal(consecutive_sum(c(1, NA, 3, 0, 5), initial_value = 10), c(11, NA, NA, 0, 5))
})

test_that("convert_to_character_matrix handles data correctly", {
  data <- data.frame(A = c(1.234, 5.678), B = c(10.123, 20.456))
  result <- convert_to_character_matrix(data)
  
  expect_type(result, "list")  # Data frames are lists in R
  expect_equal(ncol(result), 2)
  expect_equal(nrow(result), 2)
  expect_true(all(sapply(result, is.character)))  # Ensure all values are converted to character
})

test_that("create_av_packs retrieves package data correctly", {
  create_av_packs()

  expect_true(exists("av_packs"), info = "av_packs object should exist in the global environment.")
  expect_true("Package" %in% colnames(av_packs), info = "Data frame should contain a 'Package' column.")
  expect_true("Version" %in% colnames(av_packs), info = "Data frame should contain a 'Version' column.")
})

test_that("convert_SST works correctly", {
  # Create a mock data file
  mock_data <- data.frame(
    X2 = c("Year", "Lon", -5, -5, -5),
    X3 = c("2020", 5, 25, 30, 35),
    X4 = c("2021", 10, 30, 35, 40)
  )
  
  # Run convert_SST
  converted_data <- convert_SST(mock_data, data_from = 5)
  
  # Extract outputs
  my_data <- converted_data[[1]]
  lat_lon_df <- converted_data[[2]]
  
  # Check output structure
  expect_true(is.data.frame(my_data))
  expect_true(is.data.frame(lat_lon_df))
  
  # Check column names
  expect_equal(colnames(my_data), c("period", "lat", "lon", "station", "SST_value"))
  expect_equal(colnames(lat_lon_df), c("lat", "lon", "station"))
  
  # Check that SST values are numeric
  expect_true(is.numeric(my_data$SST_value))
})
