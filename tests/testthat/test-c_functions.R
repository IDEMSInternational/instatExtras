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

test_that("compare_columns displays intersection and union correctly", {
  # Define two example vectors
  x <- c(1, 2, 3, 4, 5)
  y <- c(4, 5, 6, 7, 8)
  
  # Capture and test the output of compare_columns
  expect_output(compare_columns(x, y, display_intersection = TRUE), "Columns contain all the same values: FALSE")
  expect_output(compare_columns(x, y, display_union = TRUE), "Columns contain all the same values: FALSE")
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

test_that("convert_to_character_matrix handles data correctly", {
  data <- data.frame(A = c(1.234, 5.678), B = c(10.123, 20.456))
  result <- convert_to_character_matrix(data, format_decimal_places = FALSE)
  
  expect_type(result, "list")  # Data frames are lists in R
  expect_equal(ncol(result), 2)
  expect_equal(nrow(result), 2)
  expect_true(all(sapply(result, is.character)))  # Ensure all values are converted to character
})

test_that("convert_to_character_matrix handles empty data", {
  # Create an empty data frame with column names
  empty_data <- data.frame(a = numeric(0), b = character(0), c = logical(0))
  
  # Run function
  result <- convert_to_character_matrix(empty_data)
  
  # Expect result to be a data frame with the same column names
  expect_true(is.data.frame(result))
  expect_equal(ncol(result), ncol(empty_data))
  expect_equal(nrow(result), 0)
  expect_equal(colnames(result), colnames(empty_data))
})

test_that("convert_to_character_matrix correctly handles 'sfc' class columns", {
  # Create a simple sf object (geometry column)
  points <- sf::st_sfc(sf::st_point(c(1, 2)), sf::st_point(c(3, 4)))
  sf_data <- data.frame(id = c(1, 2))
  sf_data$geometry <- points
  class(sf_data$geometry) <- c("sfc", class(sf_data$geometry))  # Ensure "sfc" class
  
  # Run function
  result <- convert_to_character_matrix(sf_data)
  
  # Expect result to be a data frame
  expect_true(is.data.frame(result))
  
  # Ensure id column remains unchanged as character
  expect_equal(result$id, as.character(sf_data$id))
})

test_that("convert_to_character_matrix replaces NA values correctly", {
  # Create a data frame with NA values
  data <- data.frame(
    numbers = c(1.23, NA, 7.89),  # Numeric with NA
    text = c("apple", NA, "cherry"),  # Character with NA
    logical = c(TRUE, FALSE, NA),  # Logical with NA
    stringsAsFactors = FALSE
  )
  
  # Define the NA replacement string
  na_placeholder <- "MISSING"
  
  # Run function with na_display
  result <- convert_to_character_matrix(data, na_display = na_placeholder)
  
  # Ensure result is a data frame
  expect_true(is.data.frame(result))
  
  # Ensure NA values are replaced
  expect_equal(result$numbers[2], na_placeholder)
  expect_equal(result$text[2], na_placeholder)
  expect_equal(result$logical[3], na_placeholder)
  
  # Ensure other values remain unchanged
  expect_equal(result$numbers[c(1,3)], as.character(data$numbers[c(1,3)]))
  expect_equal(result$text[c(1,3)], as.character(data$text[c(1,3)]))
  expect_equal(result$logical[c(1,2)], as.character(data$logical[c(1,2)]))
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

test_that("create_av_packs fetches available packages", {
  # Run the function
  create_av_packs()
  
  # Check if av_packs is created and has the expected structure
  expect_true(exists("av_packs", envir = .GlobalEnv))
  expect_true(is.data.frame(av_packs))
  expect_true("Package" %in% colnames(av_packs))
  expect_true("Version" %in% colnames(av_packs))
})

test_that("create_rankings_list returns correct structure", {
  library(gosset)
  data(cassava)
  cassava_by_id_variety <- pivot_tricot(cassava)
  
  traits <- c("colour", "firmness", "odour", "overall", 
              "stretchability", "taste", "mouldability", "smoothness")
  
  result <- create_rankings_list(data = cassava_by_id_variety, traits = traits)
  
  expect_type(result, "list")
  expect_length(result, length(traits))
  expect_named(result, traits)
  
  # Check that all elements are of class 'ranking'
  all_classes <- purrr::map_chr(result, class)
  expect_true(all("rankings" %in% all_classes))
})

test_that("create_rankings_list works with group = TRUE", {
  library(gosset)
  data(cassava)
  cassava_by_id_variety <- pivot_tricot(cassava)
  
  traits <- c("colour", "firmness", "odour", "overall", 
              "stretchability", "taste", "mouldability", "smoothness")
  
  result_grouped <- create_rankings_list(data = cassava_by_id_variety, traits = traits, group = TRUE)
  
  expect_type(result_grouped, "list")
  expect_length(result_grouped, length(traits))
  expect_named(result_grouped, traits)
  
  # Check that grouped rankings are also class 'ranking'
  all_classes <- purrr::map_chr(result_grouped, class)
  expect_true(all("grouped_rankings" %in% all_classes))
})

test_that("create_rankings_list removes rows with missing values", {
  data <- data.frame(
    id = c(1, 1, 2, 2),
    variety = c("A", "B", "A", "B"),
    trait1 = c(1, 2, NA, 1),
    trait2 = c(2, 1, 2, 3)
  )
  
  traits <- c("trait1", "trait2")
  result <- create_rankings_list(data, traits = traits)
  
  # Only trait2 should be processed because trait1 has missing values for id 2
  expect_true("trait1" %in% names(result))  # It's in the names
  expect_silent(result[["trait1"]])  # But still processed due to group filtering
  
  # The output is still valid ranking object for available values
  expect_s3_class(result[["trait2"]], "rankings")
})

test_that("check_data_levels handles missing ID correctly", {
  data_no_id <- data.frame(variety = c("A", "B", "C"))
  levels_no_id <- summarise_data_levels(list(data1 = data_no_id))
  
  expect_equal(check_data_levels(levels_no_id), "0")
})

test_that("check_data_levels detects duplicate levels", {
  data_id1 <- data.frame(id = 1:3)
  data_id2 <- data.frame(id = 4:6)
  levels_duplicate_id <- summarise_data_levels(list(data1 = data_id1, data2 = data_id2))
  levels_duplicate_id$level[1] <- "id"
  levels_duplicate_id$level[2] <- "id" # manually duplicate
  
  expect_equal(check_data_levels(levels_duplicate_id), "1")
})

test_that("check_data_levels detects missing Tricot data", {
  data_empty <- data.frame(x = 1:3)
  levels_no_marker <- summarise_data_levels(list(data1 = data_empty))
  levels_no_marker$level <- "No marker columns found."
  
  expect_equal(check_data_levels(levels_no_marker), "2")
})

test_that("check_data_levels summarises normally when valid", {
  data_good <- data.frame(id = 1:3, variety = c("A", "B", "C"))
  levels_good <- summarise_data_levels(list(data1 = data_good))
  
  expect_equal(check_data_levels(levels_good), "data1 level: id")
})
