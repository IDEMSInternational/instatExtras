test_that("dekade correctly calculates dekade periods", {
  expect_equal(dekade(as.Date("2020-12-25")), 36)  # December, 3rd dekade
  expect_equal(dekade(as.Date("1999-01-01")), 1)   # January, 1st dekade
  expect_equal(dekade(as.Date("2023-07-15")), 20)  # July, 2nd dekade
})

test_that("drop_unused_levels removes unused levels", {
  df <- data.frame(A = factor(c("apple", "banana", "apple", "orange")),
                   B = factor(c("red", "blue", "green", "red")),
                   C = c(1, 2, 3, 4))
  
  df$A <- droplevels(df$A)  # Reference for expected result
  modified_df <- drop_unused_levels(df, columns = "A")
  
  expect_equal(levels(modified_df$A), levels(df$A))
})

test_that("duplicated_cases correctly assigns unique IDs", {
  col <- c(1, 2, 2.01, 3, 4, 4.005, 4.01, 4.02)
  result <- duplicated_cases(col)
  
  expect_equal(result, c(1, 1, 2, 1, 1, 2, 3, 4))
  
  col_str <- c("A", "A", "B", "C", "C", "D", "E")
  result_str <- duplicated_cases(col_str)
  
  expect_equal(result_str, c(1, 2, 1, 1, 2, 1, 1))
})

test_that("duplicated_count_index correctly identifies duplicate counts", {
  data <- data.frame(a = c(1, 2, 3, 2, 1, 3, 4))
  
  count_result <- duplicated_count_index(data, type = "count")
  expect_equal(count_result, c(1, 1, 1, 1, 1, 1, 0))
  
  index_result <- duplicated_count_index(data, type = "index")
  expect_equal(index_result, c(1, 1, 1, 2, 2, 2, 1))
})

test_that("duplicated_cases handles NA values correctly", {
  col_name <- c(1, NA, 2, 2.01, 2.1, NA, 3)  # Includes NA values
  result <- duplicated_cases(col_name, tolerance = 0.01)
  expect_equal(result, c(1, NA, 1, 2, 1, NA, 1))
  
  col_name <- as.character(col_name)  # Includes NA values
  result <- duplicated_cases(col_name, tolerance = 0.01)
  expect_equal(result, c(1, NA, 1, 1, 1, NA, 1))
})

# detect_tricot_structure
test_that("detect_tricot_structure finds option, trait, and rank structure", {
  df <- dplyr::tibble(
    id = 1:3,
    option_a = c("x", "y", "z"),
    option_b = c("x", "y", "z"),
    option_c = c("y", "x", "z"),
    colour_pos = c("A", "B", "C"),
    taste_neg = c("C", "B", "Not observed")
  )
  
  res <- detect_tricot_structure(df)
  
  expect_equal(res$option_cols, c("option_a", "option_b", "option_c"))
  expect_true("_pos" %in% res$trait_good_cols)
  expect_true("_neg" %in% res$trait_bad_cols)
  expect_equal(sort(res$ranks), c("A", "B", "C"))
})
