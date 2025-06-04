
test_that("find_data_level works with full ID-Variety-Trait structure", {
  df <- dplyr::tibble(
    ID = rep(1:3, each = 2),
    item = rep(c("A", "B"), 3),
    trait = c("height", "height", "yield", "yield", "height", "height"),
    value = 1:6
  )
  res <- find_data_level(df)
  expect_equal(res$id_col, "ID")
  expect_equal(res$variety_col, "item")
  expect_equal(res$trait_col, "trait")
})

test_that("find_data_level falls back to id level", {
  df <- dplyr::tibble(id = 1:5)
  res <- find_data_level(df)
  expect_equal(res$level, "id")
  expect_equal(res$id_col, "id")
  expect_true(length(res$variety_col) == 0)
  expect_true(length(res$trait_col) == 0)
})

test_that("find_data_level handles duplicate columns (chooses unique)", {
  df <- dplyr::tibble(
    participant_id = 1:5,
    ID = c(1, 1, 2, 2, 3)  # not unique
  )
  res <- find_data_level(df)
  expect_equal(res$id_col, "participant_id")  # chosen for uniqueness
})

test_that("find_data_level handles no matching columns", {
  df <- dplyr::tibble(x = 1:5)
  res <- find_data_level(df)
  expect_equal(res$level, "No marker columns found.")
  expect_true(length(res$id_col) == 0)
})

test_that("message when no unique column among possible matches", {
  df <- data.frame(
    id = c(1, 1, 2, 2),
    participant_id = c(1, 1, 2, 2),
    item = c("A", "B", "A", "B"),
    trait = c("height", "height", "weight", "weight")
  )
  
  expect_true(
    find_data_level(df, id_cols = c("id", "participant_id"))$id_col == "id"
  )
})

test_that("returns correct level when no combination uniquely identifies", {
  df <- data.frame(
    id = c(1, 1, 2, 2),
    item = c("A", "A", "A", "A"),
    trait = c("height", "height", "height", "height")
  )
  
  result <- find_data_level(df)
  
  expect_equal(
    result$level,
    "No combination of markers uniquely identifies the data rows."
  )
})

test_that("No suffix-based varieties found when only ID is present", {
  df <- data.frame(id = 1:3, height = c(1.2, 2.3, 1.8), taste = c(3, 2, 4))
  
  result <- find_data_level(df)
  
  expect_equal(result$level, "id")
  expect_equal(result$varieties_cols, 0)
})
