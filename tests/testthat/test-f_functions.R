
test_that("find_data_level works with full ID-Variety-Trait structure", {
  df <- tibble::tibble(
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
  df <- tibble::tibble(id = 1:5)
  res <- find_data_level(df)
  expect_equal(res$level, "id")
  expect_equal(res$id_col, "id")
  expect_true(is.null(res$variety_col))
  expect_true(is.null(res$trait_col))
})

test_that("find_data_level handles duplicate columns (chooses unique)", {
  df <- tibble::tibble(
    participant_id = 1:5,
    ID = c(1, 1, 2, 2, 3)  # not unique
  )
  res <- find_data_level(df)
  expect_equal(res$id_col, "participant_id")  # chosen for uniqueness
})

test_that("find_data_level handles no matching columns", {
  df <- tibble::tibble(x = 1:5)
  res <- find_data_level(df)
  expect_equal(res$level, "No marker columns found.")
  expect_true(is.null(res$id_col))
})
