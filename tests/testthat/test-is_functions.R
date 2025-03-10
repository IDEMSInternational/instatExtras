test_that("hashed_id generates correct hashed values", {
  x <- c("apple", "banana", "cherry")
  result <- hashed_id(x)
  expect_type(result, "character")
  expect_equal(length(result), length(x))
  
  salted_result <- hashed_id(x, salt = "salty")
  expect_type(salted_result, "character")
  expect_equal(length(salted_result), length(x))
})

test_that("import_from_iri handles incorrect source gracefully", {
  expect_error(import_from_iri("INVALID_SOURCE", "daily_0p05", "data", -10, 10, -10, 10, "area"))
})

test_that("import_from_iri handles incorrect source gracefully", {
  expect_error(
    import_from_iri(
      download_from = "INVALID_SOURCE",
      data_file = "daily_0p05",
      path = "",
      X1 = 35,
      X2 = 36,
      Y1 = -1,
      Y2 = 0,
      get_area_point = "area"
    ),
    "Source not specified correctly."
  )
})

test_that("import_from_iri handles incorrect data_file gracefully", {
  expect_error(
    import_from_iri(
      download_from = "CHIRPS_V2P0",
      data_file = "invalid_data_file",
      path = "",
      X1 = 35,
      X2 = 36,
      Y1 = -1,
      Y2 = 0,
      get_area_point = "area"
    ),
    "Data file does not exist for CHIRPS V2P0 data"
  )
})
test_that("in_top_n correctly identifies top N values", {
  x <- c(10, 5, 7, 12, 3)
  result <- in_top_n(x, 3)
  expect_equal(sum(result), 3)
  
  weighted_result <- in_top_n(x, 3, wt = c(2, 1, 3, 4, 2), fun = sum)
  expect_equal(sum(weighted_result), 3)
})

test_that("is.binary correctly identifies binary variables", {
  expect_true(is.binary(TRUE))
  expect_true(is.binary(c(0, 1, 1, 0)))
  expect_true(is.binary(factor(c("Yes", "No", "Yes"))))
  expect_false(is.binary(c(1, 2, 3, 4)))
})

test_that("is.emptyvariable correctly identifies empty variables", {
  expect_false(is.emptyvariable(c("", "abc", "")))
  expect_true(is.emptyvariable(c("", "")))
})

test_that("is.logical.like correctly identifies logical-like objects", {
  expect_true(is.logical.like(TRUE))
  expect_true(is.logical.like(c(TRUE, FALSE)))
  expect_true(is.logical.like(1))
  expect_true(is.logical.like(c(0, 1)))
  expect_false(is.logical.like("TRUE"))
  expect_false(is.logical.like(NULL))
})

test_that("is.NAvariable correctly identifies NA and NULL vectors", {
  expect_true(is.NAvariable(c(NA, NA, NA)))
  expect_true(is.NAvariable(NULL))
  expect_false(is.NAvariable(c(1, 2, 3)))
  expect_false(is.NAvariable(c(TRUE, FALSE, NA)))
})