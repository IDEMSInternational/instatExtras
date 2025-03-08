test_that("hashed_id generates correct hash", {
  x <- c("apple", "banana", "cherry")
  hash_no_salt <- hashed_id(x)
  hash_with_salt <- hashed_id(x, salt = "salty")
  
  expect_true(is.character(hash_no_salt))
  expect_true(is.character(hash_with_salt))
  expect_equal(length(hash_no_salt), length(x))
  expect_equal(length(hash_with_salt), length(x))
})

test_that("pentad function returns correct pentad values", {
  expect_equal(pentad(as.Date("2023-01-01")), 1)
  expect_equal(pentad(as.Date("2023-07-17")), 40)
  expect_equal(pentad(as.Date("2023-12-31")), 72)
})

test_that("record_graph records a plot", {
  plot(1:10, 1:10)
  recorded <- record_graph(1)
  expect_s3_class(recorded, "recordedplot")
})

test_that("return_variable_levels generates JavaScript function", {
  df <- data.frame(
    category1 = factor(c("low", "medium", "high"), levels = c("low", "medium", "high")),
    category2 = factor(c("A", "B", "C"), levels = c("A", "B", "C"))
  )
  js_code <- return_variable_levels(df)
  expect_true(is.character(js_code))
  expect_true(grepl("function", js_code))
})

test_that("package_check returns expected output", {
  result <- package_check("ggplot2")
  expect_true(is.list(result))
  expect_true(result[[1]] %in% c("1", "2", "3", "4", "5"))
})
