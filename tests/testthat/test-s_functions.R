# Test for slope function
test_that("slope calculates correct regression slope", {
  y <- c(1, 2, 3, 4, 5)
  x <- c(2, 4, 6, 8, 10)
  expect_equal(as.numeric(slope(y, x)), 0.5)
})

# Test for slopegraph_theme
test_that("slopegraph_theme returns a list of theme elements", {
  theme_elements <- slopegraph_theme()
  expect_type(theme_elements, "list")
  expect_true(length(theme_elements) > 0)
})

# Test for split_items_in_groups
test_that("split_items_in_groups correctly splits items", {
  items <- c("A", "B", "C", "D", "E", "F", "G", "H")
  num_groups <- 2
  result <- split_items_in_groups(items, num_groups)
  expect_equal(length(result), num_groups)
  expect_equal(length(result[[1]]), 4)
  expect_equal(length(result[[2]]), 4)
})

test_that("split_items_in_groups throws error for incorrect group division", {
  items <- c("A", "B", "C", "D", "E", "F", "G")
  num_groups <- 2
  expect_error(split_items_in_groups(items, num_groups))
})

# Test for summary_sample
test_that("summary_sample returns valid sample", {
  set.seed(123)
  data <- c(1, 2, 3, 4, 5)
  sample_result <- summary_sample(data, 3)
  expect_true(length(sample_result) == 3)
  expect_true(all(sample_result %in% data))
})

test_that("summary_sample handles empty input", {
  expect_equal(summary_sample(c(), 3), NA)
})

test_that("summary_sample handles single-element input", {
  expect_equal(summary_sample(c(5), 1), 5)
})
