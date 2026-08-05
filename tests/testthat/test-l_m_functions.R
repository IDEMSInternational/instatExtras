test_that("lat_lon_dataframe correctly generates latitude-longitude dataframe", {
  mock_data <- data.frame(
    X1 = c(NA, NA, NA, 10, 20),
    X2 = c(NA, NA, NA, 30, 40),
    X3 = c(NA, NA, NA, 50, 60)
  )
  
  result <- lat_lon_dataframe(mock_data)
  expect_s3_class(result, "data.frame")
  expect_true("station" %in% colnames(result))
  expect_gt(nrow(result), 0)
})

# test_that("is.levelscount correctly checks factor levels count", {
#   factor_var <- factor(c("A", "B", "C", "D"))
#   expect_true(is.levelscount(factor_var, 4))
#   expect_false(is.levelscount(factor_var, 3))
# })

test_that("make_factor correctly converts data types", {
  num_vec <- c(1, 2, 3, 3, 2, 1)
  fact1 <- make_factor(num_vec, ordered = TRUE)
  expect_s3_class(fact1, "ordered")
  
  log_vec <- c(TRUE, FALSE, TRUE)
  fact2 <- make_factor(log_vec, ordered = FALSE)
  expect_s3_class(fact2, "factor")
  expect_setequal(levels(fact2), c("FALSE", "TRUE"))
  
  char_vec <- c("apple", "banana", "apple", "orange")
  fact3 <- make_factor(char_vec, ordered = FALSE)
  expect_s3_class(fact3, "factor")
  expect_setequal(levels(fact3), unique(char_vec))
  
  expect_error(make_factor(Sys.time(), ordered = FALSE))
})

test_that("make_factor changes ordering status correctly", {
  x <- factor(c("A", "B", "C"))  # Not ordered initially
  
  x_ordered <- make_factor(x, ordered = TRUE)
  expect_true(is.ordered(x_ordered))  # Should now be ordered
  
  x_unordered <- make_factor(x_ordered, ordered = FALSE)
  expect_false(is.ordered(x_unordered))  # Should now be unordered
})

test_that("max_consecutive_sum computes correct maximum consecutive sum", {
  x <- c(1, -2, 3, 4, -1, 2, 1, -5, 4)
  result <- max_consecutive_sum(x)
  expect_type(result, "double")
  expect_gt(result, 0)
})

test_that("make_names_pattern creates pattern for numeric suffixes", {
  items <- c(
    "X1", "X2", "X3", "X4",
    "Y1", "Y2", "Y3", "Y4"
  )
  
  expect_equal(
    make_names_pattern(items, num = 2),
    "(X|Y)(1|2|3|4)"
  )
})

test_that("make_names_pattern works with non-numeric suffixes", {
  items <- c(
    "height_male", "height_female",
    "weight_male", "weight_female"
  )
  
  expect_equal(
    make_names_pattern(items, num = 2),
    "(height_|weight_)(male|female)"
  )
})


test_that("make_names_pattern works with a single item per group", {
  items <- c("X", "Y")
  
  expect_equal(
    make_names_pattern(items, num = 2),
    "(X|Y)()"
  )
})

test_that("make_names_pattern works with more than two groups", {
  items <- c(
    "X1", "X2",
    "Y1", "Y2",
    "Z1", "Z2"
  )
  
  expect_equal(
    make_names_pattern(items, num = 3),
    "(X|Y|Z)(1|2)"
  )
})

test_that("make_names_pattern errors when items cannot be split evenly", {
  items <- c("X1", "X2", "Y1")
  
  expect_error(
    make_names_pattern(items, num = 2),
    "The number of items must be divisible by the number of groups"
  )
})

test_that("make_names_pattern handles prefixes of different lengths", {
  items <- c(
    "temp_1", "temp_2",
    "rainfall_1", "rainfall_2"
  )
  
  expect_equal(
    make_names_pattern(items, num = 2),
    "(temp_|rainfall_)(1|2)"
  )
})
