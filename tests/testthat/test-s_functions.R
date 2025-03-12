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

test_that("slopegraph handles valid input correctly", {
  data <- data.frame(
    x = factor(c("A", "B", "C"), ordered = TRUE),
    y = c(10, 20, 30),
    colour = c("Group1", "Group2", "Group3"),
    data_label = c("10", "20", "30")
  )
  
  plot <- slopegraph(data, x, y, colour, data_label)
  expect_s3_class(plot, "gg")  # Check if it returns a ggplot object
  expect_message(slopegraph(data, x, y, colour, data_label, line_colour = c(1, 2)))
  
  plot2 <- slopegraph(data, x, y, colour, data_label, reverse_x_axis = TRUE)
  expect_s3_class(plot2, "gg")  # Check if it returns a ggplot object
})

test_that("slopegraph throws error for missing required arguments", {
  data <- data.frame(
    x = factor(c("A", "B"), ordered = TRUE),
    y = c(10, 20),
    colour = c("Group1", "Group2")
  )
  
  expect_error(slopegraph(), "Not enough arguments passed requires a dataframe")
  expect_error(slopegraph(data, y, colour), "Not enough arguments passed requires a dataframe")
})

test_that("slopegraph throws error for incorrect data types", {
  data_invalid <- data.frame(
    x = c("A", "B"),  # Not a factor
    y = c("High", "Low"),  # Not numeric
    colour = c("Group1", "Group2")
  )
  
  expect_error(slopegraph(data_invalid, x, y, colour), "Variable 'y' needs to be numeric")
})

test_that("slopegraph throws error for missing data or values in x or colour", {
  data_na_x <- data.frame(
    x = factor(c("A", NA), ordered = TRUE),
    y = c(10, 20),
    colour = c("Group1", "Group2")
  )
  
  expect_error(slopegraph(data_na_x, x, y, colour), "'x' can not have missing data please remove those rows")

  data_na_colour <- data.frame(
    x = factor(c("A", "B"), ordered = TRUE),
    y = c(10, 20),
    colour = c("Group1", NA)
  )
  
  expect_error(slopegraph(data_na_colour, x, y, colour), "'colour' can not have missing data please remove those rows")
  
  # no data argument
  expect_error(slopegraph(x = x, y = y, colour = colour, y_text_size = 10), "You didn't specify a dataframe to use")
  
  # incorrect data argument
  expect_error(slopegraph(data = 10, x = x, y = y, colour = colour), "'10' does not appear to be a data frame")
  
  # call variable not in data
  expect_error(slopegraph(data = data_na_colour, x = X, y = y, colour = colour), "'X' is not the name of a variable in the dataframe")
  expect_error(slopegraph(data = data_na_colour, x = x, y = Y, colour = colour), "'Y' is not the name of a variable in the dataframe")
  expect_error(slopegraph(data = data_na_colour, x = x, y = y, data_label  = Hello, colour = colour), "'Hello' is not the name of a variable in the dataframe")
  expect_error(slopegraph(data = data_na_colour, x = x, y = y, colour = COLOUR), "'COLOUR' is not the name of a variable in the dataframe")

  # incorrect variable type
  data_na_colour$x <- as.numeric(data_na_colour$x)
  expect_error(slopegraph(data = data_na_colour, x = x, y = y, colour = x), "Variable 'x' needs to be of class character, factor or ordered")
  
  })

test_that("slopegraph converts factor x to ordered", {
  data_factor <- data.frame(
    x = factor(c("A", "B")),  # Not ordered initially
    y = c(10, 20),
    colour = c("Group1", "Group2")
  )
  
  expect_message(slopegraph(data_factor, x, y, colour), "Converting 'x' to an ordered factor")
})

