test_that("get_column_attributes retrieves correct attributes", {
  vec <- c(1, 2, 3)
  attr(vec, "test_attr") <- "example"
  
  result <- get_column_attributes(vec)
  
  expect_named(result, "test_attr")
  expect_equal(result$test_attr, "example")
})

test_that("get_default_significant_figures returns correct values", {
  expect_equal(get_default_significant_figures(1:5), getOption("digits"))
  expect_equal(get_default_significant_figures("not numeric"), NA)
})

# test_that("get_installed_packages_with_data retrieves packages correctly", {
#   packages <- get_installed_packages_with_data()
#   
#   expect_type(packages, "character")
# })

test_that("get_lon_from_data extracts longitude values", {
  mock_data <- data.frame(
    X1 = c(NA, NA, NA, NA, "Lon"),
    X2 = c(NA, NA, NA, NA, 30),
    X3 = c(NA, NA, NA, NA, 40)
  )
  
  result <- get_lon_from_data(mock_data)
  expect_equal(result, c(30, 40))
})

test_that("get_lat_from_data extracts latitude values", {
  mock_data <- data.frame(
    X1 = c(NA, NA, NA, 10, 20),
    X2 = c(NA, NA, NA, 30, 40)
  )
  
  result <- get_lat_from_data(mock_data)
  expect_equal(result, 20)
})

test_that("get_odk_form_names rejects incorrect platforms", {
  expect_error(get_odk_form_names("username", "invalid_platform"))
})

test_that("get_quarter_label returns correct quarter labels", {
  expect_equal(get_quarter_label(1, 1), factor("JFM"))
  expect_equal(get_quarter_label(2, 4), factor("JAS"))
  expect_error(get_quarter_label(5, 1))  # Invalid quarter
  expect_error(get_quarter_label(2, 13)) # Invalid start month
})

test_that("get_years_from_data extracts correct years", {
  mock_data <- data.frame(
    Name = c("John", "Alice", "Bob"),
    Age = c(25, 30, 35),
    "2019" = c(100, 150, 200),
    "2020" = c(200, 300, 400),
    "2021" = c(300, 450, 600)
  )
  
  result <- get_years_from_data(mock_data)
  expect_equal(rownames(result), c("Age", "X2019", "X2020", "X2021"))
})

# For getPass
test_that("getPass errors", {
  expect_error(getPass(1:5))
  expect_error(getPass(msg = "Hello", noblank = 1:5))
  expect_error(getPass(msg = "Hello", forcemask = 1:5))
  expect_error(getPass(msg = NULL), "argument 'msg' must be a single string")
  expect_error(getPass(noblank = "yes"), "argument 'noblank' must be one of 'TRUE' or 'FALSE'")
  expect_error(getPass(forcemask = 1), "argument 'forcemask' must be one of 'TRUE' or 'FALSE'")
})
# 
# # Test readline_masked_tcltk without user interaction
# test_that("readline_masked_tcltk handles input correctly", {
#   skip_if_not(requireNamespace("tcltk", quietly = TRUE))
#   
#   with_mocked_bindings(
#     readline_masked_tcltk_window = function(...) "tk_password",
#     expect_equal(readline_masked_tcltk("Enter password:"), "tk_password")
#   )
# })
# 
# # Test readline_masked_tcltk without user interaction
# test_that("readline_masked_tcltk handles input correctly", {
#   with_mocked_bindings(
#     readline_masked_tcltk_window = function(...) "tk_password",
#     expect_equal(readline_masked_tcltk_window("Enter password:"), "tk_password")
#   )
# })
# 
# # Test readline_nomask without user interaction
# test_that("readline_nomask handles input correctly", {
#   local_mocked_bindings(
#     readline = function(...) "test_password"
#   )
#   expect_equal(readline_nomask("Enter password: ", noblank = TRUE), "test_password")
# })
# 
# 
