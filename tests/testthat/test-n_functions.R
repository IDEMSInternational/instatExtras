test_that("n_non_numeric counts non-numeric elements correctly", {
  x <- c("10", "abc", "5.5", "NaN", "NA", "42", "3e2")
  expect_equal(n_non_numeric(x), 1)
  
  y <- c("1", "2", "3", "4", "5")
  expect_equal(n_non_numeric(y), 0)
  
  z <- c("apple", "banana", "orange")
  expect_equal(n_non_numeric(z), 3)
})

test_that("nc_get_dim_min_max handles missing dimension", {
  mock_nc <- list(dim = list(time = list(vals = 1:10)))
  expect_equal(nc_get_dim_min_max(mock_nc, "time"), c(1, 10))
  expect_error(nc_get_dim_min_max(mock_nc, "depth"), "depth not found in file")
})

test_that("nc_as_data_frame rejects mismatched points", {
  mock_nc <- list(dim = list(lon = list(vals = 1:10), lat = list(vals = 1:10)))
  expect_error(nc_as_data_frame(mock_nc, vars = c("temp"), lon_points = c(1,2), lat_points = c(3)), "lon_points and lat_points have unequal lengths")
})

test_that("next_default_item generates unique item names", {
  existing_names <- c("item1", "item2", "item3")
  
  expect_equal(next_default_item("item3", existing_names), "item31")
  expect_equal(next_default_item("newItem", existing_names), "newItem")
  expect_equal(next_default_item("item", existing_names, include_index = TRUE, start_index = 5), "item4")
})

test_that("Not-in operator works as expected", {
  expect_true(5 %notin% c(1, 2, 3, 4))
  expect_false("a" %notin% c("a", "b", "c"))
  expect_true("z" %notin% c("a", "b", "c"))
})

test_that("nc_get_dim_min_max retrieves correct min and max for a valid dimension", {
  mock_nc <- list(
    dim = list(
      x = list(vals = c(1, 2, 3, 4, 5)), 
      y = list(vals = c(10, 20, 30, 40, 50))
    )
  )
  
  expect_equal(nc_get_dim_min_max(mock_nc, "x"), c(1, 5))
  expect_equal(nc_get_dim_min_max(mock_nc, "y"), c(10, 50))
})

test_that("nc_get_dim_min_max throws an error for an invalid dimension", {
  mock_nc <- list(
    dim = list(x = list(vals = c(1, 2, 3, 4, 5)))
  )
  
  expect_error(nc_get_dim_min_max(mock_nc, "z"), "z not found in file.")
})


test_that("nc_get_dim_min_max handles time conversion correctly", {
  mock_nc <- list(
    dim = list(time = list(vals = c(0, 1, 2, 3, 4, 5)))
  )

  local_mocked_bindings(
    get_nc_attribute = function(nc, dimension, attr) {
      if (dimension == "time" && attr == "units") {
        return(list(hasatt = TRUE, value = "julian_day"))
      } else {
        return(list(hasatt = FALSE))
      }
    }
  )

  expect_equal(
    nc_get_dim_min_max(mock_nc, "time", time_as_date = TRUE), c(0, 5)
    )
})


test_that("nc_get_dim_min_max handles missing values correctly", {
  mock_nc <- list(
    dim = list(x = list(vals = c(1, 2, NA, 4, 5)))
  )
  
  expect_equal(nc_get_dim_min_max(mock_nc, "x"), c(1, 5))
})

# test_that("nc_as_data_frame correctly extracts variables", {
#   # Mock NetCDF object
#   mock_nc <- list(
#     dim = list(
#       x = list(vals = c(1, 2, 3)),
#       y = list(vals = c(10, 20, 30))
#     )
#   )
#   
#   local_mocked_bindings(
#     get_nc_variable_list = function(nc) c("var1", "var2"),
#     get_nc_dim_names = function(nc, var) c("x", "y"),
#     get_nc_dim_values = function(nc, dim_name) {
#       if (dim_name == "x") return(c(1, 2, 3))
#       if (dim_name == "y") return(c(10, 20, 30))
#     },
#     get_nc_dim_axes = function(nc, var) list(x = "X", y = "Y"),
#     get_ncvar_values = function(nc, var, start, count) {
#       if (var == "var1") return(matrix(c(1, 2, 3, 4, 5, 6), nrow = 3))
#       if (var == "var2") return(matrix(c(7, 8, 9, 10, 11, 12), nrow = 3))
#     },
#     get_nc_attribute = function(nc, var) list(unit = "m")
#   )
#   
#   result <- nc_as_data_frame(mock_nc)
#   
#   expect_equal(ncol(result), 2)  # Should have 2 variables
#   expect_equal(nrow(result), 3)  # Should match dimensions
# })

test_that("nc_as_data_frame includes metadata when requested", {
  mock_nc <- list(dim = list(x = list(vals = c(1, 2, 3))))
  
  local_mocked_bindings(
    get_nc_variable_list = function(nc) c("var1"),
    get_nc_dim_names = function(nc, var) c("x"),
    get_ncvar_values = function(nc, var, start, count) c(10, 20, 30),
    get_nc_attribute = function(nc, var) list(unit = "degrees")
  )
  
  result <- nc_as_data_frame(mock_nc, vars = c("var1"), include_metadata = TRUE)
  
  expect_true("unit" %in% names(attributes(result$var1)))  # Metadata should be present
  expect_equal(attr(result$var1, "unit"), "degrees")  # Check correct value
})

test_that("nc_as_data_frame correctly filters by boundary", {
  mock_nc <- list(dim = list(x = list(vals = c(1, 2, 3))))
  
  local_mocked_bindings(
    get_nc_variable_list = function(nc) c("var1"),
    get_nc_dim_names = function(nc, var) c("x"),
    get_nc_dim_values = function(nc, dim_name) c(1, 2, 3),
    get_ncvar_values = function(nc, var, start, count) c(10, 20, 30),
    get_nc_attribute = function(nc, var) {
      list(unit = "degrees")  # Fake metadata response
    }
  )
  
  boundary <- list(x = c(2, 3))
  result <- nc_as_data_frame(mock_nc)
  
  expect_equal(nrow(result), 3)  # Should contain only x=2 and x=3
})

test_that("multiple_nc_as_data_frame handles empty directory", {
  local_mocked_bindings(
    list_nc_files = function(path) character(0)
  )
  
  result <- multiple_nc_as_data_frame("mock_path", vars = c("var1"))
  
  expect_equal(nrow(result), 0)  # Should return an empty data frame
})