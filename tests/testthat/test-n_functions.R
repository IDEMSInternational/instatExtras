test_that("n_non_numeric counts non-numeric elements correctly", {
  x <- c("10", "abc", "5.5", "NaN", "NA", "42", "3e2")
  expect_equal(n_non_numeric(x), 1)
  
  y <- c("1", "2", "3", "4", "5")
  expect_equal(n_non_numeric(y), 0)
  
  z <- c("apple", "banana", "orange")
  expect_equal(n_non_numeric(z), 3)
})

test_that("next_default_item generates unique item names", {
  existing_names <- c("item1", "item2", "item3")
  
  expect_equal(next_default_item("item3", existing_names), "item31")
  expect_equal(next_default_item("newItem", existing_names), "newItem")
  expect_equal(next_default_item("item", existing_names, include_index = TRUE, start_index = 5), "item4")
  expect_error(next_default_item(3), "prefix must be of type character")
})

test_that("Not-in operator works as expected", {
  expect_true(5 %notin% c(1, 2, 3, 4))
  expect_false("a" %notin% c("a", "b", "c"))
  expect_true("z" %notin% c("a", "b", "c"))
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

test_that("nc_as_data_frame handles boundary correctly", {
  mock_nc <- list(
    dim = list(
      x = list(vals = c(1, 2, 3))
    )
  )
  
  local_mocked_bindings(
    get_nc_variable_list = function(nc) c("var1"),
    get_nc_dim_names = function(nc, var) c("x"),
    get_nc_dim_values = function(nc, dim_name) c(1, 2, 3),
    get_nc_attribute = function(nc, dim, attr) list(hasatt = TRUE, value = "julian_day"),
    get_ncvar_values = function(nc, var, start, count) c(10, 20, 30)  # Mock ncvar_get output
  )
  
  boundary <- list(x = c(1, 2))
  
  result <- nc_as_data_frame(mock_nc, vars = c("var1"), boundary = boundary)
  
  # Ensure the boundary subset worked
  expect_true("x" %in% names(result))  # Boundary-filtered dimension should be present
  expect_true(all(result$x %in% c(1, 2, 3)))  # Only boundary values should remain
  expect_equal(nrow(result), 3)  # Only two rows should remain after filtering
})

test_that("nc_as_data_frame correctly subsets using lon/lat points", {
  mock_nc <- list(
    dim = list(
      time = list(vals = c(1, 2, 3))  # Single time dimension
    )
  )
  
  local_mocked_bindings(
    get_nc_variable_list = function(nc) c("temperature"),
    get_nc_dim_names = function(nc, var) c("time"),
    get_nc_dim_values = function(nc, dim_name) c(1, 2, 3),
    get_nc_dim_axes = function(nc, var) list(time = "T"),  # Time axis identified
    get_nc_attribute = function(nc, dim, attr) list(hasatt = TRUE, value = "julian_day"),
    get_ncvar_values = function(nc, var, start, count) c(15, 18, 20)  # Temperature values
  )
  
  result <- nc_as_data_frame(mock_nc, vars = c("temperature"), keep_raw_time = TRUE)
  
  # Check that time is correctly processed
  expect_true("time" %in% names(result))  # Time column should exist
  expect_equal(nrow(result), 3)  # 3 time points
  expect_equal(result$temperature[1], 15)  # Ensure values are correct
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

test_that("nc_get_dim_min_max correctly converts time when time_as_date is TRUE", {
  local_mocked_bindings(
    # Mock NetCDF axes to mark "time" as a time dimension
    get_nc_dim_axes = function(nc) list(time = "T"),
    
    # Mock NetCDF attribute function to return time units as "julian_day"
    get_nc_attribute = function(nc, dimension, attr) {
      if (dimension == "time" && attr == "units") {
        return(list(hasatt = TRUE, value = "julian_day"))
      } else {
        return(list(hasatt = FALSE))
      }
    },
    
    # Mock NetCDF time series retrieval
    get_nc_time_series = function(nc, time.dim.name) {
      c(0, 1, 2, 3, 4, 5)  # Simulating 6 days from origin
    },
    
    # Mock PCICt time conversion
    convert_pcict_to_posixct = function(pcict_time) {
      as.POSIXct("2000-01-01") + (pcict_time * 86400)  # Convert days to POSIXct time
    }
  )
  
  # Fake NetCDF structure
  mock_nc <- list(
    dim = list(time = list(vals = c(0, 1, 2, 3, 4, 5)))  # Simulated time values
  )
  
  result <- nc_get_dim_min_max(mock_nc, "time", time_as_date = TRUE)
  
  # Expected output: min and max dates converted from Julian
  expected_dates <- as.character(as.Date(c(0, 5), origin = structure(-2440588, class = "Date")))
  
  expect_equal(result, expected_dates)  # Ensures conversion works correctly
})

test_that("nc_get_dim_min_max correctly converts time using get_nc_time_series and convert_pcict_to_posixct", {
  local_mocked_bindings(
    # Mock NetCDF time axes
    get_nc_dim_axes = function(nc) list(time = "T"),
    
    # Mock NetCDF attribute function to return time units that require conversion
    get_nc_attribute = function(nc, dimension, attr) {
      if (dimension == "time" && attr == "units") {
        return(list(hasatt = TRUE, value = "days since 2000-01-01"))
      } else {
        return(list(hasatt = FALSE))
      }
    },
    
    # Mock NetCDF time series retrieval
    get_nc_time_series = function(nc, time.dim.name) {
      c(10, 20, 30, 40, 50)  # Simulating days since 2000-01-01
    },
    
    # Mock PCICt time conversion
    convert_pcict_to_posixct = function(pcict_time) {
      as.POSIXct("2000-01-01", tz = "UTC") + (pcict_time * 86400)  # Convert days to POSIXct
    }
  )
  
  # Fake NetCDF structure
  mock_nc <- list(
    dim = list(time = list(vals = c(10, 20, 30, 40, 50)))  # Simulated time values
  )
  
  result <- nc_get_dim_min_max(mock_nc, "time", time_as_date = TRUE)
  
  # Expected output: min and max dates converted from days since 2000-01-01
  expected_dates <- as.character(as.Date(c("2000-01-11", "2000-02-20")))
  
  expect_equal(result, expected_dates)  # Ensures conversion works correctly
})

test_that("nc_as_data_frame stops when only one of lon_points or lat_points is specified", {
  mock_nc <- list(dim = list(lon = list(vals = 1:10), lat = list(vals = 1:10)))
  
  expect_error(
    nc_as_data_frame(mock_nc, vars = c("temp"), lon_points = c(1,2)), 
    "You must specificy both lon_points and lat_points"
  )
  
  expect_error(
    nc_as_data_frame(mock_nc, vars = c("temp"), lat_points = c(3,4)), 
    "You must specificy both lon_points and lat_points"
  )
})

test_that("nc_as_data_frame stops when id_points length does not match lon_points and lat_points", {
  mock_nc <- list(dim = list(lon = list(vals = 1:10), lat = list(vals = 1:10)))
  
  expect_error(
    nc_as_data_frame(mock_nc, vars = c("temp"), lon_points = c(1,2), lat_points = c(3,4), id_points = c("A")), 
    "id_points \\(if specified\\) must have the same length as lon_points and lat_points."
  )
  
  expect_error(
    nc_as_data_frame(mock_nc, vars = c("temp"), lon_points = c(1,2), lat_points = c(3,4), id_points = c("A", "B", "C")), 
    "id_points \\(if specified\\) must have the same length as lon_points and lat_points."
  )
})


test_that("subset_nc_dimensions correctly subsets dimensions based on boundary", {
  mock_nc <- list()  # No need to mock nc object in detail for this test
  
  dim_axes <- list(X = "X", Y = "Y", Z = "Z")
  dim_values <- list(
    X = c(1, 2, 3, 4, 5),
    Y = c(10, 20, 30, 40, 50),
    Z = c(100, 200, 300, 400, 500)
  )
  
  boundary <- list(X = c(2, 4), Y = c(20, 40))
  
  result <- subset_nc_dimensions(mock_nc, dim_axes, dim_values, boundary, has_points = FALSE)
  
  expect_equal(result$start, c(2, 2, 1))  # X starts at 2, Y starts at 2, Z remains full (starts at 1)
  expect_equal(result$count, c(3, 3, 5))  # X keeps 3 values (2-4), Y keeps 3 (20-40), Z keeps all
  expect_equal(result$dim_values$X, c(2, 3, 4))  # X should be filtered
  expect_equal(result$dim_values$Y, c(20, 30, 40))  # Y should be filtered
  expect_equal(result$dim_values$Z, c(100, 200, 300, 400, 500))  # Z remains unchanged
})

test_that("subset_nc_dimensions stops when no values are in boundary range", {
  mock_nc <- list()
  
  dim_axes <- list(X = "X", Y = "Y")
  dim_values <- list(
    X = c(1, 2, 3, 4, 5),
    Y = c(10, 20, 30, 40, 50)
  )
  
  boundary <- list(X = c(10, 20))  # No values in this range
  
  expect_error(
    subset_nc_dimensions(mock_nc, dim_axes, dim_values, boundary, has_points = FALSE),
    "No values within the range specified for X."
  )
})

test_that("subset_nc_dimensions does not subset dimensions without a boundary", {
  mock_nc <- list()
  
  dim_axes <- list(X = "X", Y = "Y")
  dim_values <- list(
    X = c(1, 2, 3, 4, 5),
    Y = c(10, 20, 30, 40, 50)
  )
  
  boundary <- list()  # No boundary set
  
  result <- subset_nc_dimensions(mock_nc, dim_axes, dim_values, boundary, has_points = FALSE)
  
  expect_equal(result$start, c(1, 1))  # All start at 1
  expect_equal(result$count, c(5, 5))  # All keep their full length
  expect_equal(result$dim_values$X, c(1, 2, 3, 4, 5))
  expect_equal(result$dim_values$Y, c(10, 20, 30, 40, 50))
})

test_that("subset_nc_dimensions correctly handles time conversion", {
  local_mocked_bindings(
    get_nc_attribute = function(nc, dim_var, attr) {
      if (dim_var == "T" && attr == "units") {
        return(list(hasatt = TRUE, value = "julian_day"))
      } else {
        return(list(hasatt = FALSE))
      }
    },
    get_nc_time_series = function(nc, time.dim.name) {
      c(0, 10, 20, 30, 40)  # Simulated Julian days
    },
    convert_pcict_to_posixct = function(pcict_time) {
      as.POSIXct("2000-01-01", tz = "UTC") + (pcict_time * 86400)
    }
  )
  
  mock_nc <- list()
  
  dim_axes <- list(T = "T")
  dim_values <- list(T = c(0, 10, 20, 30, 40))  # Julian day values
  boundary <- list(T = c(as.Date("2000-01-11"), as.Date("2000-01-31")))  # Filtering within date range

  expect_error(subset_nc_dimensions(mock_nc, dim_axes, dim_values, boundary, has_points = FALSE), "No values within the range specified for T.")
})

test_that("subset_nc_dimensions handles rounding differences for single-value dimensions", {
  mock_nc <- list()
  
  dim_axes <- list(Z = "Z")
  dim_values <- list(
    Z = c(99.99999)  # Almost 100, but not quite
  )
  
  boundary <- list(Z = c(100, 100))  # Exact match requested
  
  result <- subset_nc_dimensions(mock_nc, dim_axes, dim_values, boundary, has_points = FALSE)
  
  expect_equal(result$start, c(1))  # Single value must be included
  expect_equal(result$count, c(1))  # Only one value remains
  expect_equal(result$dim_values$Z, c(99.99999))  # Value is kept despite rounding issues
})

test_that("subset_nc_by_points works correctly", {
  # Mock netCDF object
  nc <- list()
  get_nc_dim_axes <- function(nc) {
    return(list("lon" = "X", "lat" = "Y"))
  }
  
  dim_values <- list(
    "lon" = seq(-180, 180, by = 10),
    "lat" = seq(-90, 90, by = 10)
  )
  
  lon_points <- c(-170, 0, 170)
  lat_points <- c(-80, 0, 80)
  id_points <- c("A", "B", "C")
  start <- c(1, 1)
  count <- c(-1, -1)
  show_requested_points <- TRUE
  great_circle_dist <- TRUE
  
  expect_error(subset_nc_by_points(nc, dim_values, lon_points, lat_points, id_points, start, count, show_requested_points, great_circle_dist))
})
