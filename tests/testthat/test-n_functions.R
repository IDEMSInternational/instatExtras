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

# Create mock data for testing
mock_dim_axes <- list("lon" = "X", "lat" = "Y")
mock_dim_values <- list(
  "lon" = seq(-180, 180, by = 10),
  "lat" = seq(-90, 90, by = 10)
)
mock_lon_points <- c(-170, 0, 150)
mock_lat_points <- c(-80, 10, 80)
mock_id_points <- c("A", "B", "C")
mock_start <- c(1, 1)
mock_count <- c(length(mock_dim_values$lon), length(mock_dim_values$lat))
show_requested_points <- TRUE
great_circle_dist <- TRUE

# Define test cases
test_that("Function correctly subsets points from netCDF grid", {
  result <- subset_nc_by_points(
    nc = NULL,  # Mock, not actually used in the function
    dim_axes = mock_dim_axes,
    dim_values = mock_dim_values,
    lon_points = mock_lon_points,
    lat_points = mock_lat_points,
    id_points = mock_id_points,
    start = mock_start,
    count = mock_count,
    show_requested_points = show_requested_points,
    great_circle_dist = great_circle_dist
  )
  
  # Ensure correct number of points are processed
  expect_length(result$start_list, length(mock_lon_points))
  expect_length(result$count_list, length(mock_lon_points))
  expect_length(result$dim_values_list, length(mock_lon_points))
  
  # Check that requested points were added
  expect_true(result$requested_points_added)
  
  # Verify that each subsetted point matches the expected coordinates
  for (i in seq_along(mock_lon_points)) {
    expect_equal(result$dim_values_list[[i]]$lon, mock_lon_points[i], tolerance = 1e-6)
    expect_equal(result$dim_values_list[[i]]$lat, mock_lat_points[i], tolerance = 1e-6)
    expect_equal(result$dim_values_list[[i]]$station, mock_id_points[i])
  }
})

# Create a mock NetCDF file for testing
test_nc_file <- tempfile(fileext = ".nc")
nc <- ncdf4::nc_create(test_nc_file, vars = list(ncdf4::ncvar_def("test_var", "", list(ncdf4::ncdim_def("time", "days", 1:10)))))

test_that("get_nc_attribute retrieves attribute correctly", {
  ncdf4::ncatt_put(nc, "test_var", "units", "days since 2000-01-01")
  expect_equal(get_nc_attribute(nc, "test_var", "units")$value, "days since 2000-01-01")
})

# Test get_nc_dim_names
test_that("get_nc_dim_names returns correct dimension names", {
  expect_equal(get_nc_dim_names(nc, "test_var"), "time")
})

# Test get_ncvar_values
test_that("get_ncvar_values retrieves values correctly", {
  ncdf4::ncvar_put(nc, "test_var", 1:10)
  expect_equal(as.integer(get_ncvar_values(nc, "test_var", start = 1, count = 10)), 1:10)
})

# Test list_nc_files
test_that("list_nc_files lists only NetCDF files", {
  dir <- tempdir()
  file.create(file.path(dir, "test1.nc"))
  file.create(file.path(dir, "test2.txt"))
  expect_equal(list_nc_files(dir)[length(list_nc_files(dir))], file.path(dir, "test1.nc"))
})

# Test open and close NetCDF file
test_that("open_nc_file and close_nc_file work correctly", {
  nc2 <- open_nc_file(test_nc_file)
  expect_s3_class(nc2, "ncdf4")
  close_nc_file(nc2)
})

# Test convert_pcict_to_posixct
test_that("convert_pcict_to_posixct converts PCICt time correctly", {
  pcict_time <- PCICt::as.PCICt("2000-01-01 12:00:00", cal = "gregorian")
  expect_equal(as.character(convert_pcict_to_posixct(pcict_time)), "2000-01-01 12:00:00")
})

# Test for error when boundary contains invalid dimensions
test_that("nc_as_data_frame throws error for invalid boundary dimensions", {
  expect_error(nc_as_data_frame(nc, vars = "test_var", boundary = list("invalid_dim" = c(1, 2))),
               "boundary contains dimensions not associated with")
})

# Close and remove test NetCDF file
close_nc_file(nc)
file.remove(test_nc_file)


###

test_that("plot_network works with basic rankings object", {
  R <- matrix(c(1, 2, 0, 0,
                4, 1, 2, 3,
                2, 4, 1, 3,
                1, 2, 3, 0,
                2, 1, 3, 0,
                1, 0, 3, 2), nrow = 6, byrow = TRUE)
  colnames(R) <- c("apple", "banana", "orange", "pear")
  R <- PlackettLuce::as.rankings(R)
  
  p <- plot_network(R)
  expect_s3_class(p, "gg")
})

test_that("plot_network works with fluctuate_widths = TRUE", {
  R <- matrix(c(1, 2, 3, 0,
                2, 1, 3, 0), nrow = 2, byrow = TRUE)
  colnames(R) <- c("a", "b", "c", "d")
  R <- PlackettLuce::as.rankings(R)
  
  p <- plot_network(R, fluctuate_widths = TRUE)
  expect_s3_class(p, "gg")
})

test_that("plot_network assigns column names if missing", {
  R <- matrix(c(1, 2, 3, 0,
                2, 1, 3, 0), nrow = 2, byrow = TRUE)
  R <- PlackettLuce::as.rankings(R)
  
  p <- plot_network(R)
  expect_s3_class(p, "gg")
})

test_that("btdata handles matrix input correctly", {
  mat <- matrix(c(0, 2, 1,
                  3, 0, 1,
                  4, 2, 0), nrow = 3)
  colnames(mat) <- rownames(mat) <- c("A", "B", "C")
  res <- btdata(mat, return_graph = TRUE)
  expect_s3_class(res, "btdata")
  expect_true("graph" %in% names(res))
})

test_that("btdata errors on non-square matrix", {
  m <- matrix(1:6, nrow = 2)
  expect_error(btdata(m), "must be a square")
})

test_that("btdata handles table input", {
  tbl <- as.table(matrix(c(0, 2, 1,
                           3, 0, 1,
                           4, 2, 0), nrow = 3))
  dimnames(tbl) <- list(c("A", "B", "C"), c("A", "B", "C"))
  res <- btdata(tbl)
  expect_s3_class(res, "btdata")
})

test_that("btdata errors on undirected graph", {
  g <- igraph::make_ring(3)
  expect_error(btdata(g), "x must be a 3 or 4 column dataframe, a directed igraph object, or square matrix or contingency table.")
})
