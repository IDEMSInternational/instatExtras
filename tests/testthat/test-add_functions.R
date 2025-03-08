library(testthat)

test_that("add_nc correctly appends 'data.nc'", {
  expect_equal(add_nc("my_folder/"), "my_folder/data.nc")
  expect_equal(add_nc("/path/to/file.txt"), "/path/to/file.txtdata.nc")
  expect_equal(add_nc(""), "data.nc")
})

test_that("add_t_range generates correct time range string", {
  min_date <- lubridate::ymd("2023-01-01")
  max_date <- lubridate::ymd("2023-12-31")
  expected <- "http://example.com/T/(1%20Jan%202023)/(31%20Dec%202023)/RANGEEDGES/"
  
  expect_equal(add_t_range("http://example.com/", min_date, max_date), expected)
})

test_that("add_xy_area_range generates correct XY area range", {
  expected <- "http://example.com/X/(90W)/(80W)/RANGEEDGES/Y/(30N)/(40N)/RANGEEDGES/"
  
  expect_equal(
    add_xy_area_range("http://example.com", -90, -80, 30, 40),
    expected
  )
  
  expected_custom <- "http://example.com/LON/(90W)/(80W)/RANGEEDGES/LAT/(30N)/(40N)/RANGEEDGES/"
  
  expect_equal(
    add_xy_area_range("http://example.com", -90, -80, 30, 40, "LON", "LAT"),
    expected_custom
  )
})

test_that("add_xy_point_range generates correct XY point range", {
  expected <- "data/X/(90W)/VALUES/Y/(30N)/VALUES/"
  
  expect_equal(
    add_xy_point_range("data", -90, 30),
    expected
  )
  
  expected_custom <- "path/LON/(50E)/VALUES/LAT/(10S)/VALUES/"
  
  expect_equal(
    add_xy_point_range("path", 50, -10, "LON", "LAT"),
    expected_custom
  )
})