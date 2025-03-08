# Test for yday_366 function
test_that("yday_366 returns correct day of year", {
  expect_equal(yday_366(as.Date("1999-03-01")), 61)
  expect_equal(yday_366(as.Date("2000-12-31")), 366)
  expect_equal(yday_366(as.Date("2005-12-31")), 366)
  expect_equal(yday_366(as.Date("2001-02-28")), 59)
  expect_equal(yday_366(as.Date("2001-03-01")), 61)
})

test_that("yday_366 handles NA and non-date inputs", {
  expect_true(is.na(yday_366(NA)))
  expect_error(yday_366("not_a_date"))
})

# Test for getHelpFile function
test_that("getHelpFile throws error for invalid paths", {
  expect_error(getHelpFile("non_existent_path"))
})