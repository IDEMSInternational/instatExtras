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

test_that("index.search handles AnIndex file correctly", {
  # Create a temporary directory structure for a fake package
  temp_pkg <- tempfile()
  dir.create(file.path(temp_pkg, "help"), recursive = TRUE)
  
  # Create a fake AnIndex file
  anindex_path <- file.path(temp_pkg, "help", "AnIndex")
  writeLines(c("topic1\thelpfile1", "topic2\thelpfile2"), anindex_path)
  
  # Run the function
  result <- index.search("topic1", paths = temp_pkg, firstOnly = TRUE)
  
  # Expected path should point to the helpfile1 location
  expect_equal(result, file.path(temp_pkg, "help", "helpfile1"))
  
  # Cleanup
  unlink(temp_pkg, recursive = TRUE)
})

test_that("getHelpFile throws an error for invalid file argument", {
  expect_error(getHelpFile("nonexistent/path/to/helpfile"), 
               "invalid 'file' argument")
})
