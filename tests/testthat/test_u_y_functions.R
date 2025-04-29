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

test_that("error if vars_to_get not in rankings_object", {
  # Fake data
  data <- data.frame(Name = c("var1", "var2", "var3"))
  
  # rankings_object missing one variable
  rankings_object <- list(var1 = 1:5, var2 = 6:10)
  
  # vars_to_get includes a variable that doesn't exist in rankings_object
  vars_to_get <- c("var1", "var2", "var3")  # var3 missing in rankings_object
  
  expect_error(
    get_ranking_items(data = data, vars_to_get = vars_to_get, index = "rankings_index", rankings_object = rankings_object),
    regexp = "Some vars_to_get are not in the rankings_object: var3"
  )
})
