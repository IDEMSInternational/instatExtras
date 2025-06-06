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
  expect_error(get_odk_form_names(platform = "invalid_platform"))
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

test_that("getPass correctly captures user input", {
  local_mocked_bindings(
    getPass_readline = function(...) "my_password"
  )
  expect_equal(getPass("Enter password: "), "my_password")
})

test_that("getPass returns NULL if user cancels input", {
  local_mocked_bindings(
    getPass_readline = function(...) NULL
  )

  expect_null(getPass("Enter password: "))
})

test_that("getPass uses Tcl/Tk window when available", {
  local_mocked_bindings(
    readline_masked_tcltk_window = function(...) "tcltk_password",
    hastcltk = function() TRUE
  )

  expect_equal(getPass("Enter password: "), "tcltk_password")
})

# test_that("getPass stops when masking is required but unsupported", {
#   local_mocked_bindings(
#     hastcltk = function() FALSE
#   )
# 
#   expect_error(getPass("Enter password:", forcemask = TRUE), "Masking is not supported")
# })

test_that("get_odk_form_names handles authentication and API request correctly", {

  # Mock `getPass()` to avoid user input
  local_mocked_bindings(
    getPass = function(...) "mock_password",

    # Mock our custom wrapper function instead of `httr::GET`
    get_odk_http_get = function(url, auth = NULL) {
      # Simulated JSON response structure
      fake_response <- list(
        list(title = "Form A", id = 1),
        list(title = "Form B", id = 2)
      )
      structure(
        list(content = fake_response, status_code = 200),
        class = "response"
      )
    },

    # Mock our custom wrapper function instead of `httr::content`
    get_odk_http_content = function(response, type) {
      response$content
    }
  )

  # Test function with mock responses
  form_names <- get_odk_form_names("mock_user", "kobo")

  # Expected output
  expect_equal(form_names, c("Form A", "Form B"))
})

test_that("get_odk_form_names handles invalid password correctly", {
  # Mock `getPass()` to return an incorrect password
  local_mocked_bindings(
    getPass = function(...) "wrong_password",
    
    # Mock our custom wrapper function instead of `httr::GET`
    get_odk_http_get = function(url, auth = NULL) {
      structure(
        list(status_code = 401),  # Simulate authentication failure
        class = "response"
      )
    }
  )
  
  # Expect function to throw an error due to invalid credentials
  expect_error(
    get_odk_form_names("mock_user", "ona"),
    "Invalid username/password"
  )
})

test_that("readline_masked_rstudio_window correctly calls askForPassword()", {
  local_mocked_bindings(
    has_fun = function(fun) TRUE,  # Pretend askForPassword exists
    ask_for_password = function(msg) "mock_password"
  )

  result <- readline_masked_rstudio_window("Enter password:", forcemask = FALSE)
  expect_equal(result, "mock_password")
})

test_that("readline_masked_rstudio_window falls back to readline_nomask()", {
  local_mocked_bindings(
    has_fun = function(fun) FALSE,  # Simulate an unsupported RStudio version
    readline_nomask = function(msg, silent) "fallback_password"
  )

  result <- readline_masked_rstudio_window("Enter password:", forcemask = FALSE)
  expect_equal(result, "fallback_password")
})

test_that("readline_masked_rstudio_window errors if forcemask = TRUE and masking is unavailable", {
  local_mocked_bindings(
    has_fun = function(fun) FALSE  # Simulate no password masking support
  )

  expect_error(readline_masked_rstudio_window("Enter password:", forcemask = TRUE),
               "Masked input is not supported in your version of RStudio")
})

test_that("readline_masked_tcltk_window handles password entry correctly", {
  # Skip test if Tcl/Tk is not available (headless environments like GitHub Actions)
  if (!capabilities("tcltk") || Sys.getenv("GITHUB_ACTIONS") == "true") {
    skip("Tcl/Tk not available in headless environments like GitHub Actions")
  }

  # Create a real Tcl/Tk window to use as a mock
  real_window <- tcltk::tktoplevel()

  # Create real Tcl/Tk variables
  real_pwdvar <- tcltk::tclVar("")
  real_flagvar <- tcltk::tclVar(0)

  # Stub `tclVar()` to return real Tcl/Tk variables
  mockery::stub(readline_masked_tcltk_window, "tcltk::tclVar", function(value) {
    if (value == "") return(real_pwdvar)
    else return(real_flagvar)
  })

  # mockery::stub `tclvalue()` to simulate user inputs
  mockery::stub(readline_masked_tcltk_window, "tcltk::tclvalue", function(var) {
    if (identical(var, real_flagvar)) return("1")  # Simulate submit flag
    if (identical(var, real_pwdvar)) return("my_secure_password")  # Simulate password entry
    return("")
  })

  # mockery::stub `tktoplevel()` to return the real Tk window
  mockery::stub(readline_masked_tcltk_window, "tcltk::tktoplevel", function(...) real_window)

  # mockery::stub other UI functions to prevent actual interactions
  mockery::stub(readline_masked_tcltk_window, "tcltk::tkframe", function(...) NULL)
  mockery::stub(readline_masked_tcltk_window, "tcltk::tkpack", function(...) NULL)
  mockery::stub(readline_masked_tcltk_window, "tcltk::tklabel", function(...) NULL)
  mockery::stub(readline_masked_tcltk_window, "tcltk::tkentry", function(...) NULL)
  mockery::stub(readline_masked_tcltk_window, "tcltk::tkbind", function(...) NULL)
  mockery::stub(readline_masked_tcltk_window, "tcltk::tkbutton", function(...) NULL)
  mockery::stub(readline_masked_tcltk_window, "tcltk::tkwm.minsize", function(...) NULL)
  mockery::stub(readline_masked_tcltk_window, "tcltk::tkwm.deiconify", function(...) NULL)
  mockery::stub(readline_masked_tcltk_window, "tcltk::tkfocus", function(...) NULL)
  mockery::stub(readline_masked_tcltk_window, "tcltk::tkwait.window", function(...) NULL)
  mockery::stub(readline_masked_tcltk_window, "tcltk::tkmessageBox", function(...) NULL)

  # Test case: User enters a password
  expect_equal(readline_masked_tcltk_window("Enter password:"), "my_secure_password")

  # Test case: User submits a blank password (allowed)
  mockery::stub(readline_masked_tcltk_window, "tcltk::tclvalue", function(var) {
    if (identical(var, real_flagvar)) return("1")  # Simulate submit
    if (identical(var, real_pwdvar)) return("")  # Simulate blank input
    return("")
  })

  expect_equal(readline_masked_tcltk_window("Enter password:", noblank = FALSE), "")

  # Test case: User cancels (flagvar = 0)
  mockery::stub(readline_masked_tcltk_window, "tcltk::tclvalue", function(var) {
    if (identical(var, real_flagvar)) return("0")  # Simulate cancel
    if (identical(var, real_pwdvar)) return("ignored_password")  # Should be ignored
    return("")
  })

  expect_null(readline_masked_tcltk_window("Enter password:"))

  # Destroy the Tk window after tests complete
  tcltk::tkdestroy(real_window)
})

test_that("get_installed_packages_with_data returns correct package lists", {
  # Mock `.packages(all.available = TRUE)` to simulate installed packages
  mockery::stub(get_installed_packages_with_data, ".packages", function(all.available) {
    return(c("ggplot2", "dplyr", "base", "stats"))
  })
  
  # Mock `utils::data()` to simulate packages with data sets
  mock_utils_data <- function(package) {
    return(list(results = matrix(c("ggplot2", "datasets", "base"), ncol = 1)))
  }
  mockery::stub(get_installed_packages_with_data, "utils::data", mock_utils_data)
  
  # Case 1: with_data = TRUE (should return only packages that have data sets)
  result_with_data <- get_installed_packages_with_data(with_data = TRUE)
  expect_equal(result_with_data, c("ggplot2", "datasets", "base"))
  
  # Case 2: with_data = FALSE (should return all installed packages)
  result_all_packages <- get_installed_packages_with_data(with_data = FALSE)
  expect_equal(result_all_packages, c("ggplot2", "dplyr", "base", "stats"))
})

test_that("get_odk_http_get handles request failure", {
  # Mock `httr::GET` to simulate an error (e.g., invalid URL)
  mock_GET <- function(url, auth) {
    stop("Request failed")
  }
  mockery::stub(get_odk_http_get, "httr::GET", mock_GET)
  
  expect_error(get_odk_http_get("invalid_url"), "Request failed")
})

test_that("readline_masked_tcltk_window returns test values", {
  expect_equal(readline_masked_tcltk_window("Enter password:", test_mode = "mock_password"), "mock_password")
  expect_equal(readline_masked_tcltk_window("Enter password:", noblank = TRUE, test_mode = "mock_password"), "mock_password")
})

test_that("ask_for_password returns mocked input", {
  # Mock rstudioapi::askForPassword to return a predefined password
  mock_askForPassword <- mockery::mock("mocked_password")
  
  mockery::stub(ask_for_password, "rstudioapi::askForPassword", mock_askForPassword)
  
  expect_equal(ask_for_password("Enter password:"), "mocked_password")
})

test_that("has_fun returns expected values", {
  # Mock rstudioapi::hasFun to simulate function availability
  mock_hasFun_true <- mockery::mock(TRUE)
  mock_hasFun_false <- mockery::mock(FALSE)
  
  mockery::stub(has_fun, "rstudioapi::hasFun", mock_hasFun_true)
  expect_true(has_fun("askForPassword"))
  
  mockery::stub(has_fun, "rstudioapi::hasFun", mock_hasFun_false)
  expect_false(has_fun("nonExistentFunction"))
})

data_frame <- data.frame(
  Name = c("id", "variety", "lastassessment_grainquality", "lastassessment_yield"),
  label = c("id", "variety", NA, NA),
  class = c("character", "character", "numeric", "numeric"),
  Dependent_Columns = c("count_all", "count_all", NA, NA),
  Has_Dependants = c(TRUE, TRUE, NA, NA),
  Is_Hidden = c(FALSE, FALSE, FALSE, FALSE),
  Is_Key = c(TRUE, TRUE, FALSE, FALSE),
  rankings_index = c(NA, NA, 2, 4),
  Scientific = c(FALSE, FALSE, FALSE, FALSE),
  Signif_Figures = c(NA, NA, 7, 7),
  Tricot_Type = c("id", "variety", "traits", "traits")
)

rankings_object <- list(
  lastassessment_grainquality = c(
    "CSW18 > PBW502 > HW2045", "CSW18 > HD2985 > PBW502",
    "DBW17 > RAJ4120 > HW2045", "CSW18 > PBW343 > RAJ4120",
    "PBW343 > HI1563 > HW2045", "K9107 > HD2824 > PBW502"
  ),
  lastassessment_yield = c(
    "CSW18 > PBW502 > HW2045", "CSW18 > HD2985 > PBW502",
    "DBW17 > RAJ4120 > HW2045", "CSW18 > PBW343 > RAJ4120",
    "PBW343 > HI1563 > HW2045", "K9107 > HD2824 > PBW502"
  )
)


# get_ranking_items
test_that("get_ranking_items returns expected values", {
  # Example 1: Get rankings for 'lastassessment_grainquality' and 'lastassessment_yield'
  vars_to_get <- c("lastassessment_grainquality", "lastassessment_yield")
  result1 <- get_ranking_items(data_frame, vars_to_get, "rankings_index", rankings_object)
  testthat::expect_equal(result1, list(rankings_object$lastassessment_grainquality, rankings_object$lastassessment_yield))
  
  # Example 2: Get rankings for just 'lastassessment_grainquality'
  vars_to_get2 <- c("lastassessment_grainquality")
  result2 <- get_ranking_items(data_frame, vars_to_get2, "rankings_index", rankings_object)
  testthat::expect_equal(result2, list(rankings_object$lastassessment_grainquality))

  # # Example 3: using the default data parameter.
  # rankings_index <- "rankings_index" # setting rankings_index for example 3
  # vars_to_get3 <- c("lastassessment_yield")
  # result3 <- get_ranking_items(data_frame, vars_to_get = vars_to_get3, rankings_object = rankings_object)
  # testthat::expect_equal(result3, list(rankings_object$lastassessment_yield))

  # Use the same test data from above
  vars_to_get <- c("lastassessment_grainquality", "lastassessment_yield")
  
  result <- get_ranking_items(
    data = data_frame,
    vars_to_get = vars_to_get,
    index = "rankings_index",
    rankings_object = rankings_object
  )
  
  expect_type(result, "list")
  expect_length(result, 2)
  expect_equal(names(result), NULL)  # lapply returns unnamed list here
  expect_true(all(sapply(result, is.character)))
  expect_equal(length(result[[1]]), 6)  # Each ranking has 6 entries
})

test_that("get_ranking_items returns correct list for multiple variables", {
  vars_to_get <- c("lastassessment_grainquality", "lastassessment_yield")
  result <- get_ranking_items(
    data = data_frame,
    vars_to_get = vars_to_get,
    index = "rankings_index",
    rankings_object = rankings_object
  )
  expect_type(result, "list")
  expect_length(result, 2)
  expect_true(all(sapply(result, is.character)))
})

test_that("get_ranking_items returns correct list for single variable", {
  result <- get_ranking_items(
    data = data_frame,
    vars_to_get = "lastassessment_yield",
    index = "rankings_index",
    rankings_object = rankings_object
  )
  expect_type(result, "list")
  expect_length(result, 1)
  expect_true(is.character(result[[1]]))
})

test_that("get_ranking_items returns empty list for empty vars_to_get", {
  result <- get_ranking_items(
    data = data_frame,
    vars_to_get = character(),
    index = "rankings_index",
    rankings_object = rankings_object
  )
  expect_equal(result, list())
})

test_that("get_ranking_items errors if vars_to_get is not in data", {
  expect_error(
    get_ranking_items(
      data = data_frame,
      vars_to_get = c("not_a_column"),
      index = "rankings_index",
      rankings_object = rankings_object
    ),
    "Some vars_to_get are not found in the data"
  )
})

test_that("get_ranking_items errors if variable is not in rankings_object", {
  expect_error(
    get_ranking_items(
      data = data_frame,
      vars_to_get = c("lastassessment_grainquality", "lastassessment_overallperf"),  # not in object
      index = "rankings_index",
      rankings_object = rankings_object
    ),
    "Some vars_to_get are not found in the data"
  )
})

test_that("get_ranking_items errors if vars_to_get is not character", {
  expect_error(
    get_ranking_items(
      data = data_frame,
      vars_to_get = 1:3,
      index = "rankings_index",
      rankings_object = rankings_object
    ),
    "`vars_to_get` must be a character vector"
  )
})

test_that("generate_summary_tables() returns a gt table with correct title and structure", {
  # Sample input data
  input_data <- tibble::tibble(
    `summary-variable` = rep("my__summary", 3),
    variable = c("A", "B", "C"),
    value = c(10, 20, 30)
  )
  result <- generate_summary_tables(input_data)
  
  # Test 1: Output is a gt_tbl object
  expect_s3_class(result, "gt_tbl")
  
  # Test 2: The title is set correctly (i.e., "__" replaced with space)
  expect_equal(result$`_heading`$title, "my summary")
  
  # Test 3: The 'summary-variable' column has been removed
  expect_false("summary-variable" %in% names(result$`_data`))
  
  # Test 4: The rest of the table data remains unchanged
  expect_equal(result$`_data`$variable, c("A", "B", "C"))
  expect_equal(result$`_data`$value, c(10, 20, 30))
})

test_that("Table title is generated from `summary-variable`", {
  df <- tibble::tibble(
    `summary-variable` = rep("My__Summary", 3),
    col1 = 1:3,
    col2 = 4:6
  )
  
  gt_table <- generate_summary_tables(df)
  
  expect_s3_class(gt_table, "gt_tbl")
  expect_equal(gt_table$`_heading`$title, "My Summary")
})

test_that("Table title is generated from unique summary and variable", {
  df <- data.frame(
    summary = rep("Mean", 3),
    variable = rep("Age", 3),
    col1 = 1:3
  )
  
  gt_table <- generate_summary_tables(df)
  expect_equal(gt_table$`_heading`$title, "Mean Age")
})

test_that("Table title is generated from unique summary only", {
  df <- data.frame(
    summary = rep("Total", 3),
    variable = c("Age", "Gender", "Income"),
    col1 = 1:3
  )
  
  gt_table <- generate_summary_tables(df)
  expect_equal(gt_table$`_heading`$title, "Total")
})

test_that("Table title is generated from unique variable only", {
  df <- data.frame(
    summary = c("Total", "Mean", "Median"),
    variable = rep("Income", 3),
    col1 = 1:3
  )
  
  gt_table <- generate_summary_tables(df)
  expect_equal(gt_table$`_heading`$title, "Income")
})

test_that("Table title is empty if multiple summary and variable values", {
  df <- data.frame(
    summary = c("Total", "Mean"),
    variable = c("Income", "Age"),
    col1 = 1:2
  )
  
  gt_table <- generate_summary_tables(df)
  expect_equal(gt_table$`_heading`$title, "")
})

test_that("Table returns gt object if there are two summary-variables given", {
  df <- tibble::tibble(
    `summary-variable` = c("Sum", "Mean", "SD"),
    col1 = 1:3,
    col2 = 4:6
  )
  
  gt_table <- generate_summary_tables(df)
  expect_true("gt_tbl" %in% class(gt_table))
})
