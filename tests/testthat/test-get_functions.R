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