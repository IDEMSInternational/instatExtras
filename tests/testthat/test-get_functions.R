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

test_that("getPass works in terminal mode", {
  local_mocked_bindings(
    readline_masked_term = function(...) "terminal_password",
    isaterm = function() TRUE
  )
  
  expect_equal(getPass("Enter password: "), "terminal_password")
})

test_that("getPass stops when masking is required but unsupported", {
  local_mocked_bindings(
    isaterm = function() FALSE,
    hastcltk = function() FALSE
  )
  
  expect_error(getPass("Enter password:", forcemask = TRUE), "Masking is not supported")
})

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


###

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

# test_that("readline_masked_tcltk_window captures user input correctly", {
#   skip_if_not_installed("tcltk")  
#   skip_if(Sys.getenv("DISPLAY") == "", "No DISPLAY environment set for Tk")
#   
#   # Create a mock password variable
#   mock_pwd_var <- tcltk::tclVar("user_password")  # Set initial password
#   
#   local_mocked_bindings(
#     tcl_var = function(x) mock_pwd_var,  
#     tcl_value = function(x) tcltk::tclvalue(x),  
#     
#     tk_destroy = function(x) NULL,
#     tk_wait_window = function(x) NULL
#   )
#   
#   result <- readline_masked_tcltk_window("Enter password:", noblank = FALSE)
#   expect_equal(result, "user_password")  
# })
# 
# test_that("readline_masked_tcltk_window returns NULL when user cancels", {
#   skip_if_not_installed("tcltk")  
#   skip_if(Sys.getenv("DISPLAY") == "", "No DISPLAY environment set for Tk")
#   
#   mock_pwd_var <- tcltk::tclVar("")
#   mock_flag_var <- tcltk::tclVar("0")  
#   
#   local_mocked_bindings(
#     tcl_var = function(x) if (x == 0) mock_flag_var else mock_pwd_var,
#     
#     tcl_value = function(x) {
#       if (identical(x, mock_flag_var)) return("0")  
#       return(NULL)  
#     },
#     
#     tk_destroy = function(x) NULL,
#     tk_wait_window = function(x) NULL
#   )
#   
#   result <- readline_masked_tcltk_window("Enter password:", noblank = FALSE)
#   expect_null(result)  
# })
