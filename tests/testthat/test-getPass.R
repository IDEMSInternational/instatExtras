library(mockery)

# Mocking tcltk functions to simulate user behavior
test_that("readline_masked_tcltk_window returns correct values", {
  skip_if_not(requireNamespace("tcltk", quietly = TRUE), "tcltk package is required")
  
  mock_tclvalue <- function(var) {
    if (var == "pwdvar") {
      return("test_password")  # Simulate user input
    } else if (var == "flagvar") {
      return(1)  # Simulate successful submission
    }
    return(NULL)
  }
  
  mock_tcltk <- mock(
    "test_password",  # Simulate password input
    "1"  # Simulate flagvar being set to 1 (submitted)
  )
  
  stub(readline_masked_tcltk_window, "tcltk::tclvalue", mock_tclvalue)
  
  expect_equal(readline_masked_tcltk_window("Enter password:"), "test_password")
})


test_that("readline_masked_tcltk_window handles blank input with noblank = TRUE", {
  skip_if_not(requireNamespace("tcltk", quietly = TRUE), "tcltk package is required")
  
  mock_tclvalue <- function(var) {
    if (var == "pwdvar") {
      return("")  # Simulate blank input
    } else if (var == "flagvar") {
      return(1)  # Simulate user attempting submission
    }
    return(NULL)
  }
  
  stub(readline_masked_tcltk_window, "tcltk::tclvalue", mock_tclvalue)
  
  expect_message(readline_masked_tcltk_window("Enter password:", noblank = TRUE), "No blank input please!")
})


test_that("readline_masked_tcltk_window returns NULL when user cancels", {
  skip_if_not(requireNamespace("tcltk", quietly = TRUE), "tcltk package is required")
  
  mock_tclvalue <- function(var) {
    if (var == "flagvar") {
      return(0)  # Simulate cancel action
    }
    return(NULL)
  }
  
  stub(readline_masked_tcltk_window, "tcltk::tclvalue", mock_tclvalue)
  
  expect_null(readline_masked_tcltk_window("Enter password:"))
})
