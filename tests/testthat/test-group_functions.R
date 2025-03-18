test_that("read_corpora processes different data formats correctly", {
  df <- data.frame(A = c("apple", "banana", "cherry"))
  list_data <- list(meta = "metadata", description = "test data", data = df)
  result_df <- read_corpora(df)
  expect_s3_class(result_df, "data.frame")

  result_list <- read_corpora(list_data)
  expect_s3_class(result_list, "data.frame")
  expect_true("description" %in% colnames(result_list))

  df_list <- read_corpora(rcorpora::corpora("foods/pizzaToppings"))
  expect_s3_class(df_list, "data.frame")
})

test_that("Matrix is correctly converted to a data frame", {
  input_matrix <- matrix(c("a", "b", "c", "d"), nrow = 2, byrow = TRUE)
  input_matrix <- list(input_matrix, list(NA))
  expected_output <- data.frame(variable1 = "1", list = c("a-b", "c-d", "a", "c", "b", "d", NA))
  
  result <- read_corpora(input_matrix)
  
  expect_equal(result$list, expected_output$list)
})

test_that("Lists are correctly unlisted and processed", {
  input_list <- list(
    first = list(sub1 = "value1", sub2 = "value2"),
    second = "value3"
  )
  
  result <- read_corpora(input_list)
  
  expect_true("variable1" %in% colnames(result))
  expect_true("variable2" %in% colnames(result))
  expect_true("list" %in% colnames(result))
  expect_true("value1" %in% result$list)
  expect_true("value2" %in% result$list)
  expect_true("value3" %in% result$list)
})

test_that("cbind_unique binds data and removes duplicates", {
  df1 <- data.frame(A = 1:3, B = c("x", "y", "z"))
  df2 <- data.frame(B = c("y", "z", "w"), C = c(10, 20, 30))

  result <- cbind_unique(df1, df2, "B")
  expect_true(!"C" %in% colnames(result))
  expect_equal(nrow(result), 3)
})

test_that("view_object_data prints or returns file paths", {
  txt_output <- view_object_data("Hello, world!", "text")
  expect_type(txt_output, "character")

  html_output <- view_object_data("<h1>Header</h1>", "html")
  expect_type(html_output, "character")
})

# test_that("view_graph_object saves images or prints", {
#   p <- grid::grid.rect()  # Create a simple plot
#   output <- view_graph_object(p)
#
#   expect_true(inherits(output, "rect"))
# })

test_that("view_text_object captures text output", {
  txt_output <- view_text_object("Hello, test!")
  expect_type(txt_output, "character")
})

test_that("view_html_object saves or prints HTML", {
  html_output <- view_html_object("<p>Sample HTML</p>")
  expect_type(html_output, "character")
})

# test_that("view_html_object handles list of identical objects", {
#   # Create a mock list of HTML objects (represented as characters for testing)
#   mock_html_list <- list("html1", "html2", "html3")
#   
#   # Expected output
#   expected_output <- lapply(mock_html_list, process_html_object)
#   
#   # Run function
#   result <- view_html_object(mock_html_list)
#   
#   # Check if output matches expected
#   expect_equal(result, expected_output)
# })

# test_that("view_html_object returns NULL for non-list input", {
#   non_list_input <- "html_object"
#   result <- view_html_object(non_list_input)
#   expect_equal(result, "html_object")
# })

test_that("view_graph_object handles different graph types correctly", {
  skip_if_not(Sys.getenv("GITHUB_ACTIONS") == "true")

  # Create different types of graph objects
  ggplot_obj <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) + ggplot2::geom_point()
  grob_obj <- grid::rectGrob()

  # Fix for `recordPlot()` issue: Open a device first
  png(filename = tempfile())  # Open a graphics device
  plot(1:10, 1:10)  # Create a simple base R plot
  base_plot_obj <- recordPlot()  # Now record it
  dev.off()  # Close the temporary device

  # Mock getOption("viewer") to simulate no RStudio viewer available
  mockery::stub(view_graph_object, "getOption", function(x) NULL)

  # Case 1: No viewer available (should return a file path)
  file_path <- view_graph_object(ggplot_obj)
  expect_type(file_path, "character")  # Ensure it's a string
  expect_match(file_path, "viewgraph.*\\.png")  # Ensure correct file naming

  # Case 3: Base R plot handling
  file_path_base <- view_graph_object(base_plot_obj)
  expect_type(file_path_base, "character")  # Ensure it's a string
  expect_match(file_path_base, "viewgraph.*\\.png")  # Ensure correct file naming

  # Cleanup: Remove saved files
  if (file.exists(file_path)) file.remove(file_path)
  if (file.exists(file_path_base)) file.remove(file_path_base)
})

# test_that("check_graph correctly records plots", {
#   plot_recorded <- check_graph(NULL)
#   expect_true(inherits(plot_recorded, "recordedplot") || is.null(plot_recorded))
# })

test_that("Graph object is recorded correctly", {
  # Create a simple plot
  plot(1:10, 1:10)
  graph_obj <- grDevices::recordPlot()
  
  result <- check_graph(graph_obj)
  
  expect_true(!is.null(result))
  expect_s3_class(result, "recordedplot")
})

test_that("Function handles NULL input gracefully", {
  result <- check_graph(NULL)
  
  expect_s3_class(result, "recordedplot")
})

test_that("check_graph returns the input graph object if not NULL", {
  # Create a simple ggplot graph
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, cyl)) + ggplot2::geom_point()

  # Run function
  result <- check_graph(p)

  # Expect that result is the same as input
  expect_equal(result, p)
})

test_that("check_graph captures a recorded plot when given NULL", {
  # Ensure there is an active plot
  plot(1:10, 1:10)

  # Run function with NULL input
  result <- check_graph(NULL)

  # Expect result to be a recorded plot
  expect_true(inherits(result, "recordedplot"))
})

test_that("check_graph does not modify non-graph objects", {
  non_graph <- "not_a_graph"
  result <- check_graph(non_graph)

  # Expect the function to return the same non-graph object
  expect_equal(result, non_graph)
})

test_that("get_vignette retrieves vignette information", {
  result <- get_vignette("dplyr")
  expect_type(result, "character")
})

test_that("getRowHeadersWithText retrieves correct row names", {
  df <- data.frame(Name = c("apple", "banana", "cherry"), ID = 1:3, row.names = c("Row1", "Row2", "Row3"))

  result <- getRowHeadersWithText(df, "Name", "banana", FALSE, FALSE, FALSE)
  expect_equal(result, "Row2")
})

test_that("getRowHeadersWithText handles regex and exact match cases correctly", {
  # Create a test data frame
  data <- data.frame(
    ID = c("A101", "B202", "C303", "D404", "E505"),
    Name = c("Alice", "Bob", "Charlie", "David", "Alice"),
    stringsAsFactors = FALSE
  )
  rownames(data) <- c("Row1", "Row2", "Row3", "Row4", "Row5")

  # Case 1: use_regex = TRUE and match_entire_cell = TRUE
  result1 <- getRowHeadersWithText(data, column = "Name", searchText = "Alice", use_regex = TRUE, match_entire_cell = TRUE)
  expect_equal(result1, c("Row1", "Row5"))

  # Case 2: use_regex = FALSE and match_entire_cell = TRUE
  result2 <- getRowHeadersWithText(data, column = "Name", searchText = "Alice", use_regex = FALSE, match_entire_cell = TRUE)
  expect_equal(result2, c("Row1", "Row5"))
})

test_that("convert_to_list properly converts character strings to numeric vectors", {
  expect_equal(convert_to_list("c(1,2,3)"), c(1, 2, 3))
  expect_equal(convert_to_list("1:5"), c(1, 5)) # is this how we want it to be though?
  expect_equal(convert_to_list("10"), 10)
})

test_that("getExample works with character.only = FALSE", {
  # Use a non-character input (unquoted function name)
  result <- getExample(mean, package = "base", give.lines = TRUE, character.only = FALSE)
  
  # Expect the result to be a non-empty character string
  expect_type(result, "character")
  expect_true(nchar(result) > 0)
  
  # Expect that calling with a quoted function name gives the same result
  result2 <- getExample("mean", package = "base", give.lines = TRUE, character.only = TRUE)
  expect_equal(result, result2)
})

test_that("getExample handles invalid topics with character.only = FALSE", {
  # Use an unquoted function name that doesn't exist
  expect_warning(result <- getExample(non_existent_function, package = "base", give.lines = TRUE, character.only = FALSE))
  
  # Expect an empty character vector
  expect_equal(result, character())
})

test_that("getExample handles missing topics gracefully", {
  # Expect a warning and empty character output
  expect_warning(getExample("non_existent_function", package = "base", give.lines = TRUE))
})

test_that("getExample runs with verbose mode", {
  result <- getExample("sum", package = "base", give.lines = TRUE, verbose = TRUE)
  
  # Expect result to be character output
  expect_type(result, "character")
  expect_true(nchar(result) > 0)
})

test_that("getExample respects the echo argument", {
  expect_output(getExample("mean", package = "base", echo = TRUE), "mean")
  expect_silent(getExample("mean", package = "base", echo = FALSE))
})

test_that("getExample handles run.dontrun and run.donttest correctly", {
  result <- getExample("mean", package = "base", give.lines = TRUE, run.dontrun = TRUE, run.donttest = TRUE)
  
  # Expect non-empty output with potentially "dontrun" and "donttest" blocks included
  expect_type(result, "character")
  expect_true(nchar(result) > 0)
})

test_that("getExample returns empty when no examples exist", {
  result <- getExample("library", package = "base", give.lines = TRUE)
  
  # Expect an empty character vector
  expect_equal(class(result), "character")
})

test_that("getExample respects the prompt.prefix argument", {
  result <- getExample("mean", package = "base", give.lines = TRUE, prompt.prefix = "TEST")
  
  # Expect a character output
  expect_type(result, "character")
  expect_true(nchar(result) > 0)
})

test_that("frac10, frac20, frac100, frac_den convert decimals to fractions", {
  expect_equal(frac10(0.75), "8/10")
  expect_equal(frac20(0.25), "5/20")
  expect_equal(frac100(0.123), "12/100")
  expect_equal(frac_den(0.333, 3), "1/3")
})

test_that("monitor_memory returns memory usage in MB", {
  mem_usage <- monitor_memory()
  expect_type(mem_usage, "double")
  expect_gt(mem_usage, 0)
})

test_that("Memory usage is returned correctly", {
  result <- monitor_memory()
  
  expect_true(is.numeric(result))
  expect_true(result > 0)
})

test_that("time_operation times an expression", {
  expect_output(time_operation(Sys.sleep(0.1)), "user  system elapsed")
})

# test_that("set_library_paths correctly updates library paths", {
#   old_paths <- .libPaths()
#   set_library_paths("4.1")
#   expect_true(any(grepl("R-Instat", .libPaths())))
#   .libPaths(old_paths)  # Reset to avoid issues
# })

test_that("check_github_repo handles url", {
  result <- check_github_repo(url = "https://github.com/hadley/ggplot2")
  expect_equal(result, 3)
})

test_that("check_github_repo handles various scenarios", {
  # Mock `gh::gh` to return a specific response
  mock_gh <- mockery::mock(
    list(list(sha = "latest_sha")),  # Mock response for latest commit
    list(language = "R"),            # Mock response for repo language
    cycle = TRUE                      # Cycle through responses
  )

  mockery::stub(check_github_repo, "gh::gh", mock_gh)

  # Mock `requireNamespace` to simulate installed or missing packages
  mock_requireNamespace <- mockery::mock(TRUE, FALSE, cycle = TRUE)
  mockery::stub(check_github_repo, "requireNamespace", mock_requireNamespace)

  # Mock `utils::packageDescription` to return a fake SHA
  mock_packageDescription <- function(pkg) {
    list(GithubSHA1 = if (pkg == "installed_repo") "latest_sha" else "old_sha")
  }
  mockery::stub(check_github_repo, "utils::packageDescription", mock_packageDescription)

  # Case 1: Installed package, latest commit matches
  expect_equal(check_github_repo(owner = "user", repo = "installed_repo"), 0)

  # Case 2: Installed package, latest commit differs
  expect_equal(check_github_repo(owner = "user", repo = "outdated_repo"), 4)

  # Case 3: Installed package but no local SHA
  mock_packageDescription_no_sha <- function(pkg) list(GithubSHA1 = NULL)
  mockery::stub(check_github_repo, "utils::packageDescription", mock_packageDescription_no_sha)
  expect_equal(check_github_repo(owner = "user", repo = "no_sha_repo"), 3)

  # Case 4: Not installed, R language repo
  expect_equal(check_github_repo(owner = "user", repo = "r_project"), 6)

  # Case 5: Not installed, non-R repo
  mockery::stub(check_github_repo, "gh::gh", mockery::mock(list(language = "Python")))
  expect_equal(check_github_repo(owner = "user", repo = "python_project"), 3)

  # Case 6: Non-existent repo
  mockery::stub(check_github_repo, "gh::gh", function(...) stop("Not Found"))
  expect_equal(check_github_repo(owner = "user", repo = "non_existent"), 6)
})

test_that("check_github_repo handles various scenarios", {
  # Mock `gh::gh` to return specific responses for API calls
  mock_gh <- mockery::mock(
    list(list(sha = "latest_sha")),  # Mock response for latest commit (used in case 1 & 2)
    list(language = "Python"),       # Mock response for non-R repo (used in case 5)
    cycle = TRUE                     # Cycle through responses
  )
  mockery::stub(check_github_repo, "gh::gh", mock_gh)
  
  # Mock `requireNamespace` to simulate installed or missing packages
  mock_requireNamespace <- mockery::mock(TRUE, TRUE, FALSE, cycle = TRUE)
  mockery::stub(check_github_repo, "requireNamespace", mock_requireNamespace)
  
  # Mock `utils::packageDescription` to return different SHAs for comparison
  mock_packageDescription <- function(pkg) {
    list(GithubSHA1 = if (pkg == "outdated_repo") "old_sha" else "latest_sha")
  }
  mockery::stub(check_github_repo, "utils::packageDescription", mock_packageDescription)
  
  # Case 1: Installed package, latest commit differs → should return 1
  expect_equal(check_github_repo(owner = "user", repo = "outdated_repo"), 1)
  
  # Case 2: Unable to retrieve latest commit from GitHub → should return 2
  mockery::stub(check_github_repo, "gh::gh", function(...) stop("GitHub API Error"))
  expect_equal(check_github_repo(owner = "user", repo = "api_error_repo"), 2)
  
  # Case 5: Repository exists but is not an R package → should return 5
  mockery::stub(check_github_repo, "gh::gh", function(...) list(language = "Python"))
  expect_equal(check_github_repo(owner = "user", repo = "python_project"), 5)
})


test_that("set_library_paths updates library paths correctly", {
  # Mock APPDATA environment variable
  mock_sys_getenv <- function(var) {
    if (var == "APPDATA") return("C:/Fake/AppData") else return("")
  }
  mockery::stub(set_library_paths, "Sys.getenv", mock_sys_getenv)

  # Mock dir.create to avoid real directory creation
  mock_dir_create <- mockery::mock(NULL)
  mockery::stub(set_library_paths, "dir.create", mock_dir_create)

  # Simulated library paths variable
  mocked_lib_paths <- c("C:/Existing/Library1", "C:/Existing/Library2")

  # Mock .libPaths using a helper function
  fake_libPaths <- function(new = NULL) {
    if (is.null(new)) {
      return(mocked_lib_paths)
    } else {
      mocked_lib_paths <<- new
    }
  }

  # Use `mockery::stub()` to replace .libPaths() with fake_libPaths()
  mockery::stub(set_library_paths, ".libPaths", fake_libPaths)

  # Run function
  set_library_paths("4.3")

  # Expected new library path
  expected_new_path <- "C:/Fake/AppData/R-Instat/4.3/library"

  # Verify dir.create was called with the correct path
  mockery::expect_called(mock_dir_create, 1)
  mockery::expect_args(mock_dir_create, 1, expected_new_path, recursive = TRUE, showWarnings = FALSE)

  # Verify .libPaths() was updated correctly
  expect_true(expected_new_path %in% mocked_lib_paths)

  # Verify it maintains only the valid paths (first and third if applicable)
  if (length(mocked_lib_paths) > 2) {
    expect_equal(mocked_lib_paths, c(expected_new_path, "C:/Existing/Library2"))
  }
})

test_that("check_graph catches warnings and returns NULL", {
  warning_graph <- function() {
    warning("This is a test warning")
    return(NULL)
  }
  
  expect_warning(check_graph(warning_graph()))
})

test_that("check_graph catches error and returns NULL", {
  error_graph <- function() {
    stop("This is a test error")
    return(NULL)
  }
  
  expect_error(check_graph(error_graph()))
})
