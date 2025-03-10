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

# test_that("check_graph correctly records plots", {
#   plot_recorded <- check_graph(NULL)
#   expect_true(inherits(plot_recorded, "recordedplot") || is.null(plot_recorded))
# })

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

test_that("getExample retrieves example code", {
  result <- getExample("filter", "dplyr", give.lines = TRUE)
  expect_type(result, "character")
  
  result <- getExample("filter", "dplyr", give.lines = TRUE, echo = TRUE)
  expect_type(result, "character")
})

test_that("frac10, frac20, frac100, frac_den convert decimals to fractions", {
  expect_equal(frac10(0.75), "8/10")
  expect_equal(frac20(0.25), "5/20")
  expect_equal(frac100(0.123), "12/100")
  expect_equal(frac_den(0.333, 3), "1/3")
})

# test_that("monitor_memory returns memory usage in MB", {
#   mem_usage <- monitor_memory()
#   expect_type(mem_usage, "double")
#   expect_gt(mem_usage, 0)
# })

test_that("time_operation times an expression", {
  expect_output(time_operation(Sys.sleep(0.1)), "user  system elapsed")
})

# test_that("set_library_paths correctly updates library paths", {
#   old_paths <- .libPaths()
#   set_library_paths("4.1")
#   expect_true(any(grepl("R-Instat", .libPaths())))
#   .libPaths(old_paths)  # Reset to avoid issues
# })

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
