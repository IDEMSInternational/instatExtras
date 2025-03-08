test_that("read_corpora processes different data formats correctly", {
  df <- data.frame(A = c("apple", "banana", "cherry"))
  list_data <- list(meta = "metadata", description = "test data", data = df)
  
  result_df <- read_corpora(df)
  expect_s3_class(result_df, "data.frame")
  
  result_list <- read_corpora(list_data)
  expect_s3_class(result_list, "data.frame")
  expect_true("description" %in% colnames(result_list))
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

test_that("view_graph_object saves images or prints", {
  p <- grid::grid.rect()  # Create a simple plot
  output <- view_graph_object(p)
  
  expect_true(inherits(output, "rect"))
})

test_that("view_text_object captures text output", {
  txt_output <- view_text_object("Hello, test!")
  expect_type(txt_output, "character")
})

test_that("view_html_object saves or prints HTML", {
  html_output <- view_html_object("<p>Sample HTML</p>")
  expect_type(html_output, "character")
})

test_that("check_graph correctly records plots", {
  plot_recorded <- check_graph(NULL)
  expect_true(inherits(plot_recorded, "recordedplot") || is.null(plot_recorded))
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

test_that("convert_to_list properly converts character strings to numeric vectors", {
  expect_equal(convert_to_list("c(1,2,3)"), c(1, 2, 3))
  expect_equal(convert_to_list("1:5"), c(1, 5)) # is this how we want it to be though?
  expect_equal(convert_to_list("10"), 10)
})

test_that("getExample retrieves example code", {
  result <- getExample("filter", "dplyr", give.lines = TRUE)
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

test_that("set_library_paths correctly updates library paths", {
  old_paths <- .libPaths()
  set_library_paths("4.1")
  expect_true(any(grepl("R-Instat", .libPaths())))
  .libPaths(old_paths)  # Reset to avoid issues
})