test_that("hashed_id generates correct hash", {
  x <- c("apple", "banana", "cherry")
  hash_no_salt <- hashed_id(x)
  hash_with_salt <- hashed_id(x, salt = "salty")
  
  expect_true(is.character(hash_no_salt))
  expect_true(is.character(hash_with_salt))
  expect_equal(length(hash_no_salt), length(x))
  expect_equal(length(hash_with_salt), length(x))
})

test_that("pentad function returns correct pentad values", {
  expect_equal(pentad(as.Date("2023-01-01")), 1)
  expect_equal(pentad(as.Date("2023-07-17")), 40)
  expect_equal(pentad(as.Date("2023-12-31")), 72)
})

test_that("record_graph records a plot", {
  plot(1:10, 1:10)
  recorded <- record_graph(1)
  expect_s3_class(recorded, "recordedplot")
})

test_that("return_variable_levels generates JavaScript function", {
  df <- data.frame(
    category1 = factor(c("low", "medium", "high"), levels = c("low", "medium", "high")),
    category2 = factor(c("A", "B", "C"), levels = c("A", "B", "C"))
  )
  js_code <- return_variable_levels(df)
  expect_true(is.character(js_code))
  expect_true(grepl("function", js_code))
})

test_that("package_check returns expected output", {
  result <- package_check("ggplot2")
  expect_true(is.list(result))
  expect_true(result[[1]] %in% c("1", "2", "3", "4", "5"))
})

test_that("pivot_tricot produces expected output", {
  # Create a mock dataset
  test_data <- data.frame(
    id = c(1, 2),
    option_a = c("Variety1", "Variety2"),
    option_b = c("Variety2", "Variety3"),
    option_c = c("Variety3", "Variety1"),
    trait1_pos = c("A", "A"),
    trait1_neg = c("C", "B"),
    trait2_pos = c("A", "Not observed"),
    trait2_neg = c("B", "C")
  )
  
  # Run function
  result <- pivot_tricot(test_data)
  
  # Check column names
  expect_true(all(c("id", "variety", "trait1", "trait2") %in% names(result)))
  
  # Check if ranks are correctly assigned
  expect_equal(result$trait1[result$variety == "Variety1"], c(1, 2))

  # Check for missing values handling
  expect_false(any(is.na(result$variety)))
})

test_that("package_check handles different package states correctly", {
  # Mock `pingr::is_online` to simulate online/offline status
  mockery::stub(package_check, "pingr::is_online", function() TRUE)
  
  # Mock `create_av_packs` to provide a dummy CRAN package list
  mock_create_av_packs <- function() {
    av_packs <<- data.frame(
      Package = c("ggplot2", "zzlite"),
      Version = c("3.3.5", "1.1.0"),
      stringsAsFactors = FALSE
    )
  }
  mockery::stub(package_check, "create_av_packs", mock_create_av_packs)
  
  # Mock `utils::installed.packages` to simulate installed packages
  mockery::stub(utils::installed.packages, "installed.packages", function() {
    matrix(c("ggplot2", "3.3.5"), ncol = 2, dimnames = list(NULL, c("Package", "Version")))
  })
  
  # Case 1: Installed CRAN package → out[[1]] should be "1"
  result <- package_check("ggplot2")
  expect_equal(result[[1]], "1")
  
  # Case 2: CRAN package but NOT installed → out[[1]] should be "2"
  result <- package_check("zzlite")
  expect_equal(result[[1]], "2")
  
  # Case 3: Non-existent package (not in CRAN, not installed) → out[[1]] should be "4"
  result <- package_check("nonexistentpkg")
  expect_equal(result[[1]], "4")
  
  # Case 4: Offline mode → out[[1]] should be "5"
  mockery::stub(package_check, "pingr::is_online", function() FALSE)
  result <- package_check("ggplot2")
  expect_equal(result[[1]], "5")
})

test_that("pivot_tricot works as expected", {
  library(gosset)
  data(nicabean)
  nicabean_by_id_item_trait <- nicabean$trial # D
  nicabean_by_id <- nicabean$covar # A 
  nicabean_by_id_variety <- pivot_tricot(data_id_variety_trait = nicabean_by_id_item_trait, # B
                                         data_id_variety_trait_id_col = "id",
                                         variety_col = "item",
                                         trait_col = "trait",
                                         rank_col = "rank")
  expect_true("id" %in% names(nicabean_by_id_variety))
  expect_true("variety" %in% names(nicabean_by_id_variety))
  
  nicabean_by_id$Vigor_pos = "A"
  nicabean_by_id$Vigor_neg = "C"
  expect_warning(pivot_tricot(data = nicabean_by_id,
                              data_id_col = "id",
                              option_cols = c("variety_a", "variety_b", "variety_c"),
                              data_id_variety_trait = nicabean_by_id_item_trait,
                              data_id_variety_trait_id_col = "id",
                              variety_col = "item",
                              trait_col = "trait",
                              rank_col = "rank"))
  
  data(breadwheat)
  nicabean_by_id_variety_3 <- pivot_tricot(data = breadwheat,
                                           data_id_col = "participant_name",
                                           option_cols = c("variety_a", "variety_b", "variety_c"),
                                           trait_good = "best", trait_bad = "_worst")
  expect_true("id" %in% names(nicabean_by_id_variety_3))
  expect_true("variety" %in% names(nicabean_by_id_variety_3))
})