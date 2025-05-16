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

library(gosset)
test_that("pivot_tricot works as expected", {
  data(nicabean)
  nicabean_by_id_item_trait <- nicabean$trial # D
  nicabean_by_id <- nicabean$covar # A 
  nicabean_by_id_variety <- pivot_tricot(data_plot_trait = nicabean_by_id_item_trait, # B
                                         data_plot_trait_id_col = "id",
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
                              data_plot_trait = nicabean_by_id_item_trait,
                              data_plot_trait_id_col = "id",
                              variety_col = "item",
                              trait_col = "trait",
                              rank_col = "rank"))
  
  expect_error(pivot_tricot(data = nicabean_by_id_item_trait,
                            data_plot_trait_id_col = "id",
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

test_that("pivot_tricot works as expected", {
  data(breadwheat)
  
  expect_error(pivot_tricot())
  expect_error(pivot_tricot(data = breadwheat))
  expect_error(pivot_tricot(data_id_variety_trait = breadwheat))
  
})

test_that("pivot_tricot adds new variable", {
  data(nicabean)
  nicabean_by_id_item_trait <- nicabean$trial
  nicabean_by_id <- nicabean$covar
  nicabean_by_id_variety <- pivot_tricot(data = nicabean_by_id, # B
                                         data_id_col = "id",
                                         option_cols = c("variety_a", "variety_b", "variety_c"),
                                         data_plot_trait = nicabean_by_id_item_trait, # B
                                         data_plot_trait_id_col = "id",
                                         variety_col = "item",
                                         trait_col = "trait",
                                         rank_col = "rank")
  expect_true("dummy_variety" %in% names(nicabean_by_id_variety))
})

test_that("error if data is provided but data_id_col is NULL", {
  fake_data <- data.frame(option_a = c("A", "B"), option_b = c("B", "C"), option_c = c("C", "A"))
  
  expect_error(
    pivot_tricot(data = fake_data, data_id_col = NULL),
    regexp = "If `data` is provided, `data_id_col` must also be specified."
  )
})

test_that("error if no columns ending with trait_good or trait_bad found in data", {
  fake_data <- data.frame(id = 1:2, option_a = c("A", "B"), option_b = c("B", "C"), option_c = c("C", "A"))
  
  # Here, we'll set trait_good and trait_bad to something impossible to match
  expect_error(
    pivot_tricot(data = fake_data, data_id_col = "id", trait_good = "_positive", trait_bad = "_negative"),
    regexp = "No columns ending with _positive or _negative found in `data`."
  )
})

test_that("plot_pltree runs and returns a ggplot object", {
  library(psychotree)
  library(PlackettLuce)
  
  # using the example from plackettluce
  ## Germany's Next Topmodel 2007 data
  data("Topmodel2007", package = "psychotree")
  ## convert paircomp object to grouped rankings
  R <- as.grouped_rankings(Topmodel2007$preference)
  ## rankings are grouped by judge
  print(R[1:2,], max = 4)
  ## Topmodel2007[, -1] gives covariate values for each judge
  print(Topmodel2007[1:2, -1])
  
  ## fit partition model based on all variables except preference
  ## set npseudo = 0 as all judges rank all models
  tm_tree <- pltree(R ~ ., data = Topmodel2007[, -1], minsize = 5,
                    npseudo = 0)
  
  ## log-abilities, zero sum contrast
  itempar(tm_tree, log = TRUE)
  
  # Simulate a small rankings dataset
  R <- matrix(c(1, 2, 3,
                1, NA, 2,
                2, 1, 3,
                3, 1, 2,
                2, 3, 1), byrow = TRUE, ncol = 3)
  
  ## plot shows abilities constrained to sum to 1
  plot(tm_tree, abbreviate = 1, yscale = c(0, 0.5))
  ## instead show log-abilities with Anja as reference (need to used index)
  plot(tm_tree, abbreviate = 1, worth = FALSE, ref = 6,
       yscale = c(-1.5, 2.2))
  
  p <- plot_pltree(tm_tree)
  
  expect_true("ggplot" %in% class(p))  # patchwork returns a ggplot subclass
})

test_that("plot_pltree gives informative error on invalid input", {
  expect_error(plot_pltree())
})

