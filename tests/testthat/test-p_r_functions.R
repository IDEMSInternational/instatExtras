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

test_that("beans data with no trait columns works", {
  library(PlackettLuce)
  data(beans)
  beans$id <- 1:nrow(beans)
  
  tricot_structure <- detect_tricot_structure(beans, 
                                              good_suffixes = "best", bad_suffixes = "worst", 
                                              na_candidates = "na_candidates")
  expect_equal(tricot_structure$option_cols, c("variety_a", "variety_b", "variety_c"))
  expect_equal(tricot_structure$ranks, c("A", "B", "C"))
  
  pivot_data <- pivot_tricot(data = beans, 
               data_id_col = "id", option_cols = tricot_structure$option_cols, 
               possible_ranks = tricot_structure$ranks, trait_good = tricot_structure$trait_good_cols, 
               trait_bad = tricot_structure$trait_bad_cols, 
               na_value = tricot_structure$na_candidates)
  expect_true(ncol(pivot_data) == 4)
  expect_equal(names(pivot_data), c("id", "variety", "dummy_variety", "trait"))
})


test_that("plot_pltree runs and returns a ggplot object", {
  library(psychotree)
  library(PlackettLuce)
  
  # using the example from plackettluce
  ## Germany's Next Topmodel 2007 data
  data("Topmodel2007", package = "psychotree")
  ## convert paircomp object to grouped rankings
  R <- as.grouped_rankings(Topmodel2007$preference)
  
  ## fit partition model based on all variables except preference
  ## set npseudo = 0 as all judges rank all models
  tm_tree <- pltree(R ~ ., data = Topmodel2007[, -1], minsize = 5,
                    npseudo = 0)
  
  # Simulate a small rankings dataset
  R <- matrix(c(1, 2, 3,
                1, NA, 2,
                2, 1, 3,
                3, 1, 2,
                2, 3, 1), byrow = TRUE, ncol = 3)
  
  p <- plot_pltree(tm_tree)
  
  expect_true("ggplot" %in% class(p))  # patchwork returns a ggplot subclass
})

test_that("plot_pltree gives informative error on invalid input", {
  expect_error(plot_pltree())
})


test_that("pivot_tricot warns when data and data_plot_trait have conflicting trait info", {
  # ID-Level data with trait_good and trait_bad
  data <- data.frame(
    id = c(1, 2),
    variety_a = c("A", "A"),
    variety_b = c("B", "B"),
    variety_c = c("C", "C"),
    Vigor_pos = c("A", "B"),
    Vigor_neg = c("C", "C"),
    Mould_pos = c("A", "B"),
    Mould_neg = c("C", "C")
  )
  
  data_pivot <- pivot_tricot(data, option_cols = c("variety_a", "variety_b", "variety_c"))
  
  # Plot-Trait Level data with the same IDs and varieties, but slightly conflicting ranks
  data_plot_trait <- data.frame(
    id = c(1, 1, 1, 2, 2, 2, 1, 1, 1, 2, 2, 2),
    item = c("A", "B", "C", "A", "B", "C", "A", "B", "C", "A", "B", "C"),
    trait = c(rep("Vigor", 6), rep("Mould", 6)),
    rank = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3)  # Will mismatch due to data indicating C as worst (not 3rd)
  )
  
  expect_warning(
    out <- pivot_tricot(
      data = data,
      data_plot_trait = data_plot_trait,
      data_id_col = "id",
      data_plot_trait_id_col = "id",
      variety_col = "item",
      trait_col = "trait",
      rank_col = "rank",
      option_cols = c("variety_a", "variety_b", "variety_c"),
      trait_good = "_pos",
      trait_bad = "_neg"
    )
  )
})

