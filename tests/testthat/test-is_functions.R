test_that("hashed_id generates correct hashed values", {
  x <- c("apple", "banana", "cherry")
  result <- hashed_id(x)
  expect_type(result, "character")
  expect_equal(length(result), length(x))
  
  salted_result <- hashed_id(x, salt = "salty")
  expect_type(salted_result, "character")
  expect_equal(length(salted_result), length(x))
})

test_that("import_from_iri handles incorrect source gracefully", {
  expect_error(import_from_iri("INVALID_SOURCE", "daily_0p05", "data", -10, 10, -10, 10, "area"))
})

test_that("import_from_ODK handles incorrect source gracefully", {
  expect_error(import_from_ODK(platform = "invalid_platform"))
  expect_error(import_from_ODK(form_name = "Form A", platform = "ona"))
})

test_that("import_from_ODK correctly retrieves form data from kobo", {
  local_mocked_bindings(
    getPass = function(...) "mock_password",
    
    get_odk_http_get = function(url, auth = NULL) {
      if (grepl("/api/v1/data$", url)) {
        fake_response <- list(
          list(title = "Form A", id = "123"),
          list(title = "Form B", id = "456")
        )
        return(structure(list(content = fake_response, status_code = 200), class = "response"))
      } else if (grepl("/api/v1/data/123$", url)) {
        return(structure(list(content = '{"field1": "value1", "field2": "value2"}', status_code = 200), class = "response"))
      }
    },
    
    get_odk_http_content = function(response, type) {
      if (type == "parse") return(response$content)
      if (type == "text") return(response$content)
    }
  )
  
  result <- import_from_ODK("mock_user", "Form A", "kobo")
  
  expect_true("field1" %in% names(result))
  expect_equal(result$field1, "value1")
  expect_error(import_from_ODK("mock_user", platform = "kobo"))
})

test_that("import_from_ODK handles invalid password correctly", {
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
  expect_error(import_from_ODK("mock_user", form = "form A", platform = "ona"))
})

# Test for invalid password
# test_that("import_from_ODK handles invalid password correctly", {
#   local_mocked_bindings(
#     getPass = function(...) "wrong_password",
# 
#     get_odk_http_get = function(url, auth = NULL) {
#       return(structure(list(status_code = 401), class = "response"))
#     }
#   )
# 
#   expect_error(import_from_ODK("mock_user", "Form A", "kobo"))
# })

# Test for form not found
test_that("import_from_ODK handles missing form correctly", {
  local_mocked_bindings(
    getPass = function(...) "mock_password",
    
    get_odk_http_get = function(url, auth = NULL) {
      fake_response <- list(
        list(title = "Form B", id = "456")  # Form A is missing intentionally
      )
      return(structure(list(content = fake_response, status_code = 200), class = "response"))
    },
    
    get_odk_http_content = function(response, type) {
      if (type == "parse") return(response$content)
    }
  )
  
  expect_error(
    import_from_ODK("mock_user", "Form A", "kobo"),
    "Form A not found in available forms"
  )
})

# test_that("import_from_iri correctly imports minimal monthly area data from CHIRPS_V2P0", {
#   result <- import_from_iri(
#     download_from = "CHIRPS_V2P0",
#     data_file = "monthly_prcp",
#     path = "data",
#     X1 = 10.0,
#     X2 = 10.03,
#     Y1 = 10.0,
#     Y2 = 10.06,
#     get_area_point = "area"
#   )
#   
#   expect_type(result, "list")
#   expect_s3_class(result[[1]], "data.frame")
#   expect_s3_class(result[[2]], "data.frame")
#   expect_true(nrow(result[[1]]) > 0 && nrow(result[[1]]) <= 10)  # Small number of points expected
#   expect_equal(names(result[[2]]), c("X", "Y"))
# })

test_that("import_from_iri handles incorrect source gracefully", {
  expect_error(
    import_from_iri(
      download_from = "INVALID_SOURCE",
      data_file = "daily_0p05",
      path = "",
      X1 = 35,
      X2 = 36,
      Y1 = -1,
      Y2 = 0,
      get_area_point = "area"
    ),
    "Source not specified correctly."
  )
})

test_that("import_from_iri handles incorrect data_file gracefully", {
  expect_error(
    import_from_iri(
      download_from = "CHIRPS_V2P0",
      data_file = "invalid_data_file",
      path = "",
      X1 = 35,
      X2 = 36,
      Y1 = -1,
      Y2 = 0,
      get_area_point = "area"
    ),
    "Data file does not exist for CHIRPS V2P0 data"
  )
})

test_that("import_from_iri handles incorrect data_file gracefully", {
  expect_error(
    import_from_iri(
      download_from = "TAMSAT",
      data_file = "invalid_data_file",
      path = "",
      X1 = 35,
      X2 = 36,
      Y1 = -1,
      Y2 = 0,
      get_area_point = "area"
    ),
    "Data file does not exist for TAMSAT data"
  )
})

test_that("import_from_iri handles incorrect data_file gracefully", {
  expect_error(
    import_from_iri(
      download_from = "NOAA_ARC2",
      data_file = "invalid_data_file",
      path = "",
      X1 = 35,
      X2 = 36,
      Y1 = -1,
      Y2 = 0,
      get_area_point = "area"
    ),
    "Data file does not exist for NOAA ARC2 data"
  )
})

test_that("import_from_iri handles incorrect data_file gracefully", {
  expect_error(
    import_from_iri(
      download_from = "NOAA_RFE2",
      data_file = "invalid_data_file",
      path = "",
      X1 = 35,
      X2 = 36,
      Y1 = -1,
      Y2 = 0,
      get_area_point = "area"
    ),
    "Data file does not exist for NOAA RFE2 data"
  )
})

test_that("import_from_iri handles incorrect data_file gracefully", {
  expect_error(
    import_from_iri(
      download_from = "NASA_TRMM_3B42",
      data_file = "invalid_data_file",
      path = "",
      X1 = 35,
      X2 = 36,
      Y1 = -1,
      Y2 = 0,
      get_area_point = "area"
    ),
    "Data file does not exist for NASA TRMM 3B42 data"
  )
})

test_that("import_from_iri handles incorrect data_file gracefully", {
  expect_error(
    import_from_iri(
      download_from = "NOAA_CMORPH_DAILY",
      data_file = "invalid_data_file",
      path = "",
      X1 = 35,
      X2 = 36,
      Y1 = -1,
      Y2 = 0,
      get_area_point = "area"
    ),
    "Data file does not exist for NOAA CMORPH data"
  )
})

test_that("in_top_n correctly identifies top N values", {
  x <- c(10, 5, 7, 12, 3)
  result <- in_top_n(x, 3)
  expect_equal(sum(result), 3)
  
  weighted_result <- in_top_n(x, 3, wt = c(2, 1, 3, 4, 2), fun = sum)
  expect_equal(sum(weighted_result), 3)
})

test_that("is.binary correctly identifies binary variables", {
  expect_true(is.binary(TRUE))
  expect_true(is.binary(c(0, 1, 1, 0)))
  expect_true(is.binary(factor(c("Yes", "No", "Yes"))))
  expect_false(is.binary(c(1, 2, 3, 4)))
  expect_false(is.binary(as.Date("1994-04-10")))
})

test_that("is.emptyvariable correctly identifies empty variables", {
  expect_false(is.emptyvariable(c("", "abc", "")))
  expect_true(is.emptyvariable(c("", "")))
})

test_that("is.logical.like correctly identifies logical-like objects", {
  expect_true(is.logical.like(TRUE))
  expect_true(is.logical.like(c(TRUE, FALSE)))
  expect_true(is.logical.like(1))
  expect_true(is.logical.like(c(0, 1)))
  expect_false(is.logical.like("TRUE"))
  expect_false(is.logical.like(NULL))
})

test_that("is.NAvariable correctly identifies NA and NULL vectors", {
  expect_true(is.NAvariable(c(NA, NA, NA)))
  expect_true(is.NAvariable(NULL))
  expect_false(is.NAvariable(c(1, 2, 3)))
  expect_false(is.NAvariable(c(TRUE, FALSE, NA)))
})

test_that("is.containVariableLabel correctly identifies TRUE and FALSE values", {
  df <- data.frame(x = 1:10, y = 11:20)
  sjlabelled::set_label(df$x, "Age")
  expect_false(is.containVariableLabel(df$x))
  
  vec <- c(1, 2, 3, 4, 5)
  expect_false(is.containVariableLabel(vec))
})

# Mock function for is.containValueLabel
mock_is.containValueLabel <- function(x) {
  return(!is.null(attr(x, "labels_label")))
}

test_that("is.containPartialValueLabel correctly identifies partial labels", {
  # Case 3: No labels at all (should return FALSE)
  x_no_label <- c(1, 2, 3, 4)
  attr(x_no_label, "labels_label") <- NULL  # Explicitly ensure no labels
  expect_false(is.containPartialValueLabel(x_no_label))
})
