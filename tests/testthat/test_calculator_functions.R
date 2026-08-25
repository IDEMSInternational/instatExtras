library(testthat)

test_that("ssq calculates uncorrected sum of squares correctly", {
  expect_equal(ssq(c(8, 2, 5)), 93)
  expect_equal(ssq(c(1, 2, 3, NA)), 14)
  expect_equal(ssq(c(0, 0, 0)), 0)
})


test_that("cssq calculates corrected sum of squares correctly", {
  expect_equal(cssq(c(2, 8, 5)), 18)
  expect_equal(cssq(c(1, 2, 3, 4)), 5)
  expect_equal(cssq(c(5, 5, 5)), 0)
})


test_that("digitsum calculates sum of digits correctly", {
  expect_equal(digitsum(c(8, 23, 471)), c(8, 5, 12))
  expect_equal(digitsum(c(100, 999)), c(1, 27))
})


test_that("digitsqu calculates squared digits correctly", {
  expect_equal(
    digitsqu(c(8, 23, 471)),
    list(c(64), c(4, 9), c(16, 49, 1))
  )
})


test_that("digitssq calculates sum of squared digits correctly", {
  expect_equal(digitssq(c(8, 23, 471)), c(64, 13, 66))
  expect_equal(digitssq(c(10, 99)), c(1, 162))
})


test_that("pascal generates Pascal triangle coefficients correctly", {
  expect_equal(
    pascal(c(1, 2, 3, 4)),
    list(
      c(1, 1),
      c(1, 2, 1),
      c(1, 3, 3, 1),
      c(1, 4, 6, 4, 1)
    )
  )
  
  expect_equal(
    pascal(c(0, 5)),
    list(
      c(1),
      c(1, 5, 10, 10, 5, 1)
    )
  )
})


test_that("fractions converts decimals to fractions correctly", {
  expect_equal(
    fractions(c(0.75, 2.3, 0.28)),
    c("3/4", "23/10", "7/25")
  )
  
  expect_equal(fractions(0.5), "1/2")
})



test_that("decimals converts fractions to decimals correctly", {
  expect_equal(
    decimals(c("3/4", "23/10", "7/25")),
    c(0.75, 2.3, 0.28)
  )
  
  expect_equal(decimals(c("1/2", "3/2")), c(0.5, 1.5))
})


test_that("fractions and decimals are inverse operations", {
  values <- c(0.25, 0.5, 1.75)
  
  expect_equal(
    decimals(fractions(values)),
    values
  )
})

