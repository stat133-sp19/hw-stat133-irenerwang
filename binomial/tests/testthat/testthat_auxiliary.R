library(testthat)

context('check that statistics are correct')

test_that("aux_mean works as we want it to", {
  expect_equal(aux_mean(10, 0.3), 3)
  expect_equal(aux_mean(12, 0.8), 9.6)
  expect_error(aux_mean(10))
})

test_that("aux_variance works as we want it to", {
  expect_equal(aux_variance(10, 0.3), 2.1)
  expect_equal(aux_variance(12, 0.8), 1.92)
  expect_error(aux_variance(10))
})

test_that("aux_mode works as we want it to", {
  expect_equal(aux_mode(10, 0.3), 3)
  expect_equal(aux_mode(12, 0.8), 10)
  expect_error(aux_mode(10))
})

test_that("aux_skewness works as we want it to", {
  expect_equal(aux_skewness(10, 0.3), 0.2760262)
  expect_equal(aux_skewness(12, 0.8), -0.4330127)
  expect_error(aux_skewness(10))
})

test_that("aux_kurtosis works as we want it to", {
  expect_equal(aux_kurtosis(10, 0.3), -0.1238095)
  expect_equal(aux_kurtosis(12, 0.8), 0.02083333)
  expect_error(aux_kurtosis(10))
})
