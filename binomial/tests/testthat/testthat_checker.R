library(testthat)

context('check that parameters are valid')

test_that("check_prob works as we want it to", {
  expect_true(check_prob(0.1))
  expect_true(check_prob(0.3))
  expect_true(check_prob(0.7))
  expect_error(check_prob(c(0.1, 0.3)))
  expect_error(check_prob(9))
})

test_that("check_trials works as we want it to", {
  expect_true(check_trials(9))
  expect_true(check_trials(3))
  expect_error(check_trials(-2))
})

test_that("check_success works as we want it to", {
  expect_true(check_success(9, 10))
  expect_error(check_success(3, 2))
  expect_error(check_success(3, -5))
  expect_error(check_success(-2, 2))
  expect_error(check_success(-4, -2))
})
