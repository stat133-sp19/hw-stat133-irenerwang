library(testthat)

context('check that main methods are correct')

test_that("bin_choose works as we want it to", {
  expect_equal(bin_choose(n = 5,k = 2), 10)
  expect_equal(bin_choose(5,1:3), c(5, 10, 10))
  expect_error(bin_choose(5, 8))
  expect_error(bin_choose(-2, -4))
})

test_that("bin_probability works as we want it to", {
  expect_equal(bin_probability(success = 2,trials = 5, prob = 0.5), 0.3125)
  expect_equal(bin_probability(success = 0:2,trials = 5, prob = 0.5), c(0.03125, 0.15625, 0.31250))
  expect_error(bin_probability(5, 8, 3))
  expect_error(bin_probability(-2, 4, 0.9))
  expect_error(bin_probability(5, 3, 0.1))
})

test_that("bin_distribution works as we want it to", {
  expect_equal(bin_distribution(trials = 5, prob = 0.5), data.frame('success' = 0:5, 'probability' = c(0.03125, 0.15625, 0.31250, 0.31250, 0.15625, 0.03125)))
  expect_error(bin_distribution(5, 8, 3))
  expect_error(bin_distribution(-2, 4, 0.9))
  expect_error(bin_distribution(5, 3, 0.1))
})

test_that("bin_distribution works as we want it to", {
  expect_equal(bin_distribution(trials = 5, prob = 0.5), data.frame('success' = 0:5, 'probability' = c(0.03125, 0.15625, 0.31250, 0.31250, 0.15625, 0.03125), 'cumulative' = c(0.03125, 0.18750, 0.50000, 0.81250, 0.96875, 1.00000)))
  expect_error(bin_distribution(5, 8, 3))
  expect_error(bin_distribution(-2, 4, 0.9))
  expect_error(bin_distribution(5, 3, 0.1))
})
