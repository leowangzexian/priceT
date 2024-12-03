library(testthat)

test_that("the function seasonal_c handles errors appropriately", {
  # test with input being a matrix
  expect_error(seasonal_c(matrix(c(2, 2, 2, 2), 2, 2)))
})

test_that("the function seasonal_c outputs the correct coefficients", {
  # load the temp0 data from temp0.rda in the data folder
  # 1st test case
  temp = as.numeric(temp0[, 3]) # historical temperatures at one station
  seasonal1 = seasonal_c(temp)
  expect_equal(round(c(seasonal1$a, seasonal1$b, seasonal1$c, seasonal1$d), 1),
               c(-16.8, 0.0, 44.0, 56.4))

  # 2nd test case
  temp = as.numeric(temp0[, 50]) # historical temperatures at one station
  seasonal2 = seasonal_c(temp)
  expect_equal(round(c(seasonal2$a, seasonal2$b, seasonal2$c, seasonal2$d), 1),
               c(0.9, 0.0, -4.3, -0.4))
})

test_that("the function seasonal_c outputs fitted values of the seasonal function in the correct format", {
  # load the temp0 data from temp0.rda in the data folder
  # 1st test case
  temp = as.numeric(temp0[, 3]) # historical temperatures at one station
  seasonal1 = seasonal_c(temp)
  expect_equal(length(seasonal1$seasonality), 1825)

  # 2nd test case
  temp = as.numeric(temp0[, 50]) # historical temperatures at one station
  seasonal2 = seasonal_c(temp)
  expect_equal(length(seasonal2$seasonality), 1825)
})
