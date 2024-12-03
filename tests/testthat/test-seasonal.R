library(testthat)

test_that("the function seasonal handles errors appropriately", {
  # test with input being a number
  expect_error(seasonal("temp"))

  # test with input being a matrix
  expect_error(seasonal(matrix(c(2, 2, 2, 2), 2, 2)))
})

test_that("the function seasonal outputs the correct coefficients", {
  # load the temp0 data from temp0.rda in the data folder
  # 1st test case
  temp = as.numeric(temp0[, 3]) # historical temperatures at one station
  seasonal1 = seasonal(temp)
  expect_equal(round(c(seasonal1$a, seasonal1$b, seasonal1$c, seasonal1$d), 1),
               c(65.2, 0.0, -17.1, 17.6))

  # 2nd test case
  temp = as.numeric(temp0[, 50]) # historical temperatures at one station
  seasonal2 = seasonal(temp)
  expect_equal(round(c(seasonal2$a, seasonal2$b, seasonal2$c, seasonal2$d), 1),
               c(48.0, 0.0, -23.2, 14.0))
})

test_that("the function seasonal outputs fitted values of the seasonal function in the correct format", {
  # load the temp0 data from temp0.rda in the data folder
  # 1st test case
  temp = as.numeric(temp0[, 3]) # historical temperatures at one station
  seasonal1 = seasonal(temp)
  expect_equal(length(seasonal1$seasonality), 1825)

  # 2nd test case
  temp = as.numeric(temp0[, 50]) # historical temperatures at one station
  seasonal2 = seasonal(temp)
  expect_equal(length(seasonal2$seasonality), 1825)
})
