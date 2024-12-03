library(testthat)

# devtools::load_all()

test_that("the function temp_hedge handles errors appropriately", {
  # load the residuals data from residuals.rda in the data folder
  # specify correct inputs
  residuals1 = matrix(as.numeric(residuals[, 3:5]), 730, 3)
  station1 = 1

  # test for residuals
  expect_error(temp_hedge(c(1, 2, 3), station1))
  expect_error(temp_hedge(matrix(c(2, 2, 2, 2), 2, 2), station1))

  # test for station
  expect_error(temp_hedge(residuals1, p + 1))
})

test_that("the CAT volatilities outputted from the function temp_hedge are in the correct format", {
  # load the residuals data from residuals.rda in the data folder
  # 1st test case
  residuals1 = matrix(as.numeric(residuals[, 3:5]), 730, 3)
  station1 = 1
  temp_hedge1 = temp_hedge(residuals1, station1)

  expect_equal(length(temp_hedge1$vol), 365 - 31) # includes CAT volatilities from Jan to Nov

  # 2nd test case
  residuals2 = matrix(as.numeric(residuals[, 51:55]), 730, 5)
  station2 = 3
  temp_hedge2 = temp_hedge(residuals2, station2)

  expect_equal(length(temp_hedge2$vol), 365 - 31) # includes CAT volatilities from Jan to Nov
})
