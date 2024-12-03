library(testthat)

# devtools::load_all()

test_that("the function temp_forecast handles errors appropriately", {
  # load the residuals data from residuals.rda in the data folder
  # load the temp0 data from temp0.rda in the data folder
  # load the seasonal coefficients data from seasonal_coefs.rda in the data folder
  # specify correct inputs
  residuals1 = matrix(as.numeric(residuals[, 3:5]), 730, 3)
  seasonal_coefs1 = as.numeric(seasonal_coefs[1, 2:5])
  temp = as.numeric(temp0[, 3]) # historical temperatures at one station
  station11 = 1

  # test for residuals
  expect_error(temp_forecast(c(1, 2, 3), station11, seasonal_coefs1, temp))
  expect_error(temp_forecast(matrix(c(2, 2, 2, 2), 2, 2), station11, seasonal_coefs1, temp))

  # test for seasonal_coefs
  expect_error(temp_forecast(residuals1, station11, c(1, 2, 3), temp))

  # test for temp
  expect_error(temp_forecast(residuals1, station11, seasonal_coefs1, matrix(c(2, 2, 2, 2), 2, 2)))

  # test for station
  expect_error(temp_forecast(residuals1, p + 1, seasonal_coefs1, temp))
})

test_that("the outputs of the function temp_forecast are in the correct format", {
  # load the residuals data from residuals.rda in the data folder
  # load the temp0 data from temp0.rda in the data folder
  # load the seasonal coefficients data from seasonal_coefs.rda in the data folder
  # 1st test case
  residuals1 = matrix(as.numeric(residuals[, 3:5]), 730, 3)
  seasonal_coefs1 = as.numeric(seasonal_coefs[1, 2:5])
  temp = as.numeric(temp0[, 3]) # historical temperatures at one station
  station11 = 1
  temp_f1 = temp_forecast(residuals1, station11, seasonal_coefs1, temp)

  expect_equal(length(temp_f1$point), 365)
  expect_equal(length(temp_f1$lower), 365)
  expect_equal(length(temp_f1$upper), 365)
  expect_equal(length(temp_f1$newtemps), 2190)

  # 2nd test case
  residuals2 = matrix(as.numeric(residuals[, 51:55]), 730, 5)
  seasonal_coefs2 = as.numeric(seasonal_coefs[51, 2:5])
  temp = as.numeric(temp0[, 53]) # historical temperatures at one station
  station21 = 3
  temp_f2 = temp_forecast(residuals2, station21, seasonal_coefs2, temp)

  expect_equal(length(temp_f2$point), 365)
  expect_equal(length(temp_f2$lower), 365)
  expect_equal(length(temp_f2$upper), 365)
  expect_equal(length(temp_f2$newtemps), 2190)
})
