library(testthat)

# devtools::load_all()

test_that("the function calib_c handles errors appropriately", {
  # load the residuals data from residuals.rda in the data folder
  # load the seasonal coefficients data from seasonal_coefs.rda in the data folder
  # specify correct inputs
  residuals1 = matrix(as.numeric(residuals[, 3:5]), 730, 3)
  seasonal_coefs1 = as.numeric(seasonal_coefs[1, 2:5])
  station11 = 1
  start_ind11 = 182
  end_ind11 = 212
  type11 = "CDD"
  market_price1 = 2500

  # test for market_price
  expect_error(calib_c( - 10, residuals1, station11, start_ind11, end_ind11, type11, seasonal_coefs1, Fourier_c))

  # test for residuals
  expect_error(calib_c(market_price1, c(1, 2, 3), station11, start_ind11, end_ind11, type11, seasonal_coefs1, Fourier_c))
  expect_error(calib_c(market_price1, matrix(c(2, 2, 2, 2), 2, 2), station11, start_ind11, end_ind11, type11, seasonal_coefs1, Fourier_c))

  # test for seasonal_coefs
  expect_error(calib_c(market_price1, residuals1, station11, start_ind11, end_ind11, type11, c(1, 2, 3), Fourier_c))

  # test for station
  expect_error(calib_c(market_price1, residuals1, p + 1, start_ind11, end_ind11, type11, seasonal_coefs1, Fourier_c))

  # test for start_ind
  expect_error(calib_c(market_price1, residuals1, station11, "Jan", end_ind11, type11, seasonal_coefs1, Fourier_c))

  # test for end_ind
  expect_error(calib_c(market_price1, residuals1, station11, start_ind11, 400, type11, seasonal_coefs1, Fourier_c))

  # test for type
  expect_error(calib_c(market_price1, residuals1, station11, start_ind11, end_ind11, "Call", seasonal_coefs1, Fourier_c))
})

test_that("the function calib_c outputs the correct risk-neutral parameter and the correct error", {
  # load the residuals data from residuals.rda in the data folder
  # load the seasonal coefficients data from seasonal_coefs.rda in the data folder
  # 1st test case
  residuals1 = matrix(as.numeric(residuals[, 3:5]), 730, 3)
  seasonal_coefs1 = as.numeric(seasonal_coefs[1, 2:5])
  station11 = 1
  start_ind11 = 182
  end_ind11 = 212
  type11 = "CDD"
  market_price1 = 2500
  calib11 = calib_c(market_price1, residuals1, station11, start_ind11,
                  end_ind11, type11, seasonal_coefs1, Fourier_c)
  expect_equal(round(calib11$theta, 2), 0.98) # correct calibration
  expect_lt(calib11$error, 0.05) # in-sample pricing error less than 0.05

  # 2nd test case
  residuals2 = matrix(as.numeric(residuals[, 51:55]), 730, 5)
  seasonal_coefs2 = as.numeric(seasonal_coefs[51, 2:5])
  station21 = 3
  start_ind21 = 1
  end_ind21 = 31
  type21 = "HDD"
  market_price2 = 280
  calib21 = calib_c(market_price2, residuals2, station21, start_ind21,
                  end_ind21, type21, seasonal_coefs2, Fourier_c)
  expect_equal(round(calib21$theta, 2), 1.05) # correct calibration
  expect_lt(calib21$error, 0.19) # in-sample pricing error less than 0.19
})
