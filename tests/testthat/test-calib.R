library(testthat)

# devtools::load_all()

test_that("the function calib handles errors appropriately", {
  # load the residuals data from residuals.rda in the data folder
  # load the seasonal coefficients data from seasonal_coefs.rda in the data folder
  # specify correct inputs
  residuals1 = matrix(as.numeric(residuals[, 3:5]), 730, 3)
  seasonal_coefs1 = as.numeric(seasonal_coefs[1, 2:5])
  station11 = 1
  start_ind11 = 182
  end_ind11 = 212
  type11 = "CDD"
  market_price1 = 600

  # test for market_price
  expect_error(calib( - 10, residuals1, station11, start_ind11, end_ind11, type11, seasonal_coefs1, Fourier))

  # test for residuals
  expect_error(calib(market_price1, c(1, 2, 3), station11, start_ind11, end_ind11, type11, seasonal_coefs1, Fourier))
  expect_error(calib(market_price1, matrix(c(2, 2, 2, 2), 2, 2), station11, start_ind11, end_ind11, type11, seasonal_coefs1, Fourier))

  # test for seasonal_coefs
  expect_error(calib(market_price1, residuals1, station11, start_ind11, end_ind11, type11, c(1, 2, 3), Fourier))

  # test for station
  expect_error(calib(market_price1, residuals1, p + 1, start_ind11, end_ind11, type11, seasonal_coefs1, Fourier))

  # test for start_ind
  expect_error(calib(market_price1, residuals1, station11, "Jan", end_ind11, type11, seasonal_coefs1, Fourier))

  # test for end_ind
  expect_error(calib(market_price1, residuals1, station11, start_ind11, 400, type11, seasonal_coefs1, Fourier))

  # test for type
  expect_error(calib(market_price1, residuals1, station11, start_ind11, end_ind11, "Call", seasonal_coefs1, Fourier))
})

test_that("the function calib outputs the correct risk-neutral parameter and the correct error", {
  # load the residuals data from residuals.rda in the data folder
  # load the seasonal coefficients data from seasonal_coefs.rda in the data folder
  # 1st test case
  residuals1 = matrix(as.numeric(residuals[, 3:5]), 730, 3)
  seasonal_coefs1 = as.numeric(seasonal_coefs[1, 2:5])
  station11 = 1
  start_ind11 = 182
  end_ind11 = 212
  type11 = "CDD"
  market_price1 = 600
  calib11 = calib(market_price1, residuals1, station11, start_ind11,
                  end_ind11, type11, seasonal_coefs1, Fourier)
  calib12 = calib(market_price1, residuals1, station11, start_ind11,
                  end_ind11, type11, seasonal_coefs1, adaptBW)
  calib13 = calib(market_price1, residuals1, station11, start_ind11,
                  end_ind11, type11, seasonal_coefs1, MLSS_spacetime)
  expect_equal(round(calib11$theta, 2), 0.95) # correct calibration
  expect_equal(round(calib12$theta, 2), 1.00) # correct calibration
  expect_equal(round(calib13$theta, 2), 1.03) # correct calibration
  expect_lt(calib11$error, 0.01) # in-sample pricing error less than 0.01
  expect_lt(calib12$error, 0.01) # in-sample pricing error less than 0.01
  expect_lt(calib13$error, 0.01) # in-sample pricing error less than 0.01

  # 2nd test case
  residuals2 = matrix(as.numeric(residuals[, 51:55]), 730, 5)
  seasonal_coefs2 = as.numeric(seasonal_coefs[51, 2:5])
  station21 = 3
  start_ind21 = 1
  end_ind21 = 31
  type21 = "HDD"
  market_price2 = 650
  calib21 = calib(market_price2, residuals2, station21, start_ind21,
                  end_ind21, type21, seasonal_coefs2, Fourier)
  calib22 = calib(market_price2, residuals2, station21, start_ind21,
                  end_ind21, type21, seasonal_coefs2, adaptBW)
  calib23 = calib(market_price2, residuals2, station21, start_ind21,
                  end_ind21, type21, seasonal_coefs2, MLSS_spacetime)
  expect_equal(round(calib21$theta, 2), 0.37) # correct calibration
  expect_equal(round(calib22$theta, 2), 0.38) # correct calibration
  expect_equal(round(calib23$theta, 2), 0.37) # correct calibration
  expect_lt(calib21$error, 0.01) # in-sample pricing error less than 0.01
  expect_lt(calib22$error, 0.01) # in-sample pricing error less than 0.01
  expect_lt(calib23$error, 0.01) # in-sample pricing error less than 0.01
})
