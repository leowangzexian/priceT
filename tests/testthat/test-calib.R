library(testthat)

# devtools::load_all()

test_that("the function calib handles errors appropriately", {
  # load the residuals data from residuals.rda in the data folder
  # load the seasonal coefficients data from seasonal_coefs.rda in the data folder
  # specify correct inputs
  residuals1 = matrix(rnorm(730 * 3), 730, 3)
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
