library(testthat)

test_that("the function Fourier handles errors appropriately", {
  # specify correct inputs
  residuals1 = matrix(as.numeric(residuals[, 3:5]), 730, 3)
  seasonal_coefs1 = as.numeric(seasonal_coefs[1, 2:5])
  station11 = 1
  start_ind11 = 1
  end_ind11 = 31
  type11 = "HDD"

  # test for residuals
  expect_error(Fourier(c(1, 2, 3), station11, start_ind11, end_ind11, type11, seasonal_coefs1))
  expect_error(Fourier(matrix(c(2, 2, 2, 2), 2, 2), station11, start_ind11, end_ind11, type11, seasonal_coefs1))

  # test for seasonal_coefs
  expect_error(Fourier(residuals1, station11, start_ind11, end_ind11, type11, c(1, 2, 3)))

  # test for station
  expect_error(Fourier(residuals1, p + 1, start_ind11, end_ind11, type11, seasonal_coefs1))

  # test for start_ind
  expect_error(Fourier(residuals1, station11, "Jan", end_ind11, type11, seasonal_coefs1))

  # test for end_ind
  expect_error(Fourier(residuals1, station11, start_ind11, 400, type11, seasonal_coefs1))

  # test for type
  expect_error(Fourier(residuals1, station11, start_ind11, end_ind11, "Call", seasonal_coefs1))
})

test_that("the function Fourier outputs the correct price", {
  # load the temp0 data from temp0.rda in the data folder
  # load the seasonal coefficients data from seasonal_coefs.rda in the data folder
  # 1st test case
  residuals1 = matrix(as.numeric(residuals[, 3:5]), 730, 3)
  seasonal_coefs1 = as.numeric(seasonal_coefs[1, 2:5])
  station11 = 1
  start_ind11 = 1
  end_ind11 = 31
  type11 = "HDD"
  Fourier11 = Fourier(residuals1, station11, start_ind11, end_ind11, type11, seasonal_coefs1)
  expect_equal(round(Fourier11$price, 0), 575)

  # 2nd test case
  station12 = 1
  start_ind12 = 182
  end_ind12 = 212
  type12 = "CDD"
  Fourier12 = Fourier(residuals1, station12, start_ind12, end_ind12, type12, seasonal_coefs1)
  expect_equal(round(Fourier12$price, 0), 631)

  # 3rd test case
  residuals2 = matrix(as.numeric(residuals[, 51:55]), 730, 5)
  seasonal_coefs2 = as.numeric(seasonal_coefs[51, 2:5])
  station21 = 3
  start_ind21 = 1
  end_ind21 = 31
  type21 = "HDD"
  Fourier21 = Fourier(residuals2, station21, start_ind21, end_ind21, type21, seasonal_coefs2)
  expect_equal(round(Fourier21$price, 0), 1753)

  # 4th test case
  station22 = 3
  start_ind22 = 182
  end_ind22 = 212
  type22 = "CAT"
  Fourier22 = Fourier(residuals2, station22, start_ind22, end_ind22, type22, seasonal_coefs2)
  expect_equal(round(Fourier22$price, 0), 1993)
})

test_that("the function Fourier outputs values of the seasonal variance function in the correct format", {
  # load the temp0 data from temp0.rda in the data folder
  # load the seasonal coefficients data from seasonal_coefs.rda in the data folder
  # 1st test case
  residuals1 = matrix(as.numeric(residuals[, 3:5]), 730, 3)
  seasonal_coefs1 = as.numeric(seasonal_coefs[1, 2:5])
  station11 = 1
  start_ind11 = 1
  end_ind11 = 31
  type11 = "HDD"
  Fourier11 = Fourier(residuals1, station11, start_ind11, end_ind11, type11, seasonal_coefs1)
  expect_equal(dim(Fourier11$sresids)[1], 730)
  expect_equal(dim(Fourier11$sresids)[2], 3)

  # 2nd test case
  station12 = 1
  start_ind12 = 182
  end_ind12 = 212
  type12 = "CDD"
  Fourier12 = Fourier(residuals1, station12, start_ind12, end_ind12, type12, seasonal_coefs1)
  expect_equal(dim(Fourier12$sresids)[1], 730)
  expect_equal(dim(Fourier12$sresids)[2], 3)

  # 3rd test case
  residuals2 = matrix(as.numeric(residuals[, 51:55]), 730, 5)
  seasonal_coefs2 = as.numeric(seasonal_coefs[51, 2:5])
  station21 = 3
  start_ind21 = 1
  end_ind21 = 31
  type21 = "HDD"
  Fourier21 = Fourier(residuals2, station21, start_ind21, end_ind21, type21, seasonal_coefs2)
  expect_equal(dim(Fourier21$sresids)[1], 730)
  expect_equal(dim(Fourier21$sresids)[2], 5)

  # 4th test case
  station22 = 3
  start_ind22 = 182
  end_ind22 = 212
  type22 = "CAT"
  Fourier22 = Fourier(residuals2, station22, start_ind22, end_ind22, type22, seasonal_coefs2)
  expect_equal(dim(Fourier22$sresids)[1], 730)
  expect_equal(dim(Fourier22$sresids)[2], 5)
})
