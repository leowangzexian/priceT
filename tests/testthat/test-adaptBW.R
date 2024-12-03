library(testthat)

# devtools::load_all()

test_that("the function adaptBW handles errors appropriately", {
  # load the residuals data from residuals.rda in the data folder
  # load the seasonal coefficients data from seasonal_coefs.rda in the data folder
  # specify correct inputs
  residuals1 = matrix(as.numeric(residuals[, 3:5]), 730, 3)
  seasonal_coefs1 = as.numeric(seasonal_coefs[1, 2:5])
  station11 = 1
  start_ind11 = 1
  end_ind11 = 31
  type11 = "HDD"

  # test for residuals
  expect_error(adaptBW(c(1, 2, 3), station11, start_ind11, end_ind11, type11, seasonal_coefs1))
  expect_error(adaptBW(matrix(c(2, 2, 2, 2), 2, 2), station11, start_ind11, end_ind11, type11, seasonal_coefs1))

  # test for seasonal_coefs
  expect_error(adaptBW(residuals1, station11, start_ind11, end_ind11, type11, c(1, 2, 3)))

  # test for station
  expect_error(adaptBW(residuals1, p + 1, start_ind11, end_ind11, type11, seasonal_coefs1))

  # test for start_ind
  expect_error(adaptBW(residuals1, station11, "Jan", end_ind11, type11, seasonal_coefs1))

  # test for end_ind
  expect_error(adaptBW(residuals1, station11, start_ind11, 400, type11, seasonal_coefs1))

  # test for type
  expect_error(adaptBW(residuals1, station11, start_ind11, end_ind11, "Call", seasonal_coefs1))
})

test_that("the function adaptBW outputs the correct price", {
  # load the residuals data from residuals.rda in the data folder
  # load the seasonal coefficients data from seasonal_coefs.rda in the data folder
  # 1st test case
  residuals1 = matrix(as.numeric(residuals[, 3:5]), 730, 3)
  seasonal_coefs1 = as.numeric(seasonal_coefs[1, 2:5])
  station11 = 1
  start_ind11 = 1
  end_ind11 = 31
  type11 = "HDD"
  adaptBW11 = adaptBW(residuals1, station11, start_ind11, end_ind11, type11, seasonal_coefs1)
  expect_equal(round(adaptBW11$price, 0), 867)

  # 2nd test case
  station12 = 1
  start_ind12 = 182
  end_ind12 = 212
  type12 = "CDD"
  adaptBW12 = adaptBW(residuals1, station12, start_ind12, end_ind12, type12, seasonal_coefs1)
  expect_equal(round(adaptBW12$price, 0), 600)

  # 3rd test case
  residuals2 = matrix(as.numeric(residuals[, 51:55]), 730, 5)
  seasonal_coefs2 = as.numeric(seasonal_coefs[51, 2:5])
  station21 = 3
  start_ind21 = 1
  end_ind21 = 31
  type21 = "HDD"
  adaptBW21 = adaptBW(residuals2, station21, start_ind21, end_ind21, type21, seasonal_coefs2)
  expect_equal(round(adaptBW21$price, 0), 1727)

  # 4th test case
  station22 = 3
  start_ind22 = 182
  end_ind22 = 212
  type22 = "CAT"
  adaptBW22 = adaptBW(residuals2, station22, start_ind22, end_ind22, type22, seasonal_coefs2)
  expect_equal(round(adaptBW22$price, 0), 1994)
})

test_that("the function adaptBW outputs values of the seasonal variance function in the correct format", {
  # load the residuals data from residuals.rda in the data folder
  # load the seasonal coefficients data from seasonal_coefs.rda in the data folder
  # 1st test case
  residuals1 = matrix(as.numeric(residuals[, 3:5]), 730, 3)
  seasonal_coefs1 = as.numeric(seasonal_coefs[1, 2:5])
  station11 = 1
  start_ind11 = 1
  end_ind11 = 31
  type11 = "HDD"
  adaptBW11 = adaptBW(residuals1, station11, start_ind11, end_ind11, type11, seasonal_coefs1)
  expect_equal(dim(adaptBW11$sresids)[1], 730)
  expect_equal(dim(adaptBW11$sresids)[2], 3)

  # 2nd test case
  station12 = 1
  start_ind12 = 182
  end_ind12 = 212
  type12 = "CDD"
  adaptBW12 = adaptBW(residuals1, station12, start_ind12, end_ind12, type12, seasonal_coefs1)
  expect_equal(dim(adaptBW12$sresids)[1], 730)
  expect_equal(dim(adaptBW12$sresids)[2], 3)

  # 3rd test case
  residuals2 = matrix(as.numeric(residuals[, 51:55]), 730, 5)
  seasonal_coefs2 = as.numeric(seasonal_coefs[51, 2:5])
  station21 = 3
  start_ind21 = 1
  end_ind21 = 31
  type21 = "HDD"
  adaptBW21 = adaptBW(residuals2, station21, start_ind21, end_ind21, type21, seasonal_coefs2)
  expect_equal(dim(adaptBW21$sresids)[1], 730)
  expect_equal(dim(adaptBW21$sresids)[2], 5)

  # 4th test case
  station22 = 3
  start_ind22 = 182
  end_ind22 = 212
  type22 = "CAT"
  adaptBW22 = adaptBW(residuals2, station22, start_ind22, end_ind22, type22, seasonal_coefs2)
  expect_equal(dim(adaptBW22$sresids)[1], 730)
  expect_equal(dim(adaptBW22$sresids)[2], 5)
})
