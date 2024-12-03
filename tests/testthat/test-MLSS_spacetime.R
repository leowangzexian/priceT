library(testthat)

# devtools::load_all()

test_that("the function MLSS_spacetime handles errors appropriately", {
  # load the residuals data from residuals.rda in the data folder
  # load the seasonal coefficients data from seasonal_coefs.rda in the data folder
  # specify correct inputs
  residuals1 = matrix(rnorm(730 * 3), 730, 3)
  seasonal_coefs1 = as.numeric(seasonal_coefs[1, 2:5])
  station11 = 1
  start_ind11 = 1
  end_ind11 = 31
  type11 = "HDD"

  # test for residuals
  expect_error(MLSS_spacetime(c(1, 2, 3), station11, start_ind11, end_ind11, type11, seasonal_coefs1))
  expect_error(MLSS_spacetime(matrix(c(2, 2, 2, 2), 2, 2), station11, start_ind11, end_ind11, type11, seasonal_coefs1))

  # test for seasonal_coefs
  expect_error(MLSS_spacetime(residuals1, station11, start_ind11, end_ind11, type11, c(1, 2, 3)))

  # test for station
  expect_error(MLSS_spacetime(residuals1, p + 1, start_ind11, end_ind11, type11, seasonal_coefs1))

  # test for start_ind
  expect_error(MLSS_spacetime(residuals1, station11, "Jan", end_ind11, type11, seasonal_coefs1))

  # test for end_ind
  expect_error(MLSS_spacetime(residuals1, station11, start_ind11, 400, type11, seasonal_coefs1))

  # test for type
  expect_error(MLSS_spacetime(residuals1, station11, start_ind11, end_ind11, "Call", seasonal_coefs1))
})

test_that("the function MLSS_spacetime outputs the correct price", {
  # load the residuals data from residuals.rda in the data folder
  # load the seasonal coefficients data from seasonal_coefs.rda in the data folder
  # 1st test case
  residuals1 = matrix(rnorm(730 * 3), 730, 3)
  seasonal_coefs1 = as.numeric(seasonal_coefs[1, 2:5])
  station11 = 1
  start_ind11 = 1
  end_ind11 = 31
  type11 = "HDD"
  MLSS_spacetime11 = MLSS_spacetime(residuals1, station11, start_ind11,
                                    end_ind11, type11, seasonal_coefs1)
  expect_equal(length(round(MLSS_spacetime11$price, 0)), 1)

  # 2nd test case
  station12 = 1
  start_ind12 = 182
  end_ind12 = 212
  type12 = "CDD"
  MLSS_spacetime12 = MLSS_spacetime(residuals1, station12, start_ind12,
                                    end_ind12, type12, seasonal_coefs1)
  expect_equal(length(round(MLSS_spacetime12$price, 0)), 1)

  # 3rd test case
  residuals2 = matrix(rnorm(730 * 5), 730, 5)
  seasonal_coefs2 = as.numeric(seasonal_coefs[51, 2:5])
  station21 = 3
  start_ind21 = 1
  end_ind21 = 31
  type21 = "HDD"
  MLSS_spacetime21 = MLSS_spacetime(residuals2, station21, start_ind21,
                                    end_ind21, type21, seasonal_coefs2)
  expect_equal(length(round(MLSS_spacetime21$price, 0)), 1)

  # 4th test case
  station22 = 3
  start_ind22 = 182
  end_ind22 = 212
  type22 = "CAT"
  MLSS_spacetime22 = MLSS_spacetime(residuals2, station22, start_ind22,
                                    end_ind22, type22, seasonal_coefs2)
  expect_equal(length(round(MLSS_spacetime22$price, 0)), 1)
})

test_that("the function MLSS_spacetime outputs values of the parameter mu in the correct format", {
  # load the residuals data from residuals.rda in the data folder
  # load the seasonal coefficients data from seasonal_coefs.rda in the data folder
  # 1st test case
  residuals1 = matrix(rnorm(730 * 3), 730, 3)
  seasonal_coefs1 = as.numeric(seasonal_coefs[1, 2:5])
  station11 = 1
  start_ind11 = 1
  end_ind11 = 31
  type11 = "HDD"
  MLSS_spacetime11 = MLSS_spacetime(residuals1, station11, start_ind11,
                                    end_ind11, type11, seasonal_coefs1)
  expect_equal(length(MLSS_spacetime11$mu), 3)

  # 2nd test case
  station12 = 1
  start_ind12 = 182
  end_ind12 = 212
  type12 = "CDD"
  MLSS_spacetime12 = MLSS_spacetime(residuals1, station12, start_ind12,
                                    end_ind12, type12, seasonal_coefs1)
  expect_equal(length(MLSS_spacetime12$mu), 3)

  # 3rd test case
  residuals2 = matrix(rnorm(730 * 5), 730, 5)
  seasonal_coefs2 = as.numeric(seasonal_coefs[51, 2:5])
  station21 = 3
  start_ind21 = 1
  end_ind21 = 31
  type21 = "HDD"
  MLSS_spacetime21 = MLSS_spacetime(residuals2, station21, start_ind21,
                                    end_ind21, type21, seasonal_coefs2)
  expect_equal(length(MLSS_spacetime21$mu), 5)

  # 4th test case
  station22 = 3
  start_ind22 = 182
  end_ind22 = 212
  type22 = "CAT"
  MLSS_spacetime22 = MLSS_spacetime(residuals2, station22, start_ind22,
                                    end_ind22, type22, seasonal_coefs2)
  expect_equal(length(MLSS_spacetime22$mu), 5)
})
