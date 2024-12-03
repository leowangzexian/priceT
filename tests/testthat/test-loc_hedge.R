library(testthat)

# devtools::load_all()

test_that("the function loc_hedge handles errors appropriately", {
  # load the residuals data from residuals.rda in the data folder
  # specify correct inputs
  residuals1 = matrix(as.numeric(residuals[, 3:5]), 730, 3)
  station1 = 1

  # test for residuals
  expect_error(loc_hedge(c(1, 2, 3), station1))
  expect_error(loc_hedge(matrix(c(2, 2, 2, 2), 2, 2), station1))

  # test for station
  expect_error(loc_hedge(residuals1, p + 1))
})

test_that("the function loc_hedge outputs the correct optimized weights", {
  # load the residuals data from residuals.rda in the data folder
  # 1st test case
  residuals1 = matrix(as.numeric(residuals[, 3:5]), 730, 3)
  station1 = 1
  loc_hedge1 = loc_hedge(residuals1, station1)

  expect_equal(length(loc_hedge1$optim_weights), 3 - 1)
  expect_equal(sum(loc_hedge1$optim_weights), 1) # optimal weights sum to one

  # 2nd test case
  residuals2 = matrix(as.numeric(residuals[, 51:55]), 730, 5)
  station2 = 3
  loc_hedge2 = loc_hedge(residuals2, station2)

  expect_equal(length(loc_hedge2$optim_weights), 5 - 1)
  expect_equal(sum(loc_hedge2$optim_weights), 1) # optimal weights sum to one
})
