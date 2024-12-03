library(testthat)

# devtools::load_all()

test_that("the function diagnostics handles errors appropriately", {
  # test with input being a number
  expect_error(diagnostics("temp"))

  # test with input being a matrix
  expect_error(diagnostics(matrix(c(2, 2, 2, 2), 2, 2)))
})

test_that("the function diagnostics outputs appropriate test statistics and p-values", {
  # load the residuals data from residuals.rda in the data folder
  # 1st test case
  resid = as.numeric(ghyp::rghyp(730)) # deseasonalized temperaures at one station
  diag1 = diagnostics(resid)
  expect_gt(diag1$teststat, 0.05) # test statistic less than 0.3
  expect_lt(diag1$pvalue, 0.04) # p-value is 0

  # 2nd test case
  resid = as.numeric(ghyp::rgig(730)) # deseasonalized temperaures at one station
  diag2 = diagnostics(resid)
  expect_gt(diag2$teststat, 0.5) # test statistic less than 0.3
  expect_lt(diag2$pvalue, 0.01) # p-value is 0
})
