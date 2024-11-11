#' Descriptive analysis of deseasonalized temperatures data
#'
#' @description
#' Function used for simple preliminary tests on the deseasonalized temperatures data,
#' returns test statistics and p-values from tests such as the Kolmogorov-Smirnov test,
#' and visual outputs such as Q-Q plots and kernel density estimates
#'
#' @param resid A n by 1 vector containing the past deseasonalized temperatures data at one station over (n / 365) years
#'
#' @return A list containing:
#'         \item{}{}
#'         \item{}{}
#'         \item{}{}
#'
#' @export
#'
#' @examples
#' # load the residuals data from residuals.rda in the data folder
#'
#' # example 1
#' resid = as.numeric(residuals[, 3]) # deseasonalized temperaures at one station
diagnostics = function(resid) {
  # compatibility check
  if (is.vector(resid) == FALSE) {
    stop("resid should be a vector.") # returns error message if the input resid is not a vector
  }

  kstest = ks.test(resid, "pnorm") # normality test

  qqnorm(resid)
  qqline(resid, col = "blue") # qq plot against Gaussian
  saved_plot1 = recordPlot() # save the plot

  return(list(kstest = kstest, plt1 = saved_plot1))
}
