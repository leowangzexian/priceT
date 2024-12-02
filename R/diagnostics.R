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
  teststat = kstest$statistic # test statistic from Kolmogorov-Smirnov test
  pvalue = kstest$p.value # p-value from Kolmogorov-Smirnov test
  qqnorm(resid)
  qqline(resid, col = "blue") # QQ plot against Gaussian
  saved_plot1 = recordPlot() # save the plot

  # QQ plot against generalised hyperbolic distribution
  fittedGH = ghyp::fit.ghypuv(resid, silent = TRUE) # fitting the data to the generalised hyperbolic distribution first
  ghyp::qqghyp(fittedGH, spline.points = 100) # QQ plot against asymmetric generalised hyperbolic distribution
  saved_plot2 = recordPlot() # save the plot

  # Kernel density estimates
  plot(density(resid, kernel = "epanechnikov"), main = "Kernel density estimate", xlab = "deseasonalized temperatures",
       col = "blue") # plotting the kernel density estimate with a blue curve
  curve(dnorm(x, mean = mean(resid), sd = sd(resid)),
        add = TRUE, col = "red", lwd = 2, lty = 3) # plotting the Gaussian distribution with red curve on the same graph to enable comparison
  legend("topright", legend = c("kernel density", "Gaussian"),
         col = c("blue", "red"), lty = c(1, 3), lwd = 2, cex = 0.8) # add legend to distinguish between the two lines
  saved_plot3 = recordPlot() # save the plot

  # returns the test statistic and p-value from the Kolmogorov-Smirnov normality test,
  # QQ plot against normal distribution, QQ plot against generalised hyperbolic distribution
  # and the kernel density estimate of the deseasonalized temperatures at one particular site
  # teststat = test statistic from the Kolmogorov-Smirnov normality test
  # pvalue = p-value from the Kolmogorov-Smirnov normality test (smaller p-value, especially close to 0, indicates rejection of the normality assumption)
  # plt1 = QQ plot against normal distribution
  # plt2 = QQ plot against generalised hyperbolic distribution
  # plt3 = kernel density estimate (compared against Gaussian)
  return(list(teststat = teststat, pvalue = pvalue, plt1 = saved_plot1, plt2 = saved_plot2, plt3 = saved_plot3))
}
