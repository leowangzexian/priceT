#' Pricing with adaptive bandwidth selection
#'
#' @description
#' Function that returns the price of a temperature futures using the adaptive bandwidth selection
#' method to estimate seasonal volatility
#'
#' @param residuals A n by p matrix containing the past deseasonalized temperatures data at p stations over (n / 365) years (n >= 365 * 2) (p >= 3)
#' @param station An numeric index denoting the specific station that the price of the temperature futures depends on
#' @param start_ind An numeric index for the start of the measurement period
#' @param end_ind An numeric index for the end of the measurement period
#' @param type A string denoting the type of the temperature futures, either CDD, HDD or CAT
#'
#' @return A list containing:
#'         \item{price}{}
#'         \item{sresids}{}
#'         \item{plt}{}
#'
#' @export
#'
#' @examples
#' # load the residuals data from residuals.rda in the data folder
#' residuals = matrix(as.numeric(residuals[, 3:5]), 730, 3)
adaptBW = function(residuals, station, start_ind, end_ind, type) {
  # compatibility checks
  if (is.matrix(resid) == FALSE) {
    stop("resid should be a vector.") # returns error message if the input resid is not a matrix
  }
  n = dim(residuals)[1]
  p = dim(residuals)[2]
  if (n < 365 * 2) {
    stop("Need at least two years of data for resid.") # returns error message if the data inputted is less than 2 years
  }
  if (p < 3) {
    sto("Need at least three stations.") # returns error message if the number of stations < 3
  }

  years = n / 365 # number of years included
  residt = do.call(ts.union, lapply(1:p, function(i) as.ts(residuals[, i]))) # format resid as time series
  residtar = ar(residt, aic = FALSE, order.max = 3) # fit to VAR model
  resids = as.matrix(residtar$resid) # residuals from fitted VAR model
  resids = matrix(as.numeric(resids), n, p)
  resids[1, ] = resids[1 + 365, ] # fill in the NA values
  resids[2, ] = resids[2 + 365, ] # fill in the NA values
  resids[3, ] = resids[3 + 365, ] # fill in the NA values

  for (i in 1:p) {
    # implementing the adaptive bandwidth selection method for estimating seasonal volatility

  }

  # compute the covariance matrix of the standardized residuals

  clevel = 65
  # pricing
  if (type == "CDD") { # Cooling Degree Days
    # pricing method for CDD

  } else if (type == "HDD") { # Heating Degree Days
    # pricing method for HDD

  } else if (type == "CAT") { # Cumulative Average Temperature
    # pricing method for CAT

  }

  # returns the computed futures price, standardized residuals and the graph plotted
  return(list(price = price, sresids = resids, plt = saved_plot))
}
