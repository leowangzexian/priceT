#' Pricing with truncated Fourier series
#'
#' @description
#' Function that returns the price of a temperature futures using the truncated Fourier series
#' method to estimate seasonal volatility
#'
#' @param residuals A n by p matrix containing the past deseasonalized temperatures data at p stations over (n / 365) years (n >= 365 * 2) (p >= 3)
#' @param station A numeric index denoting the specific station that the price of the temperature futures depends on
#' @param start_ind A numeric index for the start of the measurement period
#' @param end_ind A numeric index for the end of the measurement period
#' @param type A string denoting the type of the temperature futures, either CDD, HDD or CAT
#' @param seasonal_coefs A 1 by 4 vector containing the coefficients of the seasonality function at the station to be priced
#'
#' @return A list containing:
#'         \item{price}{a scalar that is the price of the derivative computed}
#'         \item{sresids}{n by p matrix containing the standardized residuals computed}
#'
#' @export
#'
#' @examples
#' # load the residuals data from residuals.rda in the data folder
#' # load the seasonal coefficients data from seasonal_coefs.rda in the data folder
#'
#' # example 1
#' residuals1 = matrix(as.numeric(residuals[, 3:5]), 730, 3)
#' seasonal_coefs1 = as.numeric(seasonal_coefs[1, 2:5])
#' station11 = 1
#' start_ind11 = 1
#' end_ind11 = 31
#' type11 = "HDD"
#' Fourier11 = Fourier_c(residuals1, station11, start_ind11, end_ind11, type11, seasonal_coefs1)
#' station12 = 1
#' start_ind12 = 182
#' end_ind12 = 212
#' type12 = "CDD"
#' Fourier12 = Fourier_c(residuals1, station12, start_ind12, end_ind12, type12, seasonal_coefs1)
#'
#' # examine results
#' Fourier11$price # HDD price
#' Fourier12$price # CDD price
#' head(Fourier11$sresids) # standardised residuals
#' head(Fourier12$sresids) # standardised residuals
#'
#' # example 2
#' residuals2 = matrix(as.numeric(residuals[, 51:55]), 730, 5)
#' seasonal_coefs2 = as.numeric(seasonal_coefs[51, 2:5])
#' station21 = 3
#' start_ind21 = 1
#' end_ind21 = 31
#' type21 = "HDD"
#' Fourier21 = Fourier_c(residuals2, station21, start_ind21, end_ind21, type21, seasonal_coefs2)
#' station22 = 3
#' start_ind22 = 182
#' end_ind22 = 212
#' type22 = "CAT"
#' Fourier22 = Fourier_c(residuals2, station22, start_ind22, end_ind22, type22, seasonal_coefs2)
#'
#' # examine results
#' Fourier21$price # HDD price
#' Fourier22$price # CAT price
#' head(Fourier21$sresids) # standardised residuals
#' head(Fourier22$sresids) # standardised residuals
Fourier_c = function(residuals, station, start_ind, end_ind, type, seasonal_coefs) {
  # compatibility checks
  if (is.matrix(residuals) == FALSE) {
    stop("residuals should be a matrix.") # returns error message if the input resid is not a matrix
  }
  n = dim(residuals)[1]
  p = dim(residuals)[2]
  if (n < 365 * 2) {
    stop("Need at least two years of data for resid.") # returns error message if the data inputted is less than 2 years
  }
  if (p < 3) {
    stop("Need at least three stations.") # returns error message if the number of stations < 3
  }
  if (is.numeric(station) == FALSE | station <= 0 | station > p) {
    stop("station should be a positive integer.") # returns error message if the index is not within the possible range
  }
  if (is.numeric(start_ind) == FALSE | start_ind <= 0 | start_ind > 365) {
    stop("start_ind should be an integer between 1 and 365.") # returns error message if the starting date is not between 1 and 365
  }
  if (is.numeric(end_ind) == FALSE | end_ind <= 0 | end_ind > 365) {
    stop("end_ind should be an integer between 1 and 365.") # returns error message if the ending date is not between 1 and 365
  }
  if (start_ind >= end_ind) {
    stop("start_ind should be smaller than end_ind") # returns error message if the starting date is not smaller than the ending date
  }
  if (!(length(seasonal_coefs) == 4)) {
    stop("seasonal_coefs should be an array of length 4.") # returns error message if seasonal_coefs is not an array with length 4
  }
  if (!(type == "CDD" | type == "HDD" | type == "CAT")) {
    stop("type should be either CDD, HDD or CAT.") # returns error message if the type of the derivative is not one of the three considered
  }

  result = Fourier_cpp(residuals, station, start_ind, end_ind, type, seasonal_coefs)
  return(result)
}
