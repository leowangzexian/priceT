#' Calibrating risk-neutral parameters
#'
#' @description
#' Function that returns the calibrated risk-neutral parameters and in-sample pricing errors based on the market prices
#'
#' @param market_price A scalar that is the actual futures prices traded in the market
#' @param func A function to compute the futures price, either Fourier, adaptBW or MLSS_spacetime
#'
#' @return A list containing:
#'         \item{theta}{a scalar that is the calibrated risk-neutral parameter}
#'         \item{error}{a scalar that is the in-sample pricing error}
#'
#' @export
#'
#' @examples
calib = function(market_price, residuals, station, start_ind, end_ind, type, seasonal_coefs, func) {
  # compatibility checks
  if (is.numeric(market_price) == FALSE | market_price < 0) {
    stop("market_price should be positive.") # returns error message if the input market_price is not a number or is not positive
  }
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
  # calibration
  optim_result = optim(
    par = 1,
    fn = obj,
    market_price = market_price,
    residuals = residuals,
    station = station,
    start_ind = start_ind,
    end_ind = end_ind,
    type = type,
    seasonal_coefs = seasonal_coefs,
    func = func,
    method = "Brent",
    lower = -20,
    upper = 20
  ) # performs univariate minimization to find the risk-neutral parameter that brings the computed price closest to the actual market price
  theta = optim_result$par # calibrated parameter
  price_t = theta * func(residuals, station, start_ind, end_ind, type, seasonal_coefs)$price # calibrated price based on the risk-neutral parameter theta
  error = 100 * abs(price_t^2 - 2 * price_t * market_price + market_price^2) / market_price # percentage deviation from the true price

  # returns the calibrated risk-neutral parameter and the in-sample pricing error
  # theta = a scalar that is the calibrated risk-neutral parameter (can be negative or positive)
  # error = a scalar that is the in-sample pricing error in percentages
  return(list(theta = theta, error = error))
}

# the objective function for minimisation
obj = function(theta, market_price, residuals, station, start_ind, end_ind, type, seasonal_coefs, func) {
  price_t = theta * func(residuals, station, start_ind, end_ind, type, seasonal_coefs)$price
  return(price_t^2 - 2 * price_t * market_price + market_price^2) # squared difference
}
