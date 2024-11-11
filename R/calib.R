#' Calibrating risk-neutral parameters
#'
#' @description
#' Function that returns the calibrated risk-neutral parameters and in-sample pricing errors based on the market prices
#'
#' @param theta A q times 1 vector
#' @param price A k times 1 vector containing the computed futures prices
#' @param market_price A k times 1 vector containing the actual futures prices traded in the market
#'
#' @return A list containing:
#'         \item{}{}
#'         \item{}{}
#'         \item{}{}
#'
#' @export
#'
#' @examples
calib = function(theta, price, market_price) {
  # compatibility checks
  if (is.vector(theta) == FALSE) {
    stop("theta should be a vector.") # returns error message if the input theta is not a vector
  }
  if (is.vector(price) == FALSE) {
    stop("price should be a vector.") # returns error message if the input price is not a vector
  }
  if (is.vector(market_price) == FALSE) {
    stop("market_price should be a vector.") # returns error message if the input market_price is not a vector
  }

  # computing price with risk-neutral parameter

  # returns the objective function for minimisation
  return(theta^2 - 2 * theta * market_price - market_price^2)
}
