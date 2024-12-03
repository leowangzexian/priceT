#' Calibrating risk-neutral parameters
#'
#' @description
#' Function that returns the calibrated risk-neutral parameters and in-sample pricing errors based on the market prices
#'
#' @param market_price A scalar that is the actual futures prices traded in the market
#' @param residuals A n by p matrix containing the past deseasonalized temperatures data at p stations over (n / 365) years (n >= 365 * 2) (p >= 3)
#' @param station A numeric index denoting the specific station that the price of the temperature futures depends on
#' @param start_ind A numeric index for the start of the measurement period
#' @param end_ind A numeric index for the end of the measurement period
#' @param type A string denoting the type of the temperature futures, either CDD, HDD or CAT
#' @param seasonal_coefs A 1 by 4 vector containing the coefficients of the seasonality function at the station to be priced
#' @param func A function to compute the futures price, either Fourier, adaptBW or MLSS_spacetime
#'
#' @return A list containing:
#'         \item{theta}{a scalar that is the calibrated risk-neutral parameter}
#'         \item{error}{a scalar that is the in-sample pricing error}
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
#' start_ind11 = 182
#' end_ind11 = 212
#' type11 = "CDD"
#' market_price1 = 2550
#' calib11 = calib_c(market_price1, residuals1, station11, start_ind11, end_ind11, type11, seasonal_coefs1, Fourier_c)
#'
#' # examine results
#' calib11$theta # calibrated risk-neutral parameter when using Fourier
#' calib11$error # in-sample pricing error when using Fourier
#'
#' # example 2
#' residuals2 = matrix(as.numeric(residuals[, 51:55]), 730, 5)
#' seasonal_coefs2 = as.numeric(seasonal_coefs[51, 2:5])
#' station21 = 3
#' start_ind21 = 1
#' end_ind21 = 31
#' type21 = "HDD"
#' market_price2 = 260
#' calib21 = calib_c(market_price2, residuals2, station21, start_ind21, end_ind21, type21, seasonal_coefs2, Fourier_c)
#'
#' # examine results
#' calib21$theta # calibrated risk-neutral parameter when using Fourier
#' calib21$error # in-sample pricing error when using Fourier
calib_c = function(market_price, residuals, station, start_ind, end_ind, type, seasonal_coefs, func) {
  result = calib_cpp(market_price, residuals, station, start_ind, end_ind, type, seasonal_coefs, func)
  return(result)
}
