#' Finding the seasonal trend in historical temperatures
#'
#' @description
#' Function that returns the estimated coefficients of the seasonality function
#'
#' @param temp A n by 1 vector containing the past temperatures data at one station over (n / 365) years
#'
#' @return A list containing:
#'         \item{a}{a scalar that is the value of the coefficient a in the seasonal function}
#'         \item{b}{a scalar that is the value of the coefficient b in the seasonal function}
#'         \item{c}{a scalar that is the value of the coefficient c in the seasonal function}
#'         \item{d}{a scalar that is the value of the coefficient d in the seasonal function}
#'         \item{seasonality}{n by 1 vector containing the values of the seasonal function}
#'
#' @export
#'
#' @examples
#' # load the temp0 data from temp0.rda in the data folder
#'
#' # example 1
#' temp = as.numeric(temp0[, 3]) # historical temperatures at one station
#' seasonal1 = seasonal_c(temp)
#'
#' # examine results
#' c(seasonal1$a, seasonal1$b, seasonal1$c, seasonal1$d) # coefficients
#' head(seasonal1$seasonality) # fitted values of the seasonal function
#'
#' # example 2
#' temp = as.numeric(temp0[, 50]) # historical temperatures at one station
#' seasonal2 = seasonal_c(temp)
#'
#' # examine results
#' c(seasonal2$a, seasonal2$b, seasonal2$c, seasonal2$d) # coefficients
#' head(seasonal2$seasonality) # fitted values of the seasonal function
seasonal_c = function(temp) {
  # compatibility check
  if (is.vector(temp) == FALSE) {
    stop("temp should be a vector.") # returns error message if the input temp is not a vector
  }

  result = seasonal_cpp(temp)
  return(result)
}
