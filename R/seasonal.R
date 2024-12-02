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
#'         \item{plt}{plot of the temperatures and the seasonal function over the time indices}
#'
#' @export
#'
#' @examples
#' # load the temp0 data from temp0.rda in the data folder
#'
#' # example 1
#' temp = as.numeric(temp0[, 3]) # historical temperatures at one station
#' seasonal1 = seasonal(temp)
#'
#' # examine results
#' c(seasonal1$a, seasonal1$b, seasonal1$c, seasonal1$d) # coefficients
#' head(seasonal1$seasonality) # fitted values of the seasonal function
#' seasonal1$plt # plot
#'
#' # example 2
#' temp = as.numeric(temp0[, 50]) # historical temperatures at one station
#' seasonal2 = seasonal(temp)
#'
#' # examine results
#' c(seasonal2$a, seasonal2$b, seasonal2$c, seasonal2$d) # coefficients
#' head(seasonal2$seasonality) # fitted values of the seasonal function
#' seasonal2$plt # plot
seasonal = function(temp) {
  # compatibility check
  if (is.vector(temp) == FALSE) {
    stop("temp should be a vector.") # returns error message if the input temp is not a vector
  }

  n = length(temp)
  t = c(1:n) # times indices
  initial = c(50, 0, 35, 0) # initialise parameter values for optimisation

  # perform minimisation
  loc1optim = optim(par = initial, fn = loc1temp, t = t, loc1temperatures = temp, method = "L-BFGS-B")

  # estimated parameters from minimisation
  loc1a = loc1optim$par[1]
  loc1b = loc1optim$par[2]
  loc1c = loc1optim$par[3]
  loc1d = loc1optim$par[4]

  # computes the fitted seasonality function
  loc1fitted = loc1a + loc1b * t + loc1c * cos(2 * pi * (t - loc1d) / 365)

  # plotting the temperatures and seasonal function
  plot(t, temp, type = "l", col = "blue", xlab = "Time", ylab = "Temperature", lwd = 2)
  lines(t, loc1fitted, col = "gold", lwd = 2.5)
  saved_plot = recordPlot() # save the plot

  # returns the estimated coefficients of the seasonality function,
  # the fitted seasonality function and the graph plotted
  # a = a scalar that is the value of the coefficient a in the seasonal function
  # b = a scalar that is the value of the coefficient b in the seasonal function
  # c = a scalar that is the value of the coefficient c in the seasonal function
  # d = a scalar that is the value of the coefficient d in the seasonal function
  # seasonality = n by 1 vector containing the values of the seasonal function
  # plt = plot of the temperatures and the seasonal function over the time indices
  return(list(a = loc1a, b = loc1b, c = loc1c, d = loc1d, seasonality = loc1fitted, plt = saved_plot))
}

# Function that computes and returns the objective function for fitting the seasonality function
loc1temp = function(params, t, loc1temperatures) {
  # computes the seasonality function
  S = params[1] + params[2] * t + params[3] * cos(2 * pi * (t - params[4]) / 365)

  # computes the squared difference between the seasonality and the observed values of the temperature
  S2 = sum(S^2 - 2 * S * loc1temperatures + loc1temperatures^2)

  return(S2) # returns the objective function
}
