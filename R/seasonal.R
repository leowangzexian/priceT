# Function that returns the estimated coefficients of the seasonality function
# temp = n by 1 vector containing the past temperatures data at one station over (n / 365) years
seasonal = function(temp) {
  # compatibility check
  if (is.vector(temp) == FALSE) {
    stop("temp should be a vector.") # returns error message if the input temp is not a vector
  }

  n = length(temp)
  t = 1:n # times indices
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
  saved_plot = recordPlot() # saving the plot

  # returns the estimated coefficients of the seasonality function,
  # the fitted seasonality function and the graph plotted
  return(c(a = loc1a, b = loc1b, c = loc1c, d = loc1d, seasonality = loc1fitted, plt = saved_plot))
}

# Function that computes and returns the objective function for fitting the seasonality function
loc1temp = function(params, t, loc1temperatures) {
  # computes the seasonality function
  S = params[1] + params[2] * t + params[3] * cos(2 * pi * (t - params[4]) / 365)

  # computes the squared difference between the seasonality and the observed values of the temperature
  S2 = S^2 - 2 * S * loc1temperatures + loc1temperatures^2

  return(S2) # returns the objective function
}
