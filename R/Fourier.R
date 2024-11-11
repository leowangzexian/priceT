#' Pricing with truncated Fourier series
#'
#' @description
#' Function that returns the price of a temperature futures using the truncated Fourier series
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
Fourier = function(residuals, station, start_ind, end_ind, type) {
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
    resids_m = matrix(resids[, i]^2, nrow = 365, byrow = TRUE) # rearranging
    svar = rowMeans(resids_m) # means for each day
    initial = c(3.5, 0.5, 0.5, -0.5, -0.2, 0.2, 0.1, -0.1, rep(0, 24)) # initialise parameter values
    lower_bound = rep(-50, 32) # lower bounds for the 32 parameters
    upper_bound = rep(50, 32) # upper bounds for the 32 parameters
    bounds = list(lower = lower_bound, upper = upper_bound) # bounds on the objective values
    optim_result = optim(
      par = initial,
      fn = loc1seasonal,
      loc1seasonal_var = svar,
      method = "L-BFGS-B",
      lower = lower_bound,
      upper = upper_bound
    ) # fitting the seasonal variance function through minimisation
    loc1paras = optim_result$par

    # generate matrix of cosine and sine terms for all days and frequencies
    days = 1:365
    freqs = 1:16

    # create cosine and sine matrices for the truncated Fourier series
    cos_terms = matrix(cos(2 * pi * outer(days, freqs, FUN = "*") / 365), nrow = 365, ncol = 16)
    sin_terms = matrix(sin(2 * pi * outer(days, freqs, FUN = "*") / 365), nrow = 365, ncol = 16)

    # compute the sum2 matrix (for each day, the sum over j of cos + sin terms)
    seasonal_var = loc1paras[1] + rowSums(matrix(loc1paras[2 * freqs] * cos_terms + loc1paras[2 * freqs + 1] * sin_terms, nrow = 365))

    # plotting the seasonal variance and fitted seasonal variance function
    plot(days, svar, type = "l", col = "blue", xlab = "Time", ylab = "Seasonal Variance", lwd = 2, main = "Seasonal Variance")
    lines(seasonal_var, col = "magenta", lwd = 2.5)
    if (p == station) {
      saved_plot = recordPlot() # save the plot for one station
    }

    resids[, p] = resids[, p] / rep(seasonal_var, times = years) # compute standardized residuals
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

# Function that computes and returns the objective function for fitting the seasonal variance function based on the truncated Fourier series
loc1seasonal = function(params, loc1seasonal_var) {
  # generate matrix of cosine and sine terms for all days and frequencies
  days = 1:365
  freqs = 1:16

  # create cosine and sine matrices for the truncated Fourier series
  cos_terms = matrix(cos(2 * pi * outer(days, freqs, FUN = "*") / 365), nrow = 365, ncol = 16)
  sin_terms = matrix(sin(2 * pi * outer(days, freqs, FUN = "*") / 365), nrow = 365, ncol = 16)

  # compute the sum2 matrix (for each day, the sum over j of cos + sin terms)
  sum2 = params[1] + rowSums(matrix(params[2 * freqs] * cos_terms + params[2 * freqs + 1] * sin_terms, nrow = 365))

  # computing and returning objective function
  return(sum(sum2^2 - 2 * sum2 * loc1seasonal_var + loc1seasonal_var^2))
}
