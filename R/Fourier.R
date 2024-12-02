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
#' @param seasonal_coefs A 1 by 4 vector contaning the coefficients of the seasonality function at the station to be priced
#'
#' @return A list containing:
#'         \item{price}{a scalar that is the price of the derivative computed}
#'         \item{sresids}{n by p matrix containing the standardized residuals computed}
#'         \item{plt}{plot of the empirical and fitted seasonal variances at the station priced}
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
#' Fourier11 = Fourier(residuals1, station11, start_ind11, end_ind11, type11, seasonal_coefs1)
#' station12 = 1
#' start_ind12 = 182
#' end_ind12 = 212
#' type12 = "CDD"
#' Fourier12 = Fourier(residuals1, station12, start_ind12, end_ind12, type12, seasonal_coefs1)
#'
#' # examine results
#' Fourier11$price # HDD price
#' Fourier12$price # CDD price
#' head(Fourier11$sresids) # standardised residuals
#' head(Fourier12$sresids) # standardised residuals
#' Fourier11$plt # plot
#' Fourier12$plt # plot
#'
#' # example 2
#' residuals2 = matrix(as.numeric(residuals[, 53:57]), 730, 5)
#' seasonal_coefs2 = as.numeric(seasonal_coefs[51, 2:5])
#' station21 = 1
#' start_ind21 = 1
#' end_ind21 = 31
#' type21 = "HDD"
#' Fourier21 = Fourier(residuals2, station21, start_ind21, end_ind21, type21, seasonal_coefs2)
#' station22 = 1
#' start_ind22 = 182
#' end_ind22 = 212
#' type22 = "CDD"
#' Fourier22 = Fourier(residuals2, station22, start_ind22, end_ind22, type22, seasonal_coefs2)
#'
#' # examine results
#' Fourier21$price # HDD price
#' Fourier22$price # CDD price
#' head(Fourier21$sresids) # standardised residuals
#' head(Fourier22$sresids) # standardised residuals
#' Fourier21$plt # plot
#' Fourier22$plt # plot
Fourier = function(residuals, station, start_ind, end_ind, type, seasonal_coefs) {
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

  years = n / 365 # number of years included
  residt = do.call(ts.union, lapply(1:p, function(i) as.ts(residuals[, i]))) # format resid as time series
  residtar = ar(residt, aic = FALSE, order.max = 1) # fit to VAR model
  resids = as.matrix(residtar$resid) # residuals from fitted VAR model
  coefs = as.matrix(residtar$ar) # get coefficients of fitted model
  dis = array(coefs, dim = c(p, p))

  # transforming to continuous-time coefficients
  Ac = - diag(p)
  Ac[lower.tri(Ac)] = 1
  A = do.call(rbind, lapply(0:(p - 1), function(i) solve(Ac, dis[p - i, ])))

  resids = matrix(as.numeric(resids), n, p)
  resids[1, ] = resids[1 + 365, ] # fill in the NA values
  resids[2, ] = resids[2 + 365, ] # fill in the NA values
  resids[3, ] = resids[3 + 365, ] # fill in the NA values

  for (i in 1:p) {
    resids_m = matrix(resids[, i]^2, nrow = 365) # rearranging
    svar = rowMeans(resids_m) # means for each day
    initial = c(3.5, 0.5, 0.5, -0.5, -0.2, 0.2, 0.1, -0.1, rep(0, 25)) # initialise parameter values
    lower_bound = rep(-50, 33) # lower bounds for the 33 parameters
    upper_bound = rep(50, 33) # upper bounds for the 33 parameters
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
    cos_matrix = outer(days, 1:length(freqs), FUN = function(i, j) cos(2 * j * pi * i / length(days)))
    sin_matrix = outer(days, 1:length(freqs), FUN = function(i, j) sin(2 * j * pi * i / length(days)))

    # compute the sum2 matrix (for each day, the sum over j of cos + sin terms)
    cos_coeffs = loc1paras[seq(2, 2 * length(freqs), by = 2)]  # Coefficients for cos
    sin_coeffs = loc1paras[seq(3, 2 * length(freqs) + 1, by = 2)]  # Coefficients for sin
    seasonal_var = loc1paras[1] + rowSums(cos_matrix * cos_coeffs + sin_matrix * sin_coeffs)
    seasonal_var = pmax(seasonal_var, 0.001)

    # plotting the seasonal variance and fitted seasonal variance function
    plot(days, svar, type = "l", col = "blue", xlab = "Time", ylab = "Seasonal Variance", lwd = 2, main = "Seasonal Variance")
    lines(seasonal_var, col = "magenta", lwd = 2.5)
    if (i == station) {
      saved_plot = recordPlot() # save the plot for one station
    }

    resids[, i] = resids[, i] / rep(seasonal_var, times = years) # compute standardized residuals
  }

  # initialising parameters for derivatives pricing
  t = 0
  t1 = 24455
  damp = 0.01
  loc1a = seasonal_coefs[1]
  loc1b = seasonal_coefs[2]
  loc1c = seasonal_coefs[3]
  loc1d = seasonal_coefs[4]

  clevel = 65
  # pricing
  if (type == "CDD") { # Cooling Degree Days
    # pricing method for CDD
    i_seq = (t1 + start_ind):(t1 + end_ind)
    i_seq2 = start_ind:end_ind
    seas = loc1a + loc1b * i_seq + loc1c * cos(2 * pi * (i_seq - loc1d) / 365) # 1st term in pricing formula
    re = sapply(i_seq2, function(i) expm::expm(A) %*% pmin(sigma(resids, i), 1)) # 2nd term in pricing formula
    re1 = expm::expm(A) %*% residuals[n, ] * damp # 3rd term in pricing formula
    acc = sum(pmax(seas + apply(re, 2, function(x) x[station]) + re1[station] - clevel, 0)) # payoff
  } else if (type == "HDD") { # Heating Degree Days
    # pricing method for HDD
    i_seq = (t1 + start_ind):(t1 + end_ind)
    i_seq2 = start_ind:end_ind
    seas = loc1a + loc1b * i_seq + loc1c * cos(2 * pi * (i_seq - loc1d) / 365) # 1st term in pricing formula
    re = sapply(i_seq2, function(i) expm::expm(A) %*% pmin(sigma(resids, i), 1)) # 2nd term in pricing formula
    re1 = expm::expm(A) %*% residuals[n, ] * damp # 3rd term in pricing formula
    acc = sum(pmax(clevel - seas - apply(re, 2, function(x) x[station]) - re1[station], 0)) # payoff
  } else if (type == "CAT") { # Cumulative Average Temperature
    # pricing method for CAT
    i_seq = (t1 + start_ind):(t1 + end_ind)
    i_seq2 = start_ind:end_ind
    seas = loc1a + loc1b * i_seq + loc1c * cos(2 * pi * (i_seq - loc1d) / 365) # 1st term in pricing formula
    re = sapply(i_seq2, function(i) expm::expm(A) %*% pmin(sigma(resids, i), 1)) # 2nd term in pricing formula
    re1 = expm::expm(A) %*% residuals[n, ] * damp # 3rd term in pricing formula
    acc = sum(seas + apply(re, 2, function(x) x[station]) + re1[station]) # payoff
  }

  # returns the computed futures price, standardized residuals and the graph plotted
  # price = a scalar that is the price of the derivative computed
  # sresids = n by p matrix containing the standardized residuals computed
  # plt = plot of the empirical and fitted seasonal variances at the station priced
  return(list(price = acc, sresids = resids, plt = saved_plot))
}

# Function that computes and returns the objective function for fitting the seasonal variance function based on the truncated Fourier series
loc1seasonal = function(params, loc1seasonal_var) {
  n_days = 365
  n_harmonics = 16

  # Create a matrix of days (1 to 365)
  time_indices = 1:n_days

  # Create cosine and sine matrices for all harmonics and all days
  cos_matrix = outer(time_indices, 1:n_harmonics, FUN = function(i, j) cos(2 * j * pi * i / n_days))
  sin_matrix = outer(time_indices, 1:n_harmonics, FUN = function(i, j) sin(2 * j * pi * i / n_days))

  # Extract cosine and sine coefficients from the parameters
  cos_coeffs = params[seq(2, 2 * n_harmonics, by = 2)]  # Coefficients for cos
  sin_coeffs = params[seq(3, 2 * n_harmonics + 1, by = 2)]  # Coefficients for sin

  # Compute the sum2 matrix for all days and harmonics
  sum2_matrix = params[1] + rowSums(cos_matrix * cos_coeffs + sin_matrix * sin_coeffs)

  # Compute residuals (difference between predicted and actual seasonal variance)
  residuals = sum2_matrix - loc1seasonal_var

  # Return the sum of squared residuals (least squares method)
  return(sum(residuals^2))
}

# Function that computes and returns the covariance matrix of the standardized residuals for each day
sigma = function(sresids, t) {
  if (t == 1 | t == 2 | t == 3) {
    t = t + 3 # adjusting indices
  }
  sampled = sresids[seq(t, t + 365, by = 365), ] # select samples
  return(cov(sampled)) # output covariance matrix
}
