#' Forecasting future temperatures
#'
#' @description
#' Function that returns the point estimates and confidence intervals of the forecasted daily temperatures in the next year
#' after fitting the historical data to the spatio-temporal models
#'
#' @param residuals A n by p matrix containing the past deseasonalized temperatures data at p stations over (n / 365) years (n >= 365 * 2) (p >= 3)
#' @param station A numeric index denoting the specific station where forecasts are based on
#' @param seasonal_coefs A 1 by 4 vector containing the coefficients of the seasonality function at the station considered
#' @param temp A m by 1 vector containing the past temperatures data at one station over (n / 365) years
#'
#' @return A list containing:
#'         \item{point}{a 1 by 365 vector containing the forecasted temperatures for the next year}
#'         \item{lower}{a 1 by 365 vector containing the left endpoints of the confidence intervals for the forecasted temperatures}
#'         \item{upper}{a 1 by 365 vector containing the right endpoints of the confidence intervals for the forecasted temperatures}
#'         \item{newtemps}{a 1 by (m + 365) vector containing the past and future temperatures at that particular station}
#'
#' @export
#'
#' @examples
#' # load the residuals data from residuals.rda in the data folder
#' # load the seasonal coefficients data from seasonal_coefs.rda in the data folder
#' # load the temp0 data from temp0.rda in the data folder
#'
#' # example 1
#' residuals1 = matrix(as.numeric(residuals[, 3:5]), 730, 3)
#' seasonal_coefs1 = as.numeric(seasonal_coefs[1, 2:5])
#' temp = as.numeric(temp0[, 3]) # historical temperatures at one station
#' station11 = 1
#' temp_f1 = temp_forecast(residuals1, station11, seasonal_coefs1, temp)
#'
#' # examine results
#' temp_f1$point # point estimates
#' temp_f1$lower # left endpoints of confidence intervals
#' temp_f1$upper # right endpoints of confidence intervals
#' temp_f1$newtemps # past plus futures forecasted temperatures
#' length(temp_f1$newtemps) # 365 * 6 = 2190
#'
#' # example 2
#' residuals2 = matrix(as.numeric(residuals[, 51:55]), 730, 5)
#' seasonal_coefs2 = as.numeric(seasonal_coefs[51, 2:5])
#' temp = as.numeric(temp0[, 53]) # historical temperatures at one station
#' station21 = 3
#' temp_f2 = temp_forecast(residuals2, station21, seasonal_coefs2, temp)
#'
#' # examine results
#' temp_f2$point # point estimates
#' temp_f2$lower # left endpoints of confidence intervals
#' temp_f2$upper # right endpoints of confidence intervals
#' temp_f2$newtemps # past plus futures forecasted temperatures
#' length(temp_f2$newtemps) # 365 * 6 = 2190
temp_forecast = function(residuals, station, seasonal_coefs, temp) {
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
  if (!(length(seasonal_coefs) == 4)) {
    stop("seasonal_coefs should be an array of length 4.") # returns error message if seasonal_coefs is not an array with length 4
  }
  if (is.vector(temp) == FALSE) {
    stop("temp should be a vector.") # returns error message if the input temp is not a vector
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
      fn = loc1seasonal_s,
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

    resids[, i] = resids[, i] / rep(seasonal_var, times = years) # compute standardized residuals
  }

  # Non-Gaussian spatio-temporal modelling
  fittedNIG = ghyp::fit.NIGmv(resids, silent = TRUE) # using normal inverse Gaussian distribution
  sigma1 = fittedNIG@sigma / 10^4 # covariance of the multivariate distribution
  mu = fittedNIG@mu

  # initialising parameters for temperature forecasting
  t = 0
  t1 = 24455
  damp = 0.01
  sig = 0.9999
  loc1a = seasonal_coefs[1]
  loc1b = seasonal_coefs[2]
  loc1c = seasonal_coefs[3]
  loc1d = seasonal_coefs[4]
  start_ind = 1
  end_ind = 365

  # simulating the forecasted temperatures for the next year
  # based on the continuous-time modelling and non-Gaussian spatio-temporal random fields
  i_seq = (t1 + start_ind):(t1 + end_ind)
  i_seq2 = start_ind:end_ind
  seas = loc1a + loc1b * i_seq + loc1c * cos(2 * pi * (i_seq - loc1d) / 365) # 1st term in pricing formula
  re = sapply(i_seq2, function(i) expm::expm(A) %*% pmin(sigma1 %*% sigma_s(resids, i), 1)) # 2nd term in pricing formula
  re1 = expm::expm(A) %*% sigma1 %*% residuals[n, ] * damp # 3rd term in pricing formula
  re_vec = apply(re, 2, function(x) x[station])
  new_temps = seas + re_vec + re1[station] + qnorm(sig) * ghyp::rghyp(length(i_seq2)) # new temperatures simulated
  new_temps[new_temps < -10 | new_temps > 100] = NA # controls the minimum and maximum of the simulated temperatures
  new_temps = zoo::na.approx(new_temps, rule = 3) # interpolated values with outliers removed
  lower_conf = new_temps - qnorm(sig) * abs(re_vec) # left endpoints of confidence intervals
  upper_conf = new_temps + qnorm(sig) * abs(re_vec) # right endpoints of confidence intervals
  lower_conf[lower_conf < -10 | lower_conf > 100] = NA; lower_conf = zoo::na.approx(lower_conf, rule = 3)
  upper_conf[upper_conf < -10 | upper_conf > 100] = NA; upper_conf = zoo::na.approx(upper_conf, rule = 3)
  full_temps = c(temp, new_temps) # appending the new temperatures in the future to the list of past temperatures

  # returns the future temperatures (point estimates), their lower and upper levels of the corresponding confidence intervals
  # and the updated list of temperatures (appending the newly forecasted temperatures to the past temperatures)
  # point = a 1 by 365 vector containing the forecasted temperatures for the next year
  # lower = a 1 by 365 vector containing the left endpoints of the confidence intervals for the forecasted temperatures
  # upper = a 1 by 365 vector containing the right endpoints of the confidence intervals for the forecasted temperatures
  # newtemps = a 1 by 365 * 6 vector containing the past and future temperatures at that particular station
  return(list(point = new_temps, lower = lower_conf, upper = upper_conf, newtemps = full_temps))
}

# Function that computes and returns the objective function for fitting the seasonal variance function
loc1seasonal_s = function(params, loc1seasonal_var) {
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
sigma_s = function(sresids, t) {
  if (t == 1 | t == 2 | t == 3) {
    t = t + 3 # adjusting indices
  }
  sampled = sresids[seq(t, t + 365, by = 365), ] # select samples
  return(cov(sampled)) # output covariance matrix
}
