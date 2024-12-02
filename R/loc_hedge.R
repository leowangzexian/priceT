#' Hedging spatial risk with optimal portfolio
#'
#' @description
#' Function that computes and returns the optimal weights for hedging the
#' temperature risk at a particular location using the contracts traded based on other locations
#'
#' @param residuals A n by p matrix containing the past deseasonalized temperatures data at p stations over (n / 365) years (n >= 365 * 2) (p >= 3)
#' @param station A numeric index denoting the specific station to be hedged
#'
#' @return A list containing:
#'         \item{optim_weights}{A 1 by (p - 1) vector containing the optimal weights for each station in the portfolio}
#'
#' @export
#'
#' @examples
#' # load the residuals data from residuals.rda in the data folder
#'
#' # example 1
#' residuals1 = matrix(as.numeric(residuals[, 3:5]), 730, 3)
#' station1 = 1
#' loc_hedge1 = loc_hedge(residuals1, station1)
#'
#' # examine results
#' loc_hedge1$optim_weights # weights for each of the other stations in the optimal portfolio
#'
#' # example 2
#' residuals2 = matrix(as.numeric(residuals[, 51:55]), 730, 5)
#' station2 = 3
#' loc_hedge2 = loc_hedge(residuals2, station2)
#'
#' # examine results
#' loc_hedge2$optim_weights # weights for each of the other stations in the optimal portfolio
loc_hedge = function(residuals, station) {
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

  init = rep(1 / (p - 1), p - 1) # initialise with equal weights
  target = resids[, station] # payoff to be replicated
  hedge = resids[, - station]
  optim_r = optim(
    par = init,
    fn = obj_s,
    hedge = hedge,
    target = target
  ) # unconstrained optimisation
  optim_weights = optim_r$par
  optim_weights = optim_weights / sum(optim_weights) # normalising weights

  # returns the optimal weights for each station
  # optim_weights = A 1 by (p - 1) vector containing the optimal weights for each station
  return(list(optim_weights = optim_weights))
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

# the objective function for minimisation to find the optimal weights
obj_s = function(optim_weights, hedge, target) {
  return(sum(hedge %*% optim_weights[1:(p - 1)] - target))
}
