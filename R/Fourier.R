# Function that returns the price of a temperature futures using the truncated Fourier series method
# resid = n by p matrix containing the past deseasonalized temperatures data at p stations over (n / 365) years
# station = the specific station that the price of the temperature futures depends on
# start_ind = index of the start of the measurement period
# end_ind = index of the end of the measurement period
# type = type of the temperature futures, CDD, HDD or CAT
Fourier = function(resid, station, start_ind, end_ind, type) {
  n = length(sredis)
  years = n / 365 # number of years included
  sresids_m = matrix(sresids, nrow = 365, byrow = TRUE) # rearranging
  svar = rowMeans(sresids_m) # means for each day

  # Simulate the time variable (e.g., days of the year)
  t <- 1:365

  # Fit Fourier series to the data
  fit <- fit_fourier_series(temperature_data, t)

  # Calculate residuals (deviation from Fourier fit)
  residuals <- calculate_residuals(fit, temperature_data, t)

  # Calculate seasonal variance based on residuals
  seasonal_variance <- calculate_variance(residuals)

  # Depending on the type of future (CDD, HDD, CAT), calculate the price
  if (future_type == "CDD") {
    price <- sum(pmax(temperature_data - 65, 0) * seasonal_variance)  # CDD = Cooling Degree Days
  } else if (future_type == "HDD") {
    price <- sum(pmax(65 - temperature_data, 0) * seasonal_variance)  # HDD = Heating Degree Days
  } else if (future_type == "CAT") {
    price <- sum(temperature_data)  # CAT = temperature average or total
  }

  # Return the computed futures price
  return(price)
}

fit_fourier_series <- function(temps, t, num_terms = 3) {
  # Fit Fourier series to the temperature data
  formula <- as.formula(paste("temps ~ a + b * t +",
                              paste(paste("c", 1:num_terms, "* cos(2 * pi * (t - d", 1:num_terms, ") / 365)", sep = ""), collapse = " + ")))

  fit <- nls(formula, data = data.frame(temps = temps, t = t), start = list(a = 50, b = 0, c1 = 35, d1 = 0))
  return(fit)
}

# Function to calculate residuals after Fourier fit
calculate_residuals <- function(fit, temps, t) {
  fitted_temps <- predict(fit, newdata = data.frame(t = t))
  residuals <- temps - fitted_temps
  return(residuals)
}

# Function to compute the variance based on residuals (seasonal variance)
calculate_variance <- function(residuals) {
  squared_residuals <- residuals^2
  seasonal_variance <- rep(NA, length(residuals))
  for (i in 3:length(residuals)) {
    seasonal_variance[i] <- mean(squared_residuals[(i-2):(i+2)])  # Using a rolling window
  }
  return(seasonal_variance)
}
