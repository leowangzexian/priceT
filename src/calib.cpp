// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]

// Objective function for optimization
// [[Rcpp::export]]
double obj_cpp(double theta, double market_price, const Rcpp::NumericMatrix& residuals, int station,
               int start_ind, int end_ind, const std::string& type, const Rcpp::NumericVector& seasonal_coefs,
               Rcpp::Function func) {

  // Call the func (Fourier, adaptBW, MLSS_spacetime) function in Rcpp and get the price
  Rcpp::List price_data = func(residuals, station, start_ind, end_ind, type, seasonal_coefs);
  double price_t = Rcpp::as<double>(price_data["price"]);

  // Manually compute squared difference between model price and market price
  double price_diff = price_t - market_price;
  return price_diff * price_diff; // (price_t - market_price)^2
}

// Gradient of the objective function (derivative of squared difference)
double grad_obj_cpp(double theta, double market_price, const Rcpp::NumericMatrix& residuals, int station,
                    int start_ind, int end_ind, const std::string& type, const Rcpp::NumericVector& seasonal_coefs,
                    Rcpp::Function func) {

  // Call the func (Fourier, adaptBW, MLSS_spacetime) function in Rcpp and get the price
  Rcpp::List price_data = func(residuals, station, start_ind, end_ind, type, seasonal_coefs);
  double price_t = Rcpp::as<double>(price_data["price"]);

  // Compute the gradient (derivative of the objective function with respect to theta)
  double price_diff = price_t - market_price;
  double gradient = 2 * price_diff; // Derivative of (price_t - market_price)^2 is 2 * (price_t - market_price)

  return gradient;
}

// Function to perform optimization using Gradient Descent with theta limits
// [[Rcpp::export]]
Rcpp::List calib_cpp(double market_price, const Rcpp::NumericMatrix& residuals, int station, int start_ind,
                     int end_ind, const std::string& type, const Rcpp::NumericVector& seasonal_coefs,
                     Rcpp::Function func, double learning_rate = 0.1, int max_iter = 1000, double tol = 1e-6,
                     double theta_min = -50.0, double theta_max = 50.0) {

  // Compatibility checks (similar to the R code)
  int n = residuals.nrow();
  int p = residuals.ncol();
  if (market_price <= 0) Rcpp::stop("market_price should be positive.");
  if (n < 365 * 2) Rcpp::stop("Need at least two years of data for residuals.");
  if (p < 3) Rcpp::stop("Need at least three stations.");
  if (station <= 0 || station > p) Rcpp::stop("station index is out of bounds.");
  if (start_ind < 1 || start_ind > 365 || end_ind < 1 || end_ind > 365 || start_ind >= end_ind)
    Rcpp::stop("start_ind should be smaller than end_ind.");
  if (seasonal_coefs.size() != 4) Rcpp::stop("seasonal_coefs should be of length 4.");
  if (!(type == "CDD" || type == "HDD" || type == "CAT")) Rcpp::stop("Invalid type.");

  // Initialize theta (starting point)
  double theta = 1.0;
  double prev_theta = 0.0;
  double gradient = 0.0;

  // Gradient Descent Loop
  int iter = 0;
  while (iter < max_iter) {
    // Compute the gradient of the objective function
    gradient = grad_obj_cpp(theta, market_price, residuals, station, start_ind, end_ind, type, seasonal_coefs, func);

    // Update theta using the gradient descent rule: theta = theta - learning_rate * gradient
    prev_theta = theta;
    theta = theta - learning_rate * gradient;

    // Ensure theta stays within the bounds of [-50, 50]
    if (theta < theta_min) {
      theta = theta_min;
    } else if (theta > theta_max) {
      theta = theta_max;
    }

    // Check for convergence (if the change in theta is below the tolerance)
    if (std::abs(theta - prev_theta) < tol) {
      break;
    }

    // Increment the iteration counter
    iter++;
  }

  // Now, compute the calibrated price and error using the optimized theta
  Rcpp::List price_data = func(residuals, station, start_ind, end_ind, type, seasonal_coefs);
  double price_t = Rcpp::as<double>(price_data["price"]);

  // Manually compute the in-sample pricing error percentage
  double price_diff = price_t - market_price;
  double error_percent = (price_diff * price_diff) / (market_price * market_price) * 100;

  // Return the result as a list
  return Rcpp::List::create(Rcpp::Named("theta") = theta, Rcpp::Named("error") = error_percent);
}
