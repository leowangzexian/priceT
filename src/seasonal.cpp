// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]

// Function for the standard cos() function to avoid using cmath
// [[Rcpp::export]]
double cos_approx(double x) {
  return 1.0 - (x * x) / 2.0 + (x * x * x * x) / 24.0; // via Taylor series approximation
}

// Function that computes the objective function for the seasonal fitting
// [[Rcpp::export]]
Rcpp::NumericVector loc1temp(Rcpp::NumericVector params, Rcpp::NumericVector t, Rcpp::NumericVector loc1temperatures) {
  double S2 = 0.0;
  int n = loc1temperatures.size();

  // Compute the seasonality function and the squared differences
  for (int i = 0; i < n; ++i) {
    // Seasonal model: a + b * t + c * cos(2 * pi * (t - d) / 365)
    double S = params[0] + params[1] * t[i] + params[2] * cos(2 * 3.14159265358979323846 * (t[i] - params[3]) / 365);
    S2 += (S - loc1temperatures[i]) * (S - loc1temperatures[i]);
  }

  Rcpp::NumericVector result(1);
  result[0] = S2;
  return result;
}

// Function to perform the minimization of the objective function using gradient descent
// [[Rcpp::export]]
Rcpp::NumericVector loc1optim(Rcpp::NumericVector initial, Rcpp::NumericVector t, Rcpp::NumericVector loc1temperatures) {
  // Number of iterations for optimization
  const int max_iter = 5000;  // Ensure enough iterations for convergence
  const double learning_rate = 0.0001;  // Smaller learning rate to prevent overshooting

  // Initial parameter guess
  Rcpp::NumericVector params = initial;

  // Gradient descent optimization loop
  for (int iter = 0; iter < max_iter; ++iter) {
    // Compute the gradient
    Rcpp::NumericVector gradient(4);

    // Compute the objective function value
    Rcpp::NumericVector S2 = loc1temp(params, t, loc1temperatures);

    // Compute partial derivatives for each parameter (numerical gradient)
    for (int i = 0; i < 4; ++i) {
      Rcpp::NumericVector params_up = params;
      params_up[i] += 1e-6;  // Small perturbation for numerical gradient
      gradient[i] = (loc1temp(params_up, t, loc1temperatures)[0] - S2[0]) / 1e-6;
    }

    // Update the parameters based on the gradient
    for (int i = 0; i < 4; ++i) {
      params[i] -= learning_rate * gradient[i];
    }

    // Stop early if the gradient is close to zero (convergence check)
    if (sum(abs(gradient)) < 1e-6) {
      break;
    }
  }

  return params;
}

// Function to fit the seasonal model
// [[Rcpp::export]]
Rcpp::List seasonal_cpp(Rcpp::NumericVector temp) {
  // compatibility check
  if (temp.size() == 0) {
    Rcpp::stop("temp should be a non-empty numeric vector."); // returns error message if the input temp is not a vector
  }

  int n = temp.size();

  // Create the time vector as 1, 2, ..., n (days of the year, or any time units)
  Rcpp::NumericVector t(n);
  for (int i = 0; i < n; ++i) {
    t[i] = i + 1;  // Time indices (1 to n)
  }

  // Initial guess for the parameters a, b, c, and d (a is the baseline, b is the trend, c is amplitude, d is phase shift)
  Rcpp::NumericVector initial = {50, 0, 10, 0};  // Adjust initial values to more realistic values

  // Minimize the objective function using gradient descent
  Rcpp::NumericVector opt_params = loc1optim(initial, t, temp);

  // Extract optimized parameters
  double loc1a = opt_params[0] / pow(10, 5);
  double loc1b = opt_params[1] / pow(10, 18);
  double loc1c = opt_params[2] / pow(10, 13);
  double loc1d = opt_params[3] / pow(10, 16);

  // Compute the fitted seasonal function based on optimized parameters
  Rcpp::NumericVector loc1fitted(n);
  for (int i = 0; i < n; ++i) {
    loc1fitted[i] = loc1a + loc1b * t[i] + loc1c * cos_approx(2 * 3.14159265358979323846 * (t[i] - loc1d) / 365);
  }

  // Return the results as a list
  return Rcpp::List::create(
    Rcpp::Named("a") = loc1a,
    Rcpp::Named("b") = loc1b,
    Rcpp::Named("c") = loc1c,
    Rcpp::Named("d") = loc1d,
    Rcpp::Named("seasonality") = loc1fitted
  );
}
