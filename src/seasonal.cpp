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
double loc1temp(Rcpp::NumericVector params, Rcpp::NumericVector t, Rcpp::NumericVector loc1temperatures) {
  double S2 = 0.0;
  int n = loc1temperatures.size();

  // Compute the seasonality function and the squared differences
  for (int i = 0; i < n; ++i) {
    double S = params[0] + params[1] * t[i] + params[2] * cos(2 * 3.14159265358979323846 * (t[i] - params[3]) / 365);
    S2 += (S - loc1temperatures[i]) * (S - loc1temperatures[i]);
  }

  return S2;
}

// Function to performs the minimization of the objective function
// [[Rcpp::export]]
Rcpp::NumericVector loc1optim(Rcpp::NumericVector initial, double* t, Rcpp::NumericVector loc1temperatures) {
  // Number of iterations for optimization
  const int max_iter = 1000;
  const double learning_rate = 0.001;

  // Initial parameter guess
  Rcpp::NumericVector params = initial;

  // Gradient descent optimization loop
  for (int iter = 0; iter < max_iter; ++iter) {
    // Compute the gradient
    Rcpp::NumericVector gradient(4);

    // Compute the objective function value
    double S2 = loc1temp(params, t, loc1temperatures);

    // Compute partial derivatives for each parameter
    for (int i = 0; i < 4; ++i) {
      Rcpp::NumericVector params_up = params;
      params_up[i] += 1e-6;
      gradient[i] = (loc1temp(params_up, t, loc1temperatures) - S2) / 1e-6;
    }

    // Update the parameters based on the gradient
    for (int i = 0; i < 4; ++i) {
      params[i] -= learning_rate * gradient[i];
    }
  }

  return params;
}

// [[Rcpp::export]]
Rcpp::List seasonal_cpp(Rcpp::NumericVector temp) {
  // compatibility check
  if (temp.size() == 0) {
    Rcpp::stop("temp should be a vector."); // returns error message if the input temp is not a vector
  }

  int n = temp.size();
  double *t = new double[n];  // raw arrays for time indices
  for (int i = 0; i < n; ++i) {
    t[i] = i + 1; // time indices
  }

  // Initial guess for the parameters a, b, c and d
  Rcpp::NumericVector initial = {50, 0, 35, 0};

  // Minimise the objective function using a simple optimization approach
  Rcpp::NumericVector opt_params = loc1optim(initial, t, temp);

  double loc1a = opt_params[0];
  double loc1b = opt_params[1];
  double loc1c = opt_params[2];
  double loc1d = opt_params[3];

  // Compute the fitted seasonal function
  Rcpp::NumericVector loc1fitted(n);
  for (int i = 0; i < n; ++i) {
    loc1fitted[i] = loc1a + loc1b * t[i] + loc1c * cos_approx(2 * 3.14159265358979323846 * (t[i] - loc1d) / 365);
  }

  // Free the dynamically allocated memory for t
  delete[] t;

  // Return the results as a list
  return Rcpp::List::create(
    Rcpp::Named("a") = loc1a,
    Rcpp::Named("b") = loc1b,
    Rcpp::Named("c") = loc1c,
    Rcpp::Named("d") = loc1d,
    Rcpp::Named("seasonality") = loc1fitted
  );
}
