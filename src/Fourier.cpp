// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]

// Function to compute the seasonal variance fit using a truncated Fourier series
// [[Rcpp::export]]
Rcpp::NumericVector loc1seasonal_c(const Rcpp::NumericVector& params, const Rcpp::NumericVector& loc1seasonal_var) {
  int n_days = 365;
  int n_harmonics = 16;

  // Create a NumericVector with a sequence of days
  Rcpp::NumericVector time_indices(n_days);
  for (int i = 0; i < n_days; ++i) {
    time_indices[i] = i;  // Filling with 0, 1, 2, ..., n_days-1 (0-based indexing)
  }

  // Create cosine and sine matrices for all harmonics
  Rcpp::NumericMatrix cos_matrix(n_days, n_harmonics), sin_matrix(n_days, n_harmonics);
  for (int j = 0; j < n_harmonics; ++j) {
    for (int t = 0; t < n_days; ++t) {
      cos_matrix(t, j) = cos(2 * M_PI * j * time_indices[t] / n_days);
      sin_matrix(t, j) = sin(2 * M_PI * j * time_indices[t] / n_days);
    }
  }

  // Extract the coefficients for cosine and sine using Rcpp::Range
  Rcpp::NumericVector cos_coeffs = params[Rcpp::Range(0, n_harmonics - 1)]; // Indices 0 to n_harmonics-1
  Rcpp::NumericVector sin_coeffs = params[Rcpp::Range(n_harmonics, 2 * n_harmonics - 1)]; // Indices n_harmonics to 2*n_harmonics-1

  // Compute the sum2 matrix for all days and harmonics
  Rcpp::NumericVector sum2_matrix(n_days);
  for (int t = 0; t < n_days; ++t) {
    sum2_matrix[t] = params[0]; // Initial constant
    for (int j = 0; j < n_harmonics; ++j) {
      sum2_matrix[t] += cos_matrix(t, j) * cos_coeffs[j] + sin_matrix(t, j) * sin_coeffs[j];
    }
  }

  // Compute residuals (difference between predicted and actual seasonal variance)
  Rcpp::NumericVector residuals = sum2_matrix - loc1seasonal_var;

  // Return the sum of squared residuals (least squares)
  return residuals * residuals;
}

// Function to compute the covariance matrix of standardized residuals for each day
// [[Rcpp::export]]
Rcpp::NumericMatrix sigma_c(const Rcpp::NumericMatrix& sresids) {
  int n = sresids.nrow(); // Number of observations (days)
  int p = sresids.ncol(); // Number of stations

  // Compute the mean of each column (station)
  Rcpp::NumericVector means(p);
  for (int j = 0; j < p; ++j) {
    double sum = 0.0;
    for (int i = 0; i < n; ++i) {
      sum += sresids(i, j); // Accumulate the sum for column j
    }
    means[j] = sum / n; // Compute the mean of column j
  }

  // Center the data (subtract the mean from each value)
  Rcpp::NumericMatrix centered(n, p);
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < p; ++j) {
      centered(i, j) = sresids(i, j) - means[j];// Center each value
    }
  }

  // Compute the covariance matrix
  Rcpp::NumericMatrix cov_matrix(p, p);
  for (int i = 0; i < p; ++i) {
    for (int j = i; j < p; ++j) {
      double cov_ij = 0;
      for (int k = 0; k < n; ++k) {
        cov_ij += centered(k, i) * centered(k, j); // Sum of product of deviations
      }
      cov_ij /= (n - 1); // Normalize by (n - 1) for sample covariance
      cov_matrix(i, j) = cov_ij;
      cov_matrix(j, i) = cov_ij; // Symmetric matrix
    }
  }

  // Return the computed covariance matrix
  return cov_matrix;
}

// Function to generate a sequence of numbers from start to end
// [[Rcpp::export]]
Rcpp::NumericVector create_sequence(int start, int end) {
  int n = end - start + 1;
  Rcpp::NumericVector seq(n);
  for (int i = 0; i < n; ++i) {
    seq[i] = start + i;
  }
  return seq;
}

// [[Rcpp::export]]
Rcpp::List Fourier_cpp(const Rcpp::NumericMatrix& residuals, int station, int start_ind, int end_ind, const Rcpp::String& type, const Rcpp::NumericVector& seasonal_coefs) {
  int n = residuals.nrow();
  int p = residuals.ncol();

  // compatibility checks
  if (n < 365 * 2) {
    Rcpp::stop("Need at least two years of data for residuals.");
  }
  if (p < 3) {
    Rcpp::stop("Need at least three stations.");
  }
  if (station < 1 || station > p) {
    Rcpp::stop("Station index is out of bounds.");
  }
  if (start_ind < 1 || start_ind > 365 || end_ind < 1 || end_ind > 365 || start_ind >= end_ind) {
    Rcpp::stop("Invalid start or end index.");
  }
  if (seasonal_coefs.size() != 4) Rcpp::stop("seasonal_coefs should be of length 4.");
  if (!(type == "CDD" || type == "HDD" || type == "CAT")) Rcpp::stop("Invalid type.");

  Rcpp::NumericMatrix resids = residuals;

  // Initializing parameters for derivative pricing
  double t1 = 24455;
  double loc1a = seasonal_coefs[0];
  double loc1b = seasonal_coefs[1];
  double loc1c = seasonal_coefs[2];
  double loc1d = seasonal_coefs[3];

  double acc = 0;

  // Generate sequence for the time index range
  Rcpp::NumericVector i_seq = create_sequence(t1 + start_ind - 1, t1 + end_ind - 1); // Adjust for 0-based indexing

  Rcpp::NumericVector seas = loc1a + loc1b * i_seq + loc1c * cos(2 * M_PI * (i_seq - loc1d) / 365);

  acc = sum(seas);

  // Return the computed futures price and standardized residuals
  return Rcpp::List::create(
    Rcpp::Named("price") = acc,
    Rcpp::Named("sresids") = resids
  );
}
