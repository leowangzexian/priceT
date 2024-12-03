// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// loc1seasonal_c
Rcpp::NumericVector loc1seasonal_c(const Rcpp::NumericVector& params, const Rcpp::NumericVector& loc1seasonal_var);
RcppExport SEXP _priceT_loc1seasonal_c(SEXP paramsSEXP, SEXP loc1seasonal_varSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type params(paramsSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type loc1seasonal_var(loc1seasonal_varSEXP);
    rcpp_result_gen = Rcpp::wrap(loc1seasonal_c(params, loc1seasonal_var));
    return rcpp_result_gen;
END_RCPP
}
// sigma_c
Rcpp::NumericMatrix sigma_c(const Rcpp::NumericMatrix& sresids);
RcppExport SEXP _priceT_sigma_c(SEXP sresidsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::NumericMatrix& >::type sresids(sresidsSEXP);
    rcpp_result_gen = Rcpp::wrap(sigma_c(sresids));
    return rcpp_result_gen;
END_RCPP
}
// create_sequence
Rcpp::NumericVector create_sequence(int start, int end);
RcppExport SEXP _priceT_create_sequence(SEXP startSEXP, SEXP endSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type start(startSEXP);
    Rcpp::traits::input_parameter< int >::type end(endSEXP);
    rcpp_result_gen = Rcpp::wrap(create_sequence(start, end));
    return rcpp_result_gen;
END_RCPP
}
// Fourier_cpp
Rcpp::List Fourier_cpp(const Rcpp::NumericMatrix& residuals, int station, int start_ind, int end_ind, const Rcpp::String& type, const Rcpp::NumericVector& seasonal_coefs);
RcppExport SEXP _priceT_Fourier_cpp(SEXP residualsSEXP, SEXP stationSEXP, SEXP start_indSEXP, SEXP end_indSEXP, SEXP typeSEXP, SEXP seasonal_coefsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::NumericMatrix& >::type residuals(residualsSEXP);
    Rcpp::traits::input_parameter< int >::type station(stationSEXP);
    Rcpp::traits::input_parameter< int >::type start_ind(start_indSEXP);
    Rcpp::traits::input_parameter< int >::type end_ind(end_indSEXP);
    Rcpp::traits::input_parameter< const Rcpp::String& >::type type(typeSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type seasonal_coefs(seasonal_coefsSEXP);
    rcpp_result_gen = Rcpp::wrap(Fourier_cpp(residuals, station, start_ind, end_ind, type, seasonal_coefs));
    return rcpp_result_gen;
END_RCPP
}
// obj_cpp
double obj_cpp(double theta, double market_price, const Rcpp::NumericMatrix& residuals, int station, int start_ind, int end_ind, const std::string& type, const Rcpp::NumericVector& seasonal_coefs, Rcpp::Function func);
RcppExport SEXP _priceT_obj_cpp(SEXP thetaSEXP, SEXP market_priceSEXP, SEXP residualsSEXP, SEXP stationSEXP, SEXP start_indSEXP, SEXP end_indSEXP, SEXP typeSEXP, SEXP seasonal_coefsSEXP, SEXP funcSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< double >::type market_price(market_priceSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericMatrix& >::type residuals(residualsSEXP);
    Rcpp::traits::input_parameter< int >::type station(stationSEXP);
    Rcpp::traits::input_parameter< int >::type start_ind(start_indSEXP);
    Rcpp::traits::input_parameter< int >::type end_ind(end_indSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type type(typeSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type seasonal_coefs(seasonal_coefsSEXP);
    Rcpp::traits::input_parameter< Rcpp::Function >::type func(funcSEXP);
    rcpp_result_gen = Rcpp::wrap(obj_cpp(theta, market_price, residuals, station, start_ind, end_ind, type, seasonal_coefs, func));
    return rcpp_result_gen;
END_RCPP
}
// calib_cpp
Rcpp::List calib_cpp(double market_price, const Rcpp::NumericMatrix& residuals, int station, int start_ind, int end_ind, const std::string& type, const Rcpp::NumericVector& seasonal_coefs, Rcpp::Function func, double learning_rate, int max_iter, double tol, double theta_min, double theta_max);
RcppExport SEXP _priceT_calib_cpp(SEXP market_priceSEXP, SEXP residualsSEXP, SEXP stationSEXP, SEXP start_indSEXP, SEXP end_indSEXP, SEXP typeSEXP, SEXP seasonal_coefsSEXP, SEXP funcSEXP, SEXP learning_rateSEXP, SEXP max_iterSEXP, SEXP tolSEXP, SEXP theta_minSEXP, SEXP theta_maxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type market_price(market_priceSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericMatrix& >::type residuals(residualsSEXP);
    Rcpp::traits::input_parameter< int >::type station(stationSEXP);
    Rcpp::traits::input_parameter< int >::type start_ind(start_indSEXP);
    Rcpp::traits::input_parameter< int >::type end_ind(end_indSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type type(typeSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type seasonal_coefs(seasonal_coefsSEXP);
    Rcpp::traits::input_parameter< Rcpp::Function >::type func(funcSEXP);
    Rcpp::traits::input_parameter< double >::type learning_rate(learning_rateSEXP);
    Rcpp::traits::input_parameter< int >::type max_iter(max_iterSEXP);
    Rcpp::traits::input_parameter< double >::type tol(tolSEXP);
    Rcpp::traits::input_parameter< double >::type theta_min(theta_minSEXP);
    Rcpp::traits::input_parameter< double >::type theta_max(theta_maxSEXP);
    rcpp_result_gen = Rcpp::wrap(calib_cpp(market_price, residuals, station, start_ind, end_ind, type, seasonal_coefs, func, learning_rate, max_iter, tol, theta_min, theta_max));
    return rcpp_result_gen;
END_RCPP
}
// cos_approx
double cos_approx(double x);
RcppExport SEXP _priceT_cos_approx(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(cos_approx(x));
    return rcpp_result_gen;
END_RCPP
}
// loc1temp_c
Rcpp::NumericVector loc1temp_c(Rcpp::NumericVector params, Rcpp::NumericVector t, Rcpp::NumericVector loc1temperatures);
RcppExport SEXP _priceT_loc1temp_c(SEXP paramsSEXP, SEXP tSEXP, SEXP loc1temperaturesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type params(paramsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type t(tSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type loc1temperatures(loc1temperaturesSEXP);
    rcpp_result_gen = Rcpp::wrap(loc1temp_c(params, t, loc1temperatures));
    return rcpp_result_gen;
END_RCPP
}
// loc1optim
Rcpp::NumericVector loc1optim(Rcpp::NumericVector initial, Rcpp::NumericVector t, Rcpp::NumericVector loc1temperatures);
RcppExport SEXP _priceT_loc1optim(SEXP initialSEXP, SEXP tSEXP, SEXP loc1temperaturesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type initial(initialSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type t(tSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type loc1temperatures(loc1temperaturesSEXP);
    rcpp_result_gen = Rcpp::wrap(loc1optim(initial, t, loc1temperatures));
    return rcpp_result_gen;
END_RCPP
}
// seasonal_cpp
Rcpp::List seasonal_cpp(Rcpp::NumericVector temp);
RcppExport SEXP _priceT_seasonal_cpp(SEXP tempSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type temp(tempSEXP);
    rcpp_result_gen = Rcpp::wrap(seasonal_cpp(temp));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_priceT_loc1seasonal_c", (DL_FUNC) &_priceT_loc1seasonal_c, 2},
    {"_priceT_sigma_c", (DL_FUNC) &_priceT_sigma_c, 1},
    {"_priceT_create_sequence", (DL_FUNC) &_priceT_create_sequence, 2},
    {"_priceT_Fourier_cpp", (DL_FUNC) &_priceT_Fourier_cpp, 6},
    {"_priceT_obj_cpp", (DL_FUNC) &_priceT_obj_cpp, 9},
    {"_priceT_calib_cpp", (DL_FUNC) &_priceT_calib_cpp, 13},
    {"_priceT_cos_approx", (DL_FUNC) &_priceT_cos_approx, 1},
    {"_priceT_loc1temp_c", (DL_FUNC) &_priceT_loc1temp_c, 3},
    {"_priceT_loc1optim", (DL_FUNC) &_priceT_loc1optim, 3},
    {"_priceT_seasonal_cpp", (DL_FUNC) &_priceT_seasonal_cpp, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_priceT(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}