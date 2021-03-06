// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// perceptronC
NumericVector perceptronC(NumericVector w, double b, double l_rate, NumericMatrix feature, NumericVector target);
RcppExport SEXP _StatComp20042_perceptronC(SEXP wSEXP, SEXP bSEXP, SEXP l_rateSEXP, SEXP featureSEXP, SEXP targetSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type w(wSEXP);
    Rcpp::traits::input_parameter< double >::type b(bSEXP);
    Rcpp::traits::input_parameter< double >::type l_rate(l_rateSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type feature(featureSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type target(targetSEXP);
    rcpp_result_gen = Rcpp::wrap(perceptronC(w, b, l_rate, feature, target));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_StatComp20042_perceptronC", (DL_FUNC) &_StatComp20042_perceptronC, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_StatComp20042(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
