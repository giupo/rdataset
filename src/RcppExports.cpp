// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// tsWrite_nativo
void tsWrite_nativo(NumericVector series, std::string path);
RcppExport SEXP rdataset_tsWrite_nativo(SEXP seriesSEXP, SEXP pathSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type series(seriesSEXP);
    Rcpp::traits::input_parameter< std::string >::type path(pathSEXP);
    tsWrite_nativo(series, path);
    return R_NilValue;
END_RCPP
}
// tsRead_nativo
NumericVector tsRead_nativo(std::string path);
RcppExport SEXP rdataset_tsRead_nativo(SEXP pathSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type path(pathSEXP);
    rcpp_result_gen = Rcpp::wrap(tsRead_nativo(path));
    return rcpp_result_gen;
END_RCPP
}
