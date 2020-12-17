#include <Rcpp.h>
using namespace Rcpp;

//' @title timesTwo
//' @description A Gibbs sampler using Rcpp
//' @param x the number of between-sample random numbers
//' @return a random sample of size a
//' @examples
//' \dontrun{
//' timesTwo(x)
//' }
//' @export
// [[Rcpp::export]]
int timesTwo(int x) {
  return x * 2;
}
