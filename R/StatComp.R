#' @title Benchmark R and Rcpp functions.
#' @name benchmarks
#' @description Use R package \code{microbenchmark} to compare the performance of C functions (\code{gibbsR} and \code{vaccR}) and Cpp functions (\code{gibbsC} and \code{vaccC}).
#' @import microbenchmark
#' @import Rcpp
#' @useDynLib StatComp20042
#' @examples
#' \dontrun{
#' microbenchmark(sumtwo(1,2),timesTwo(100))
#' }
NULL
#' Title
#'
#' @param x num
#' @param y num
#'
#' @return x+y
#' @export
#'
#' @examples
#' \dontrun{
#' sumtwo(1,2)
#' }
sumtwo <- function(x,y) {
  return(x+y)
}
