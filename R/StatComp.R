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














#' Title
#'
#' @param w wight vector
#' @param b bias
#' @param l_rate learning rate
#' @param feature characteristic matrix
#' @param target category
#'
#' @return parameters of hyperplane
#' @export
#'
#' @examples
#' \dontrun{
#' library(StatComp20042)
#' w = c(0,0)
#' b=0
#' f = iris_trim[,c(1,2)]
#' t = iris_trim[,3]
#' l_rate =0.001
#' perceptronR(w,b,l_rate,f,t)
#' }
perceptronR<- function(w,b,l_rate,feature,target){
  judge=TRUE
  while(judge){
    wrong_cnt <- 0
    for(i in 1:nrow(feature)){
      if(target[i] * ((feature[i,] %*% w)+b) <=0) {
        w <- w + l_rate*target[i]*feature[i,]
        b <- b + l_rate*target[i]
        wrong_cnt = wrong_cnt+1
      }
    }
    if (wrong_cnt ==0){
      judge=FALSE
    }

  }
  re<- list(w,b)
  return(re)
}











