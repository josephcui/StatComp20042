#include <Rcpp.h>
using namespace Rcpp;

double dot(NumericVector x0,NumericVector y0){
  NumericVector dotVector =x0*y0;
  double s = 0;
  for (int i=0;i<dotVector.length();i++){
    s = s+dotVector[i];
  }
  return(s);
}




#include <Rcpp.h>
using namespace Rcpp;

//' @title perceptronC
//' @description  divide linearly separable datasets into two categories
//' @param w wight vector
//' @param b bias
//' @param l_rate learning rate
//' @param feature characteristic matrix
//' @param target category
//' @return  parameters of hyperplane
//' @examples
//' \dontrun{
//' data(iris_trim)
//' w = c(0,0)
//' b=0
//' f = iris_trim[,c(1,2)]
//' t = iris_trim[,5]
//' l_rate =0.001
//' perceptronC(w,b,l_rate,f,t)
//' }
//' @export
// [[Rcpp::export]]
NumericVector perceptronC(NumericVector w,double b, double l_rate,NumericMatrix feature, NumericVector target){
  bool judge = true;
  while(judge){
    int wrong_cnt =0;
    for (int j=0;j<feature.nrow();j++){
      if(target[j] * (dot(feature.row(j),w)+b) <= 0) {
        w = w + l_rate*target[j]*feature.row(j);
        b = b + l_rate*target[j];
        wrong_cnt = wrong_cnt+1;
      }
    }
    if(wrong_cnt==1){
      judge = false;
    }
  }
  w.insert(0,b);
  return(w);
}
