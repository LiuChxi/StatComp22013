#include <Rcpp.h>
using namespace Rcpp;

//' @title A gibbs sampler using Rcpp
//' @description Implement A gibbs sampler using Rcpp for generating the two-dimensional normal distribution
//' @param N Number of random numbers
//' @param rho Correlation coefficient of two-dimensional normal distribution
//' @param mu1 Mean value of the first normal distribution
//' @param mu2 Mean value of the second normal distribution
//' @param sigma1 Standard deviation of the first normal distribution
//' @param sigma2 Standard deviation of the second normal distribution
//' @return Returns chain
//' @examples
//' \dontrun{
//' bmygibbsC(1000,0.7,0,0,1,1)
//' }
//' @export
// [[Rcpp::export]]
NumericMatrix bmygibbsC(int N,double rho,double mu1,double mu2,double sigma1,double sigma2){
  NumericMatrix mat(N,2);
  double s1 = sqrt(1 - pow(rho,2))* sigma1;
  double s2 = sqrt(1 - pow(rho,2))* sigma2;
  double x=mu1,y=mu2;
  for (int i=0;i<N;i++){
    double m1=mu1+rho*(y-mu2) * sigma1/sigma2;
    x=rnorm(1, m1,s1)[0];
    double m2=mu2+rho*(x-mu1) * sigma2/sigma1; 
    y=rnorm(1, m2,s2)[0];
    mat(i,0)=x;
    mat(i,1)=y;
  }
  return(mat);
}
