# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' @title A gibbs sampler using Rcpp
#' @description Implement A gibbs sampler using Rcpp for generating the 
#' two-dimensional normal distribution
#' @param N Number of random numbers
#' @param rho Correlation coefficient of two-dimensional normal distribution
#' @param mu1 Mean value of the first normal distribution
#' @param mu2 Mean value of the second normal distribution
#' @param sigma1 Standard deviation of the first normal distribution
#' @param sigma2 Standard deviation of the second normal distribution
#' @return Returns chain
#' @examples
#' \dontrun{
#' bmygibbsC(1000,0.7,0,0,1,1)
#' }
#' @export
bmygibbsC <- function(N, rho, mu1, mu2, sigma1, sigma2) {
    .Call('_StatComp22013_bmygibbsC', PACKAGE = 'StatComp22013', N, rho, mu1, mu2, sigma1, sigma2)
}

