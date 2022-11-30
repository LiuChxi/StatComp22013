#' @title Generate random numbers related to normal distribution using R
#' @description This function can generate a mixed distribution of two normal distributions in mode 1 and a two-dimensional normal distribution in mode 2 Mode 1 adopts independent sampling algorithm with normal distribution as the  proposed distribution, and mode 2 adopts gibbs sampling algorithm.
#' @param mode 1 is to choose to mix two normal distributions, 2 is to choose to generate two-dimensional normal distributions
#' @param burn Points removed before convergence
#' @param p Proportion of the first normal distribution in mode1
#' @param mu1 Mean value of the first normal distribution
#' @param sigma1 Standard deviation of the first normal distribution
#' @param mu2 Mean value of the second normal distribution
#' @param sigma2 Standard deviation of the second normal distribution
#' @param mu0 Mean of the proposed distribution in model 1
#' @param sigma0 Standard deviation of the proposed distribution in model 1
#' @param m the the length of the chain
#' @param rho Correlation coefficient of two-dimensional normal distribution in model 2
#' @return the relevant convergence and scatter plot
#' @importFrom stats rnorm runif
#' @importFrom stats dnorm
#' @examples
#' \dontrun{
#' bmymixnorsam(1,1000,0.2,0,1,5,1,4,1,10000,0.2)
#' bmymixnorsam(2,1000,0.2,0,1,2,1,0,0,10000,-0.7)
#' }
#' @export
bmymixnorsam<-function(mode,burn,p,mu1,sigma1,mu2,sigma2,mu0,sigma0,m,rho){
  if(mode == 1){
    f<-function(s){
        p*(2*pi*(sigma1^2))^(-1/2)*exp(-((s-mu1)^2)/(2*(sigma1^2)))+
        (1-p)*(2*pi*(sigma2^2))^(-1/2)*exp(-((s-mu2)^2)/(2*(sigma2^2)))
    }
    x<-numeric(m)
    # 写出提议分布的参数情况(可修改)
    x[1]<-rnorm(1,mu0,sigma0)
    u<-runif(m)
    for(i in 2:m){
      xt<-x[i-1] 
      y<-rnorm(1,mu0,sigma0) 
      #mh
      num<-f(y)*dnorm(xt,mu0,sigma0) 
      den<-f(xt)*dnorm(y,mu0,sigma0) 
      #判断是否接受由提议分布生成的随机数
      if(u[i]<=num/den) 
        x[i]<-y 
      else{
        x[i]<-xt 
      }
    }
    picchain<-plot(x[burn+1:m],type = "l",ylab="x")
    
    result1=list()
    result1[["chain"]]<-x[burn+1:m]
    result1[["convergence"]]<-picchain
    
    return (result1)
  }
  #生成二维正态分布
  if(mode == 2){
    X<-matrix(0,m,2)
    s1<-sqrt(1-rho^2)*sigma1
    s2<-sqrt(1-rho^2)*sigma2
    
    X[1,]<-c(mu1,mu2)
    #gibbs
    for(i in 2:m){
      x2<-X[i-1,2]
      m1<-mu1+rho*(x2-mu2)*sigma1/sigma2
      X[i,1]<-rnorm(1,m1,s1)
      
      x1<-X[i-1,1]
      m2<-mu2+rho*(x1-mu1)*sigma2/sigma1
      X[i,2]<-rnorm(1,m2,s2)
    }
    xx<-X[(burn+1):m,]
    picchain<-plot(xx,main="",ylim=range(xx[,2]))
    
    result2=list()
    result2[["chain"]]<-xx
    result2[["Plot"]]<-picchain
    
    return (result2)
  }
  
}
