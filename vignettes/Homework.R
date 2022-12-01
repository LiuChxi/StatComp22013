## -----------------------------------------------------------------------------
height<-c(163,158,157,158,157,170,167,160,167,160,170,160,165,162,166,169,159,162,166,169)
jump<-c(180,173,175,177,176,182,180,177,180,167,198,167,180,175,180,179,165,175,185,189)
cab=cbind(height,jump)
library(knitr)
kable(cab)

## -----------------------------------------------------------------------------
fit=lm(height ~ jump)
summary(fit)$coef
summary(fit)$r.squared

## -----------------------------------------------------------------------------
par(mfrow=c(2,2))
plot(fit)

## -----------------------------------------------------------------------------
n <- 500
u <- runif(n)
x <- 2*(1-u)^{-1/2} 
hist(x, prob = TRUE, main = expression(f(x)==8*x^(-3)))  
y <- seq(0, 100, .01)
lines(y, 8*y^(-3)) 

## -----------------------------------------------------------------------------
n <- 1e4;j<-k<-0;y <- numeric(n)
while (k < n) {
  u <- runif(1)
  j <- j + 1
  x <- runif(1)
  if ((x^2) * (1-x) > u) {
    k <- k + 1
    y[k] <- x
  }
}
j
hist(y, prob = TRUE, main = expression(f(x)==12*x^2*(1-x))) 
w <- seq(0, 10, .001)
lines(w, 12*w^2*(1-w))  

## -----------------------------------------------------------------------------
n <- 1000        
r <- 4         
beta <- 2        
lambda <- rgamma(n, r, beta)  
y <- rexp(n, lambda) 
y  

## -----------------------------------------------------------------------------
n <- 1000       
r <- 4          
beta <- 2  
lambda <- rgamma (n, r, beta)  
y <- rexp (n, lambda) 
y           
hist(y, prob = TRUE, main = expression(f(y)))  
x <- seq(0, 5, 0.01)   
lines(x, 32/(x+2)^5)   

## -----------------------------------------------------------------------------
quick_sort<-function(x){
  num<-length(x)
  if(num==0||num==1){return(x)
  }else{
    a<-x[1]
    y<-x[-1]
    lower<-y[y<a]
    upper<-y[y>=a]
    return(c(quick_sort(lower),a,quick_sort(upper)))}
}

## -----------------------------------------------------------------------------
k<-replicate(100, {
  test<-sample(1:1e4)
  system.time(quick_sort(test))[1]
})
k=mean(k)

kk<-replicate(100, {
  test<-sample(1:2e4)
  system.time(quick_sort(test))[1]
})
kk=mean(kk)

kkk<-replicate(100, {
  test<-sample(1:4e4)
  system.time(quick_sort(test))[1]
})
kkk=mean(kkk)

kkkk<-replicate(100, {
  test<-sample(1:6e4)
  system.time(quick_sort(test))[1]
})
kkkk=mean(kkkk)

kkkkk<-replicate(100, {
  test<-sample(1:8e4)
  system.time(quick_sort(test))[1]
})
kkkkk=mean(kkkkk)

yy<-c(k,kk,kkk,kkkk,kkkkk)
xx<-c(1e4*log(1e4),2e4*log(2e4),3e4*log(3e4),4e4*log(4e4),5e4*log(5e4))

## -----------------------------------------------------------------------------
data <- data.frame(yy,xx)

## -----------------------------------------------------------------------------
model <- lm(yy~xx, data=data)
summary(model)
install.packages("ggplot2", repos="https://cloud.r-project.org/")
library(ggplot2)

ggplot(data, aes(yy, xx))+
  geom_point(size=3,shape=21)+
  geom_smooth(method = "lm")+
  labs(x="nlog(n)",y="an")

## -----------------------------------------------------------------------------
s<-20000

## -----------------------------------------------------------------------------
mc<-replicate(2000,expr={
  mean(exp(runif(s)))
})

## -----------------------------------------------------------------------------
zz<-replicate(2000,expr={
  u<-runif(s/2)
  v<-1-u
  mean((exp(u)+exp(v))/2)
})

## -----------------------------------------------------------------------------
v1<-var(mc)
v2<-var(zz)
mm<-(v1-v2)/v1
mm

## -----------------------------------------------------------------------------
x<-seq(1,10,0.01)
y<-(x^2)*exp(-x^2/2)/sqrt(2*pi)
plot(x,y,type="l",ylim=c(0,0.5))

## -----------------------------------------------------------------------------
plot(x,y,type="l",ylim=c(0,1))
lines(x,2*dnorm(x,1,1),lty=2)
lines(x,dgamma(x-1,4/3,0.9),lty=3)
legend("topright",inset=0.01,legend=c("g(x)","f1(x)","f2(x)"),lty=1:3)

## -----------------------------------------------------------------------------
plot(x,y/(2*dnorm(x,1,1)),type="l",lty=2, ylab="",ylim = c(0,1))
lines(x,y/dgamma(x-1,4/3,0.9),lty=3)
legend("topright",inset = 0.01,legend = c("f1(x)","f2(x)"),lty=2:3)

## -----------------------------------------------------------------------------
m<-10000
is1<-replicate(1000,expr = {
  x<-sqrt(rchisq(m,1))+1
  f<-2*dnorm(x,1,1)
  g<-(x^2)*exp(-x^2/2)/sqrt(2*pi)
  mean(g/f)
})

is2<-replicate(1000,expr = {
  x<-rgamma(m,4/3,2)+1
  f<-dgamma(x-1,4/3,2)
  g<-(x^2)*exp(-x^2/2)/sqrt(2*pi)
  mean(g/f)
})

c(mean(is1),mean(is2))
c(var(is1),var(is2))
var(is1)/var(is2)

## -----------------------------------------------------------------------------
M<-20000
k<-5
m<-M/k
mn<-numeric(k)
va<-numeric(k)
g<-function(x) exp(-x)/(1+x^2)
f<-function(x) (k/(1-exp(-1)))*exp(-x)
for (j in 1:k){
  u<-runif(m,(j-1)/k,j/k)
  x<- -log(1-(1-exp(-1))*u)
  ra<- g(x)/f(x)
  mn[j]<-mean(ra)
  va[j]<-var(ra)
}
sum(mn)
mean(va)
sqrt(mean(va))

## -----------------------------------------------------------------------------
M<-20000
mn<-0
va<-0
f<-function(x) (1/(1-exp(-1)))*exp(-x)
u<-runif(M,0,1)
x<- -log(1-(1-exp(-1))*u)
ra<- g(x)/f(x)
mn<-mean(ra)
va<-var(ra)
sum(mn)
mean(va)
sqrt(mean(va))

## -----------------------------------------------------------------------------
integrate(g,lower=0,upper=1)

## -----------------------------------------------------------------------------
M <- 10000; k <- 5 
r <- M/k 
N <- 50 
T2 <- numeric(k)
est <- matrix(0, N, 2)
g<-function(x)exp(-x)/(1+x^2)
for (i in 1:N) {
  est[i, 1] <- mean(g(runif(M)))
  for(j in 1:k)T2[j]<-mean(g(runif(M/k,(j-1)/k,j/k)))
  est[i, 2] <- mean(T2)
}
round(apply(est,2,mean),4)
round(apply(est,2,sd),5)

## -----------------------------------------------------------------------------
rm(list = ls())
f1 <- function(m,n){
  set.seed(123)
  y <- matrix(0,m,n)
  for (i in 1:m){
    x <- rlnorm(n)
    y[i,1:n] <- log(x)
  }
  y
}

## -----------------------------------------------------------------------------
  f2 <- function(Sam){
  m <- nrow(Sam)
  n <- ncol(Sam)
  CI <- matrix(0,m,2)
  for (i in 1:m){
    y <- Sam[i,1:n]
    ybar <- mean(y)
    se <- sd(y)/sqrt(n)
    CI[i,1] <- ybar + se * qnorm(c(0.025))
    CI[i,2] <- ybar + se * qnorm(c(0.975)) 
  }
  CI
}

## -----------------------------------------------------------------------------
f3 <- function(confidenceint){
  m <- nrow(confidenceint)
  LOW <- confidenceint[1:m,1]
  UP <- confidenceint[1:m,2]
  res <- mean(LOW < 0 & UP > 0)
  res
}

## -----------------------------------------------------------------------------
m <- 10000
n <- 100
sam <- f1(m,n)
ci <- f2(sam)
re <- f3(ci) 

Cii <- data.frame(ci)
names(Cii) <- c('Low bound','Upper bound')
knitr::kable (head(Cii))
re

## -----------------------------------------------------------------------------
rm(list = ls())
f1<- function(sigma1,sigma2,m,n){
  set.seed(12)
  Samp1 <- matrix(0,m,n)
  Samp2 <- matrix(0,m,n)
  for (i in 1:m){
    x <- rnorm(n, 0, sigma1)
    y <- rnorm(n, 0, sigma2)
    Samp1[i,1:n] <- x
    Samp2[i,1:n] <- y
  }
  SAmp <- rbind(Samp1,Samp2)
  SAmp
}

## -----------------------------------------------------------------------------
  couu <- function(x, y) {
  X <- x - mean(x)
  Y <- y - mean(y)
  outx <- sum(X > max(Y)) + sum(X < min(Y))
  outy <- sum(Y > max(X)) + sum(Y < min(X))
  cou5te <- as.integer(max(c(outx, outy)) > 5)
  cou5te
}

## -----------------------------------------------------------------------------
f2 <- function(Samp) {
  m <- nrow(Samp)/2
  n <- ncol(Samp)
  Test <- matrix(0,2,m)
  for (i in 1:m){
    x <- Samp[i,1:n]
    y <- Samp[m+i,1:n]
    C5 <- couu(x, y)
    Fp <- var.test(x, y)$p.value
    Ftest <- as.integer(Fp <= 0.055)
    Test[1:2,i] <- c(C5, Ftest)
  }
  Test
}

f3 <- function(Test) {
  RE <- c(n,rowMeans(Test))
  RE
}

## -----------------------------------------------------------------------------
sigma1 <- 1
sigma2 <- 1.5
m <- 10000
N <- c(20, 30, 50, 100, 200, 500)
K <- length(N)
resu <- matrix(0,K,3)
for (k in 1:K){
  n <- N[k]
  samp <- f1(sigma1,sigma2,m,n)
  test <- f2(samp)
  resu[k,1:3] <- f3(test)
}
resu

## -----------------------------------------------------------------------------
rm(list = ls())
library(boot)
x <- aircondit[1]
rate <- function(x, i) return(1/mean(as.matrix(x[i, ]))) 
boot(x, statistic = rate, R = 10000)

## -----------------------------------------------------------------------------
mMean <- function(x, i) return(mean(as.matrix(x[i, ])))
b <- boot(x, statistic =mMean, R = 6999) 
b

## -----------------------------------------------------------------------------
boot.ci(b, type = c("norm", "perc", "basic", "bca"))

## -----------------------------------------------------------------------------
hist(b$t, prob=TRUE, main = "")
points(b$t0, 0, cex = 2, pch = 16)

## -----------------------------------------------------------------------------
rm(list = ls())
MMEAN <- function(x,i) {
  xbar <- mean(x[i])
  return( xbar )
}

## -----------------------------------------------------------------------------
TH <- 0
n <- 30
m <- 1220
set.seed(1234)
library(boot)
nor.norm <- nor.basic <- nor.perc <- matrix(0, m, 2)
for (i in 1:m) {
  data.nor <- rnorm(n, 0, 3.28)
  nor.ske <- boot(data.nor, statistic = MMEAN, R=1000)
  nor <- boot.ci(nor.ske, type=c("norm","basic","perc"))
  nor.norm[i,] <- nor$norm[2:3]
  nor.basic[i,] <- nor$basic[4:5]
  nor.perc[i,] <- nor$percent[4:5]
}

## -----------------------------------------------------------------------------
norm <- mean(nor.norm[,1] <= TH & nor.norm[,2] >= TH)
basic <- mean(nor.basic[,1] <= TH & nor.basic[,2] >= TH)
perc <- mean(nor.perc[,1] <= TH & nor.perc[,2] >= TH)

## -----------------------------------------------------------------------------
norm.left <- mean(nor.norm[,1] >= TH )
basic.left <- mean(nor.basic[,1] >= TH )
perc.left <- mean(nor.perc[,1] >=TH )

## -----------------------------------------------------------------------------
norm.right <- mean(nor.norm[,2] <= TH )
basic.right <- mean(nor.basic[,2] <= TH )
perc.right <- mean(nor.perc[,2] <= TH)

## -----------------------------------------------------------------------------
Type <- c("norm","basic","perc")
PinLeft <- c(norm.left,basic.left, perc.left)
PinRight <- c(norm.right, basic.right, perc.right)
P.coverage <- c(norm, basic, perc)
ffram <- data.frame(Type, PinLeft, PinRight, P.coverage)
knitr::kable(ffram)

## -----------------------------------------------------------------------------
install.packages('bootstrap',repo="mirrors.ustc.edu.cn/CRAN/")
library(bootstrap)
attach(scor)

## -----------------------------------------------------------------------------
rm(list = ls())

## -----------------------------------------------------------------------------
x <- as.matrix(scor)
n <- nrow(x)
thetajack <- numeric(n)

## -----------------------------------------------------------------------------
comlam <- function(c1){
  eigen(cov(c1))$values  
}
comtheha<- function(c2){
  max(c2/sum(c2))
}
comjack<-function(c3){
  thejack <- numeric(n)
  for  (i in 1:n) {
    y <- c3[-i, ]
    s <- cov(y)
    lamda <- eigen(s)$values
    thejack[i] <- comtheha(lamda)
  }
  thejack
}

## -----------------------------------------------------------------------------
lamda<-comlam(x)
thetahat <- comtheha(lamda)
thetajack<-comjack(x)

## -----------------------------------------------------------------------------
biasjack <- (n - 1) * (mean(thetajack) - thetahat)
sejack <- sqrt((n - 1)/n * sum((thetajack - mean(thetajack))^2)) 
c(thetahat, biasjack, sejack)

## -----------------------------------------------------------------------------
install.packages('DAAG',repo="mirrors.ustc.edu.cn/CRAN/")
library(DAAG, warn.conflict = FALSE) 
attach(ironslag)

## -----------------------------------------------------------------------------
rm(list = ls())

## -----------------------------------------------------------------------------
n <- length(magnetic)
N <- choose(n, 2)
e1 <- e2 <- e3 <- e4 <- e5 <- numeric(N) 
js<-1

## -----------------------------------------------------------------------------
come<-function(can1,can2){
  sum((can1 - can2)^2)
}

f1<-function(a,b){
  lm(a~b)
}

f2<-function(a,b){
  lm(a~b+I(b^2))
}

f3<-function(a,b){
  lm(log(a) ~ b)
}

f4<-function(a,b){
  lm(log(a) ~ log(b))
}

f5<-function(a,b){
  c1<-b^2
  c2<-b^3
  J5<-lm(a~b+c1+c2)
}

## -----------------------------------------------------------------------------
for (i in 1:(n - 1)) {
  for (j in (i + 1):n) { 
    k<-c(i,j)
    y <- magnetic[-k]
    x <- chemical[-k]
    hg1<-f1(y,x)
    yhat1 <- hg1$coef[1] + hg1$coef[2] * chemical[k] 
    e1[js] <- come(magnetic[k],yhat1)
    
    hg2<-f2(y,x)
    yhat2 <- hg2$coef[1] + hg2$coef[2] * chemical[k] + hg2$coef[3] * chemical[k]^2
    e2[js] <- come(magnetic[k],yhat2)
    
    hg3 <- f3(y,x)
    logyhat3 <- hg3$coef[1] + hg3$coef[2] * chemical[k]
    yhat3 <- exp(logyhat3)
    e3[js] <- come(magnetic[k],yhat3)
        
    hg4 <- f4(y,x)
    logyhat4 <- hg4$coef[1] + hg4$coef[2] * log(chemical[k])
    yhat4 <- exp(logyhat4)
    e4[js] <- come(magnetic[k],yhat4)
    
    hg5<-f5(y,x)
    yhat5 <- hg5$coef[1] + hg5$coef[2] * chemical[k] +hg5$coef[3] * chemical[k]^2 + hg5$coef[4] * chemical[k]^3
    e5[js] <- come(magnetic[k],yhat5)
    js=js+1
    }
 }

## -----------------------------------------------------------------------------
result<-c(sum(e1), sum(e2), sum(e3), sum(e4), sum(e5))
result/N

## -----------------------------------------------------------------------------
install.packages('MASS',repo="mirrors.ustc.edu.cn/CRAN/")
library(MASS)

## -----------------------------------------------------------------------------
rm(list = ls())

## -----------------------------------------------------------------------------
miu <- c(0, 0)
sig <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
n <- 42
R <- 642
set.seed(123)

## -----------------------------------------------------------------------------
spearff <- function(x, y) {
  spmtest <- cor.test(x, y, method = "spearman")
  n <- length(x)
  rs <- replicate(R, expr = {
    kk <- sample(1:n)
    cor.test(x, y[kk], method = "spearman")$estimate
   })
 rs1 <- c(spmtest$estimate, rs)
 pval <- mean(as.integer(spmtest$estimate <=rs1))
 return(list(rho.s = spmtest$estimate, p.value = pval))
}

## -----------------------------------------------------------------------------
funnorm<- function(c1,c2,c3){
  xk <- mvrnorm(c1, c2, c3)
  xk
}

funexpnorm<- function(c1,c2,c3){
  xkk <- exp(mvrnorm(c1, c2, c3))
  xkk
}

## -----------------------------------------------------------------------------
x<-funnorm(n, miu, sig)
cor.test(x[, 1], x[, 2], method = "spearman")
spearff(x[, 1], x[, 2])

## -----------------------------------------------------------------------------
xx<-funexpnorm(n, miu, sig)
cor.test(xx[, 1], xx[, 2], method = "spearman")
spearff(xx[, 1], xx[, 2])

## -----------------------------------------------------------------------------
#CLEAR MEMORY
rm(list = ls())
set.seed(123)

#define a random walk Metropolis sampler function 
#generating the standard Laplace distribution
scl <- function(N, x0, sigma) {
 x <- numeric(N)
 x[1] <- x0
 u <- runif(N)
 k<-0
 for (i in 2:N) {
      xt <- x[i - 1]
      y <- rnorm(1, xt, sigma)
      if (u[i] <= exp(abs(xt) - abs(y)))
        x[i] <- y
      else {
        x[i] <- x[i - 1] 
        k <- k + 1
      }
  }
return(list(x = x, k = k))
}

#define the Gelman-Rubin method function
rb<-function(psi){
  psi<-as.matrix(psi)
  n<-ncol(psi)
  k<-nrow(psi)
  psi.means<-rowMeans(psi)
  B<-n*var(psi.means)
  psi.w<-apply(psi,1,"var")
  W<-mean(psi.w)
  v.hat<-W*(n-1)/n+(B/(n*k))
  r.hat<-v.hat/W
  return(r.hat)
}

#Define initial value
N <- 50000
sigma <- c(0.5, 1, 2, 4)
x0 <- c(-2,0,2)

#By x_ 0 is 1 as the initial value and as an example
rw1 <- scl(N, x0[3], sigma[1])
rw2 <- scl(N, x0[3], sigma[2])
rw3 <- scl(N, x0[3], sigma[3])
rw4 <- scl(N, x0[3], sigma[4])

#The generated chains are compared in the following plots
par(mfrow = c(2, 2))
plot(rw1$x, type = "l") 
plot(rw2$x, type = "l") 
plot(rw3$x, type = "l") 
plot(rw4$x, type = "l") 

#monitor convergence of the chain

rw1.2 <- scl(N, x0[1], sigma[1])
rw2.2 <- scl(N, x0[1], sigma[2])
rw3.2 <- scl(N, x0[1], sigma[3])
rw4.2 <- scl(N, x0[1], sigma[4])

rw1.3 <- scl(N, x0[2], sigma[1])
rw2.3 <- scl(N, x0[2], sigma[2])
rw3.3 <- scl(N, x0[2], sigma[3])
rw4.3 <- scl(N, x0[2], sigma[4])


b <- 20000
y1 <- rw1$x[(b + 1):N] 
y2 <- rw2$x[(b + 1):N] 
y3 <- rw3$x[(b + 1):N] 
y4 <- rw4$x[(b + 1):N]

y1.2 <- rw1.2$x[(b + 1):N] 
y2.2 <- rw2.2$x[(b + 1):N] 
y3.2 <- rw3.2$x[(b + 1):N] 
y4.2 <- rw4.2$x[(b + 1):N]

y1.3 <- rw1.3$x[(b + 1):N] 
y2.3 <- rw2.3$x[(b + 1):N] 
y3.3 <- rw3.3$x[(b + 1):N] 
y4.3 <- rw4.3$x[(b + 1):N]

#sigama=0.5
xx1<-rbind(y1,y1.2,y1.3)
psi1<-t(apply(xx1,1,cumsum))
nr<-nrow(psi1)
for (i in 1:nr)
  psi1[i,]<-psi1[i,]/(1:ncol(psi1))
print(rb(psi1))

par(mfrow = c(1, 3))
for(i in 1:nr)
  plot(psi1[i,1:ncol(psi1)],type="l",xlab=i,ylab=bquote(psi1))

#sigama=1
xx2<-rbind(y2,y2.2,y2.3)
psi2<-t(apply(xx2,1,cumsum))
nr<-nrow(psi2)
for (i in 1:nr)
  psi2[i,]<-psi2[i,]/(1:ncol(psi2))
print(rb(psi2))

par(mfrow = c(1, 3))
for(i in 1:nr)
  plot(psi2[i,1:ncol(psi2)],type="l",xlab=i,ylab=bquote(psi2))

#sigama=2
xx3<-rbind(y3,y3.2,y3.3)
psi3<-t(apply(xx3,1,cumsum))
nr<-nrow(psi3)
for (i in 1:nr)
  psi3[i,]<-psi3[i,]/(1:ncol(psi3))
print(rb(psi3))

par(mfrow = c(1, 3))
for(i in 1:nr)
  plot(psi3[i,1:ncol(psi3)],type="l",xlab=i,ylab=bquote(psi3))

#sigama=4
xx4<-rbind(y4,y4.2,y4.3)
psi4<-t(apply(xx4,1,cumsum))
nr<-nrow(psi4)
for (i in 1:nr)
  psi4[i,]<-psi4[i,]/(1:ncol(psi4))
print(rb(psi4))

par(mfrow = c(1, 3))
for(i in 1:nr)
  plot(psi4[i,1:ncol(psi4)],type="l",xlab=i,ylab=bquote(psi4))


#Each of the chains appear to have converged to the target Laplace distribution.
par(mfrow = c(2, 2))
p <- ppoints(100)
y <- qexp(p, 1)
z <- c(-rev(y), y)
fx <- 0.5 * exp(-abs(z))
hist(y1, breaks = "Scott", freq = FALSE, ylim = c(0,0.5))
lines(z, fx)
hist(y2, breaks = "Scott", freq = FALSE, ylim = c(0,0.5))
lines(z, fx)
hist(y3, breaks = "Scott", freq = FALSE, ylim = c(0,0.5))
lines(z, fx)
hist(y4, breaks = "Scott", freq = FALSE, ylim = c(0, 0.5))
lines(z, fx)

#QQ plots
par(mfrow = c(2, 2))
Q1 <- quantile(y1, p)
qqplot(z, Q1, cex = 0.8) 
abline(0, 1)
Q2 <- quantile(y2, p)
qqplot(z, Q2, cex = 0.8) 
abline(0, 1)
Q3 <- quantile(y3, p)
qqplot(z, Q3, cex = 0.8) 
abline(0, 1)
Q4 <- quantile(y4, p)
qqplot(z, Q4, cex = 0.8) 
abline(0, 1)


cat("rejection rates ", (c(rw1$k, rw2$k, rw3$k, rw4$k)/N), "\n")
cat("acceptance rates ", (c(N-rw1$k,N- rw2$k,N- rw3$k,N- rw4$k)/N), "\n")


## -----------------------------------------------------------------------------
#clear memory
rm(list = ls())
set.seed(123)

#Set parameters
N <- 5000
burn <- 1000
X <- matrix(0, N, 2)
rho <- 0.9
mu1 <- 0
mu2 <- 0
sigma1 <- 1
sigma2 <- 1
s1 <- sqrt(1 - rho^2)* sigma1
s2 <- sqrt(1 - rho^2)* sigma2

#Implement a Gibbs sampler to generate a bivariate normal chain
#with zero means, unit standard deviations, and correlation 0.9
gibl<-function(X){
  for (i in 2:N) {
    x2<-X[i-1,2] 
    m1<-mu1+rho*(x2-mu2) * sigma1/sigma2
    X[i, 1] <- rnorm(1, m1,s1)
    x1 <- X[i, 1] 
    m2<-mu2+rho*(x1-mu1) * sigma2/sigma1 
    X[i, 2]<-rnorm(1, m2,s2)
  }
  return(X)
}

#define the Gelman-Rubin method function
rb<-function(psi){
  psi<-as.matrix(psi)
  n<-ncol(psi)
  k<-nrow(psi)
  psi.means<-rowMeans(psi)
  B<-n*var(psi.means)
  psi.w<-apply(psi,1,"var")
  W<-mean(psi.w)
  v.hat<-W*(n-1)/n+(B/(n*k))
  r.hat<-v.hat/W
  return(r.hat)
}

#use x0=(0,0) to output the result

X[1, ] <- c(1, 1)
X_re2<-gibl(X)

X[1, ] <- c(-1, -1)
X_re3<-gibl(X)

X[1, ] <- c(mu1, mu2)
X_re<-gibl(X)

#monitor convergence of the chain
xx1<-rbind(X_re2[(burn+1):N,1],X_re3[(burn+1):N,1],X_re[(burn+1):N,1])
psi1<-t(apply(xx1,1,cumsum))
nr<-nrow(psi1)
for (i in 1:nr)
  psi1[i,]<-psi1[i,]/(1:ncol(psi1))
print(rb(psi1))

par(mfrow = c(1, 3))
for(i in 1:nr)
  plot(psi1[i,1:ncol(psi1)],type="l",xlab=i,ylab=bquote(psi1))

xx2<-rbind(X_re2[(burn+1):N,2],X_re3[(burn+1):N,2],X_re[(burn+1):N,2])
psi2<-t(apply(xx2,1,cumsum))
nr<-nrow(psi2)
for (i in 1:nr)
  psi2[i,]<-psi2[i,]/(1:ncol(psi2))
print(rb(psi2))

par(mfrow = c(1, 3))
for(i in 1:nr)
  plot(psi2[i,1:ncol(psi2)],type="l",xlab=i,ylab=bquote(psi2))

b<- burn + 1 
x<- X_re[b:N, ] 
Xx<- x[, 1]
Y<- x[, 2]
L<- lm(Y ~ Xx)
L
summary(L)

#The scatterplot of the generated chain (after discarding the burn-in sample)
plot(Xx, Y, cex = 0.25) 
abline(h = 0, v = 0)

#residual plot
plot(L$fit, L$res, cex = 0.25) 
abline(h = 0)

#qq plot
qqnorm(L$res, cex = 0.25) 
qqline(L$res)

## -----------------------------------------------------------------------------
#CLEAR MEMORY
rm(list = ls())
set.seed(123)

# Function with equation
fun <- function(x) {
    (-11*exp(-x*11)+12*exp(-x*12))/(exp(-x*11)+exp(-x*12))+
    (-8*exp(-x*8)+9*exp(-x*9))/(exp(-x*8)+exp(-x*9))+
    (-27*exp(-x*27)+28*exp(-x*28))/(exp(-x*27)+exp(-x*28))+
    (-13*exp(-x*13)+14*exp(-x*14))/(exp(-x*13)+exp(-x*14))+
    (-0*exp(-x*0)+1*exp(-x*1))/(exp(-x*0)+exp(-x*1))+
    (-23*exp(-x*23)+24*exp(-x*24))/(exp(-x*23)+exp(-x*24))+
    (-10*exp(-x*10)+11*exp(-x*11))/(exp(-x*10)+exp(-x*11))+
    (-24*exp(-x*24)+25*exp(-x*25))/(exp(-x*24)+exp(-x*25))+
    (-2*exp(-x*2)+3*exp(-x*3))/(exp(-x*2)+exp(-x*3))+
    (-16*exp(-x*16)+17*exp(-x*17))/(exp(-x*16)+exp(-x*17))
  }
  
# Calling uniroot() function
uniroot(fun,c(0,1))$root


## -----------------------------------------------------------------------------
#CLEAR MEMORY
rm(list = ls())
set.seed(123)


# Function with equation
fun <- function(x) {
    (-11*exp(-x*11)+12*exp(-x*12))/(exp(-x*11)+exp(-x*12))+
    (-8*exp(-x*8)+9*exp(-x*9))/(exp(-x*8)+exp(-x*9))+
    (-27*exp(-x*27)+28*exp(-x*28))/(exp(-x*27)+exp(-x*28))+
    (-13*exp(-x*13)+14*exp(-x*14))/(exp(-x*13)+exp(-x*14))+
    (-0*exp(-x*0)+1*exp(-x*1))/(exp(-x*0)+exp(-x*1))+
    (-23*exp(-x*23)+24*exp(-x*24))/(exp(-x*23)+exp(-x*24))+
    (-10*exp(-x*10)+11*exp(-x*11))/(exp(-x*10)+exp(-x*11))+
    (-24*exp(-x*24)+25*exp(-x*25))/(exp(-x*24)+exp(-x*25))+
    (-2*exp(-x*2)+3*exp(-x*3))/(exp(-x*2)+exp(-x*3))+
    (-16*exp(-x*16)+17*exp(-x*17))/(exp(-x*16)+exp(-x*17))
}

#em
emfun<-function(max.it,eps){
  lambda0<-0.1
  i<-1
  theta1<-1
  theta2<-0.1
  xx<-fun(lambda0)
  while(abs(theta1-theta2)>=eps){
    theta1<-theta2
    theta2<-10/(10/theta1-xx)
    xx<-fun(theta2)
    if (i == max.it) break
    i<-i+1
  }
  return(theta2)
}

emfun(10000,1e-5)

  


## -----------------------------------------------------------------------------
#clear memory
rm(list = ls())
set.seed(123)


#Implement a Gibbs sampler to generate a bivariate normal chain
#with zero means, unit standard deviations, and correlation 0.9
gibl<-function(N){
  rho=0.9
  mu1=mu2=0
  sigma1=sigma2=1
  X <- matrix(0, N, 2)
  X[1, ] <- c(0, 0)
  s1 <- sqrt(1 - rho^2)* sigma1
  s2 <- sqrt(1 - rho^2)* sigma2
  for (i in 2:N) {
    x2<-X[i-1,2] 
    m1<-mu1+rho*(x2-mu2) * sigma1/sigma2
    X[i, 1] <- rnorm(1, m1,s1)
    x1 <- X[i, 1] 
    m2<-mu2+rho*(x1-mu1) * sigma2/sigma1 
    X[i, 2]<-rnorm(1, m2,s2)
  }
  return(X)
}

## -----------------------------------------------------------------------------
library(Rcpp)
#// This is the giblC.cpp
#include <Rcpp.h>
#using namespace Rcpp;
#// [[Rcpp::export]]
cppFunction('NumericMatrix gibbsC(int N){
  NumericMatrix mat(N,2);
  double rho=0.9,mu1=0,mu2=0,sigma1=1,sigma2=1;
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
}')


## -----------------------------------------------------------------------------
library(microbenchmark)
ts <- microbenchmark(gibbR=gibl(5000),gibbC=gibbsC(5000))
L1=gibl(5000)
L2=gibbsC(5000)
summary(ts)[,c(1,3,5,6)]
qqplot(L1[,1],L2[,1],xlab="R GIBBS", ylab="C GIBBS", col = "blue")
qqplot(L1[,2],L2[,2],xlab="R GIBBS", ylab="C GIBBS", col = "blue")
qqplot(L1,L2,xlab="R GIBBS", ylab="C GIBBS", col = "blue")

