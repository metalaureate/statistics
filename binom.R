mu1 <- log(1)   
mu2 <- log(90)
sig1 <- log(3)
sig2 <- log(3)
cpct <- .6  

bimodalDistFunc <- function (n,cpct, mu1, mu2, sig1, sig2) {
  y0 <- rlnorm(n,mean=mu1, sd = sig1)
  y1 <- rlnorm(n,mean=mu2, sd = sig2)
  
  flag <- rbinom(n,size=1,prob=cpct)
  y <- y0*(1 - flag) + y1*flag 
}

bimodalData <- bimodalDistFunc(n=10000,cpct,mu1,mu2, sig1,sig2)
hist(log(bimodalData),breaks=500)
b <- hist(log(bimodalData),breaks=500)
str(b)
a <- seq(-5, 5, 1/66)
plot(a+5, (pi/2 - atan(a))/3, "l")