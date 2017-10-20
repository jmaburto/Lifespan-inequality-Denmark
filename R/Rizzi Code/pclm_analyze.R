# Estimation of smooth age at death distributions from grouped death counts

setwd("Q:/pclm R package")
source("pclm_source.r")

# DATA AND PREPARATION
# Read actual deaths counts by single-year of age from HMD, Sweden 2014
HMDSweden_deaths <- read.csv("HMDSweden_deaths.txt", sep="")
# Aggregate them artificially in 5-years age classes with 85+ 
# add last bin with 0 counts to complete the histogram
HMDSweden_deaths$Groups_Counts <- c(rep(1:17, each=5), rep(18,26))
a_g <- aggregate(HMDSweden_deaths$Total, by=list(HMDSweden_deaths$Groups_Counts), FUN="sum")
y_1 <- a_g$x 
y <-  c(y_1,0)


# detailed grid for the underlying distribution
# we assume no deaths after the age of 115
m = 130
x = 1:m

# compute the group counts
# 5-year age classes with open-ended age class at 85
# close the histogram with bin of 0 counts where no deaths are expected
# here we should have a user-friendly and flexible way to insert the breaks
# addition of the bin with 0 counts at the right-hand side of the distribution is necessary for age-at-death distributions, especially if last bin is 85+
ilo_1 = seq(1, 86, by = 5)
ilo_2 = 116
ilo = c(ilo_1, ilo_2)
ihi = c(seq(5,85,5),115,130)
n = length(ihi)
ihi[n] = m
# intervals lengths
leng <- ihi-ilo+1

# make C matrix and (trivial) basis B
C = matrix(0, n, m)
for (i in 1:n) C[i, ilo[i] : ihi[i]] =  1
B = diag(m)
# Construct B-spline basis if detailed sequence to be estimated is > 130 (rule of thumb)
# library(MortalitySmooth)
# B <- MortSmooth_bbase(x=x, xl=min(x), xr=max(x),ndx=floor(m/5), deg=3) # ncol(B) = ndx + deg =  
# rule of thumb: one knot for the B-spline basis each 4-5 observations. 


# SOLVE THE MODEL
# find the lambda that minimizes the AIC
lambda <- 10^seq(-2,7, .25)
nl <- length(lambda)
AICs <- numeric(nl)
for(j in 1:nl){
  mod = pclm(y, C, B,lambda = lambda[j], deg = 2)
  AICs[j] <- mod$aic
}
lambda.hat <- lambda[which.min(AICs)]
# or use alternatively BIC, especially indicated for large sample sizes because AIC has the tendency to undersmooth

# solve the PCLM 
mod = pclm(y, C, B,lambda = lambda.hat, deg = 2)
cat('lambda.hat, ED & AIC:', lambda.hat, mod$trace, mod$aic, '\n')
# as output we should have mod$gamma, mod$trace, mod$dev, mod$aic or mod$bic, mod$H0, mod$H1, mod$eta,lambda.hat, s.e.

# standard errors using sandwitch estimator (use alternatively mod$H0 for Bayesian approach)
s.e. = sqrt(diag(mod$H1))
# confidence intervals
lower = exp(mod$eta - 2*s.e.)
upper = exp(mod$eta + 2*s.e.)


# PLOT
hist <- y/leng
Age <- 0:129
# estimated distribution
plot(Age, mod$gamma, type="l", ylab="No. of Deaths", xlab="Age", 
main="Age at death distribution, Sweden 2014. 
     Source: HMD", col = "red", lwd=3)
# true dstribution values
lines(0:110, HMDSweden_deaths$Total, type="o", col="blue") 
# histogram
for(i in 1:n){
  polygon(x=c(ilo[i], ihi[i], ihi[i], ilo[i]),
          y=c(0, 0, hist[i], hist[i]),
          angle=seq(45,180,length=n)[i])
}
# confidence intervals
lines(Age, lower, col="pink") 
lines(Age, upper, col="pink")
