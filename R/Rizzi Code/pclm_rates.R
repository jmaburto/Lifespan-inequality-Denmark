
# Estimation of smooth mortality rates from grouped mortality counts

setwd("Q:/pclm R package")
source("pclm_source.r")

# DATA AND PREPARATION
# Read actual deaths counts by single-year of age from HMD, Sweden 2014
HMDSweden_deaths <- read.csv("HMDSweden_deaths.txt", sep="")
# Aggregate them artificially in 5-years age classes with 85+ 
HMDSweden_deaths$Groups_Counts <- c(rep(1:17, each=5), rep(18,26))
a_g <- aggregate (HMDSweden_deaths$Total, by=list(HMDSweden_deaths$Groups_Counts), FUN="sum")
y <- a_g$x # for rates no bin with 0 counts!!
# Read actual exposures in single year age classes. 
# if also exposures are grouped, ungrouped them first with the pclm!
HMDSweden_exposures <- read.csv("HMDSweden_exposures.txt", sep="")
E = as.matrix(HMDSweden_exposures$Total)

# Define the detailed grid where to estimate the underlying density
m = 111
x = 1:m

# Compute the group counts, i.e. 5-years age classes with 85+  
ilo = seq(1, 86, by = 5)
ihi = ilo + 4
n = length(ihi)
ihi[n] = m
# intervals lengths
leng <- ihi-ilo+1

# Make C matrix and (trivial) basis B
C0 = matrix(0, n, m)
for (i in 1:n) C0[i, ilo[i] : ihi[i]] =  1
Cp = C0%*%diag(c(E)) # multiply C matrix by exposures as offset
B = diag(m)

# SOLVE THE PCLM 
lambda <- 10^seq(-2,7,.25)
nl <- length(lambda)
AICs <- numeric(nl)
for(j in 1:nl){
  mod = pclm(y, Cp, B,lambda = lambda[j], deg = 2)
  AICs[j] <- mod$aic
}
lambda.hat <- lambda[which.min(AICs)]

mod = pclm(y, Cp, B,lambda = lambda.hat, deg = 2)
cat('lambda.hat, ED & AIC:', lambda.hat, mod$trace, mod$aic, '\n')

# PLOT RATES
# estimated smooth rates
plot(0:110, log10(mod$gamma), type="l", col="red", lwd=2,
     ylab="Death Rates (log10)", xlab="Age",
     main="Death rates, Sweden 2014. 
     Source: HMD")
# true empirical rates
lines(0:110, log10(HMDSweden_deaths$Total/HMDSweden_exposures$Total), type="o", col="blue")






