# Demo of the penalized composite link model (PCLM) for grouped counts

pclm <- function(y, C, X, lambda = 1, deg = 2, show = F){
  # Fit a PCLM (estimate b in ) E(y) = C %*% exp(X %*% b)
  # y = the vector of observed counts of length i
  # C = the composition matrix of dimension IxJ
  # X = the identity matrix of dimension JxJ; or B-spline basis
  # lambda = smoothing parameter
  # deg = order of differences of the components of b
  # show = indicates whether iteration details should be shown
  
  # Some preparations
  nx <- dim(X)[2]
  D <- diff(diag(nx), diff = deg)
  bstart <- log(sum(y) / nx);
  b <- rep(bstart, nx);
  
  # Perform the iterations
  for (it in 1:50) {
    b0 <- b
    eta <- X %*% b
    gam <- exp(eta)
    mu <- C %*% gam
    w <- c(1 / mu, rep(lambda, nx - deg)) 
    Gam <- gam %*% rep(1, nx)
    Q <- C %*% (Gam * X)
    z <- c(y - mu + Q %*% b, rep(0, nx - deg))
    Fit <- lsfit(rbind(Q, D), z, wt = w, intercept = F) 
    b <- Fit$coef
    
    db <- max(abs(b - b0))
    if (show)  cat(it, " ", db, "\n")
    if (db < 1e-6) break
    
  }
  # cat(it, "  ", db, "\n")
  
  # Regression diagnostics
  R <- t(Q) %*% diag(c(1 / mu)) %*% Q
  H <- solve(R + lambda * t(D) %*% D, R)
  H0 <- solve(R + lambda * t(D) %*% D) # variance-covariance matrix Bayesian approach
  H1 <- H0 %*% R %*% H0 # variance-covaraince matrix sandwitch estimator
  fit <- list()
  fit$trace <- sum(diag(H))
  ok <- y > 0
  fit$dev <- 2 * sum(y[ok] * log(y[ok] / mu[ok]))
  fit$gamma <- gam
  fit$aic <- fit$dev + 2 * fit$trace
  fit$bic <- fit$dev + log(length(y)) * fit$trace
  fit$mu <- mu
  fit$H0 <- H0
  fit$H1 <- H1
  fit$eta <- eta
  fit
}

Single_COD_fun<- function(y2=y3){
  
  # some parameters
  y <- y2[-1]
  
  m  <- 110
  x  <- 1:m
  gr <- c(1,seq(5,80,5))
  n  <- length(gr)
  
  # Make C matrix and (trivial) basis B
  C                <- matrix(0, n, m)
  C[1,1:4]         <- 1
  C[2:(n-1),5:79]  <- kronecker(diag(n-2), matrix(1, 1, 5))
  C[n, 80:110]     <- 1
  
  B <- diag(m)
  
  lambda <- 10^seq(-2,7, .25)
  
  AICs <- parallelsugar::mclapply(lambda, function(z,y,C,B){
    mod <- pclm(y,C,B,z,deg=2)
    mod$aic
  },  y=y,C=C,B=B,mc.cores = 4)
  
  AICs <- unlist(AICs)
  
  lambda.hat <- lambda[which.min(AICs)]
  # or use alternatively BIC, especially indicated for large sample sizes because AIC has the tendency to undersmooth
  
  # solve the PCLM 
  mod = pclm(y,C,B,lambda = lambda.hat, deg = 2)
  
  c(y2[1],mod$gamma)
}


#get.AIC <- function( z, ...){
#  mod <- pclm(y,C,B,z,deg = 2)
#  mod$aic
#}

#y3 <- Data[Data$Country == 4050 & Data$Sex == 'f' & Data$Year == 1960 & Data$Cat == 2,]$Dx
#y2=y3
#Single_COD_fun(y2=y3)
#plot(Single_COD_fun(y2=y3))