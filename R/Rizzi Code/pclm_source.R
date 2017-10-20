
# univariate pclm source function

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

