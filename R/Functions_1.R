
Age.labels <- c('Total', '0', '1-4', '5-9', '10-14','15-19','20-24','25-29',
  '30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69',
  '70-74','75-79','80-84')

Category.labels <- c(
  'Infectious, non-R',
  'Cancer AS',
  'Cancer NAS',
  'Diabetes',
  'Cardiovascular',
  'Respiratory I',
  'Respiratory NI',
  'External',
  'Other')




AKm02a0        <- function(m0, sex = "m"){
  sex <- rep(sex, length(m0))
  ifelse(sex == "m", 
         ifelse(m0 < .0230, {0.14929 - 1.99545 * m0},
                ifelse(m0 < 0.08307, {0.02832 + 3.26201 * m0},.29915)),
         # f
         ifelse(m0 < 0.01724, {0.14903 - 2.05527 * m0},
                ifelse(m0 < 0.06891, {0.04667 + 3.88089 * m0}, 0.31411))
  )
}

LifeExpectancy <- compiler::cmpfun(function(mx,sex = "f"){
  i.openage <- length(mx)
  OPENAGE   <- i.openage - 1
  RADIX     <- 1
  ax        <- mx * 0 + .5
  ax[1]     <- AKm02a0(m0 = mx[1], sex = sex)
  qx        <- mx / (1 + (1 - ax) * mx)
  qx[i.openage]       <- ifelse(is.na(qx[i.openage]), NA, 1)
  ax[i.openage]       <- 1 / mx[i.openage]                   
  px 				    <- 1 - qx
  px[is.nan(px)]      <- 0
  lx 			        <- c(RADIX, RADIX * cumprod(px[1:OPENAGE]))
  dx 				    <- lx * qx
  Lx 				    <- lx - (1 - ax) * dx
  Lx[i.openage ]	    <- lx[i.openage ] * ax[i.openage ]
  Tx 				    <- c(rev(cumsum(rev(Lx[1:OPENAGE]))),0) + Lx[i.openage]
  ex 				    <- Tx / lx
  ex[1]
})

LifeTable      <- function(mx,sex = "f"){
  mx <- as.matrix(mx)
  i.openage <- nrow(mx)
  ax        <- mx * 0 + .5
  ax[1, ]   <- AKm02a0(m0 = mx[1, ], sex = sex)
  qx        <- mx / (1 + (1 - ax) * mx)        
  qx[i.openage, ]       <- ifelse(is.na(qx[i.openage, ]), NA, 1)
  ax[i.openage, ]       <- 1 / mx[i.openage, ]                   
  px 				      <- 1 - qx 																				
  px[is.nan(px)]  <- 0 
  lx 			        <- apply(px, 2, function(px., RADIX, OPENAGE){ 		
    if (all(is.na(px.))) {
      px.
    } else {
      c(RADIX, RADIX * cumprod(px.[1:OPENAGE]))
    }
  }, RADIX = 1, OPENAGE = i.openage - 1
  )
  rownames(lx)    <- 0:(i.openage - 1) 
  dx 				      <- lx * qx 																				
  Lx 				      <- lx - (1 - ax) * dx 														
  Lx[i.openage, ]	<- lx[i.openage, ] * ax[i.openage, ]
  Tx 				      <- apply(Lx, 2, function(Lx., i.openage, OPENAGE){
    c(rev(cumsum(rev(Lx.[1:OPENAGE]))),0) + Lx.[i.openage]	
  }, OPENAGE = i.openage - 1, i.openage = i.openage
  )
  rownames(Tx)    <- rownames(lx)
  ex 				      <- Tx / lx 	                              
  list(e0=ex[1,],ex=ex,lx=lx,mx=mx)
}

# First create function that estimates lbar from mortality rates (function from Ugofilippo)
Gini.frommx           <- function(mx,sex="f"){
  i.openage <- length(mx)
  OPENAGE    <- i.openage - 1
  RADIX     <- 1
  ax        <- mx * 0 + .5
  ax[1]     <- AKm02a0(m0 = mx[1], sex = sex)
  qx        <- mx / (1 + (1 - ax) * mx)
  qx[i.openage]       <- ifelse(is.na(qx[i.openage]), NA, 1)
  ax[i.openage]       <- 1 / mx[i.openage]                   
  px 				    <- 1 - qx
  px[is.nan(px)]      <- 0
  lx 			        <- c(RADIX, RADIX * cumprod(px[1:OPENAGE]))
  dx 				    <- lx * qx
  Lx 				    <- lx - (1 - ax) * dx
  Lx[i.openage ]	    <- lx[i.openage ] * ax[i.openage ]
  Tx 				    <- c(rev(cumsum(rev(Lx[1:OPENAGE]))),0) + Lx[i.openage]
  ex 				    <- Tx / lx
  cx        <- ax-1/2
  lx.1      <- c(lx[-1],0)
  lx.1.sq   <- lx.1^2
  ax.hat    <- (1-(2/3)*qx+cx*(2-qx-(6/5)*cx))/(2-qx)
  ax.hat[1] <- ax[1]*(1-qx[1]*(3+0.831*ax[1])/(2+qx[1]))  
  count     <- lx.1.sq + ax.hat*(lx^2 - lx.1.sq)     
  lbar      <- sum(count)/(ex[1]*(lx[1])^2)  
  Gini      <- 1- lbar
  return(Gini)
}


### a function to calclate lifespan equality, like Colchero et al from mortality rates
Keyfitz.frommx <- function(mx,sex){
  i.openage <- length(mx)
  OPENAGE   <- i.openage - 1
  RADIX     <- 1
  ax        <- mx * 0 + .5
  ax[1]     <- AKm02a0(m0 = mx[1], sex = sex)
  qx        <- mx / (1 + (1 - ax) * mx)
  qx[i.openage]       <- ifelse(is.na(qx[i.openage]), NA, 1)
  ax[i.openage]       <- 1 / mx[i.openage]                   
  px 				    <- 1 - qx
  px[is.nan(px)]      <- 0
  lx 			        <- c(RADIX, RADIX * cumprod(px[1:OPENAGE]))
  dx 				    <- lx * qx
  Lx 				    <- lx - (1 - ax) * dx
  Lx[i.openage ]	    <- lx[i.openage ] * ax[i.openage ]
  Tx 				    <- c(rev(cumsum(rev(Lx[1:OPENAGE]))),0) + Lx[i.openage]
  ex 				    <- Tx / lx
  l <- length(ex)
  v <- (sum(dx[-l]* (ex[-l] + ax[-l]*(ex[-1]-ex[-l]) )) + ex[l])
  k <- v/ex[1]
  return(k)
}

cv.frommx <- function(mx,sex){
  i.openage <- length(mx)
  OPENAGE   <- i.openage - 1
  RADIX     <- 1
  ax        <- mx * 0 + .5
  ax[1]     <- AKm02a0(m0 = mx[1], sex = sex)
  qx        <- mx / (1 + (1 - ax) * mx)
  qx[i.openage]       <- ifelse(is.na(qx[i.openage]), NA, 1)
  ax[i.openage]       <- 1 / mx[i.openage]                   
  px 				    <- 1 - qx
  px[is.nan(px)]      <- 0
  lx 			        <- c(RADIX, RADIX * cumprod(px[1:OPENAGE]))
  dx 				    <- lx * qx
  Lx 				    <- lx - (1 - ax) * dx
  Lx[i.openage ]	    <- lx[i.openage ] * ax[i.openage ]
  Tx 				    <- c(rev(cumsum(rev(Lx[1:OPENAGE]))),0) + Lx[i.openage]
  ex 				    <- Tx / lx
  age           <- 0:(i.openage-1)
  vx <- sum(dx*(age+ax-ex[1L])^2)
  cv <- sqrt(vx)/ex[1L]
  return(cv)
}

SD.frommx <- function(mx,sex){
  i.openage <- length(mx)
  OPENAGE   <- i.openage - 1
  RADIX     <- 1
  ax        <- mx * 0 + .5
  ax[1]     <- AKm02a0(m0 = mx[1], sex = sex)
  qx        <- mx / (1 + (1 - ax) * mx)
  qx[i.openage]       <- ifelse(is.na(qx[i.openage]), NA, 1)
  ax[i.openage]       <- 1 / mx[i.openage]                   
  px 				    <- 1 - qx
  px[is.nan(px)]      <- 0
  lx 			        <- c(RADIX, RADIX * cumprod(px[1:OPENAGE]))
  dx 				    <- lx * qx
  Lx 				    <- lx - (1 - ax) * dx
  Lx[i.openage ]	    <- lx[i.openage ] * ax[i.openage ]
  Tx 				    <- c(rev(cumsum(rev(Lx[1:OPENAGE]))),0) + Lx[i.openage]
  ex 				    <- Tx / lx
  age           <- 0:(i.openage-1)
  vx <- sum(dx*(age+ax-ex[1L])^2)
  SD <- sqrt(vx)
  return(SD)
}

# Subdiagonal function functionformakingsubdiagonals(fromBillVenables)
subdiag<-function(v,k){
  n <-length(v)+abs(k)
  x <-matrix(0,n,n)
  if(k==0)
    diag(x)<-v
  else if (k<0)
  { ##sub-diagonal
    j <-1:(n+k)
    i <-(1-k):n
    x[cbind(i,j)]<-v
  }
  x
}

SD.sensitivity <- function(qx,ax,Age=0:110,...){
  q <- qx
  x <- ax + Age
  # defining survival probabilities
  p   <- 1-q
  s   <- length(p)+1
  s2  <- length(p)
  I   <- diag(rep(1,s)) #identity matrix
  I   <- as(I,"sparseMatrix")
  e   <- rep(1,s) #vector of ones for summations
  e1  <- c(1,rep(0,s-1))
  age <- 0:s2
  #  C matrix is for calculating cumulative sums
  C  <-  Matrix(0,nrow=s,ncol=s)
  for(i in 1:s){  C[,i]<-c(rep(0,i),rep(1,s-i)) }
  C  <-as(C,"sparseMatrix")
  
  U <- subdiag(p,-1)
  U <- as(U,"sparseMatrix")
  N <- solve(I-U)
  N <- as(N,"sparseMatrix")
  M <- diag(c(q,1))
  M <- as(M,"sparseMatrix")
  B <- M%*%N #the complete distribution of ages at death
  B <- as(B,"sparseMatrix")
  # survivorship (alternatively ell<-e-C%*%f)
  ell <- N%*%e1
  ell <- as(ell,"sparseMatrix")
  
  # remaining life expectancy at each age, check consistency with lifetable
  mean_eta <- colSums(N)-0.5
  
  eta2 <- 2*mean_eta%*%N
  
  V <- eta2-mean_eta*mean_eta
  S <- sqrt(V)
  
  # derivatives of U with respect to mortality change
  dvecU_dmu <- Matrix(0,nrow=s*s,ncol=s-1)
  r <- seq(2,s*s,s+1) # rows that will contain the elements once stacked
  for (i in 1:(s-1)){
    dvecU_dmu[r[i],i] <- -p[i]
  }
  dvecU_dmu <- as(dvecU_dmu,"sparseMatrix")
  
  dV_dmu <- (2*kronecker(t(N) %*% t(N),t(mean_eta)) +
               2*kronecker(t(N),mean_eta %*% N) -
               (I + 2*(diag(mean_eta))) %*% kronecker(t(N),t(mean_eta))) %*% dvecU_dmu
  
  dS_dmu <- 0.5*diag(as.vector(1/S)) %*% dV_dmu
  dS_dmu <- colSums(dS_dmu)
  return(as.vector(dS_dmu))
  
}


# Decomposition function based on Horiuchi etal 2008, 
# better to check my last program with van Raalte 2016

Decomp <-function (func, rates1, rates2, N, ...) {
  y1 <- func(rates1, ...)
  y2 <- func(rates2, ...)
  d <- rates2 - rates1
  n <- length(rates1)
  delta <- d/N
  x <- rates1 + d * matrix(rep(0.5:(N - 0.5)/N, length(rates1)), 
                           byrow = TRUE, ncol = N)
  cc <- matrix(0, nrow = n, ncol = N)
  for (j in 1:N) {
    for (i in 1:n) {
      z <- rep(0, n)
      z[i] <- delta[i]/2
      cc[i, j] <- func((x[, j] + z), ...) - func((x[, j] - 
                                                    z), ...)
    }
  }
  return(rowSums(cc))
}

# A function to make colors transparent
makeTransparent <- function(someColor, alpha=100){
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}

### Some functions to create evaluating graphs

# A function that returns the year range of the differences according to the lag
Y <- function(Year,lag.2){  
  seq(min(Year),max(Year)-lag.2,1)
}

# Some graphical settings
my.settings1 <- list(  
  strip.background=list(col="grey"),
  strip.border=list(col="black")  
)

my.settings2 <- list(  
  strip.background=list(col="grey"),
  strip.border=list(col="black")
  ,axis.line=list(col=c(0))
)

