############# Written by JMA
############# Project with Soren, Ugo & Jim Vaupel
############# Crazy EDSD'ers 2015-16
############# Perform decomposition by age of lbar and life expectancy based 
############# on a continous time model (Horiuchi et al 2008)
library(data.table)
library(reshape)
library(reshape2)
library(foreach)

setwd("C:/Users/jmaburto/Documents/GitHub/e_0-and-eta--long-run-relationship")

# Loading data and calculating lbar ---------------------------------------


#source("R/1_GetHMDData.R") # Just in case you want updated HMD Data

load("Data/HMD_Data.RData")

# Upload useful functions

source("R/Functions_1.R")

# Restrict data after 1900
#Be careful with the missing years of Belgium
# exclude France and E&W civilian populations
Data <- HMDL[HMDL$Year >= 1900 & (HMDL$PopName != "BEL" & HMDL$PopName !="FRACNP" & HMDL$PopName != "GBRCENW"),]
# Get data for belgium in consecutive years
Bel <- HMDL[HMDL$Year >= 1919 & HMDL$PopName=="BEL",]
Data <- data.table(rbind(Data,Bel))

# Example of decomposition for life expectancy and lbar -------------------

#An example
#Decomposition of life expetancy
lt1        <- subset(Data, Year==1960 & PopName=="CZE" & Sex=="m")
lt2        <- subset(Data, Year==2014 & PopName=="CZE" & Sex=="m")

LifeExpectancy(lt1$mx,sex="m")
LifeExpectancy(lt2$mx,sex="m")

## Difference
LifeExpectancy(lt2$mx,sex="m")-LifeExpectancy(lt1$mx,sex="m")

## Example decomposition
Example.le <- Decomp(func=LifeExpectancy, rates1=lt1$mx , rates2=lt2$mx, N=100,sex="m")
plot(Example.le)
## Error
sum(Example.le) - (LifeExpectancy(lt2$mx,sex="m")-LifeExpectancy(lt1$mx,sex="m"))

#Decomposition of lbar
eta.frommx(lt1$mx,sex="m")
eta.frommx(lt2$mx,sex="m")
Example.lb <- Decomp(func=eta.frommx, rates1=lt1$mx , rates2=lt2$mx, N=100,sex="m")

plot(Example.lb)
## Error
sum(Example.lb) - (eta.frommx(lt2$mx,sex="m")-eta.frommx(lt1$mx,sex="m"))

# Decomposing by single age results and first differences -----------------
gdata::keep(HMDL,Data,sure=T)
source("R/Functions_1.R")


Data <- Data[,c('PopName','Sex','Year','Age','mx')]
Data <- Data[with(Data,order(PopName,Sex,Year,Age)),]
nms  <- unique(Data$PopName)
#i <- "RUS"
#j <- "f"
#k <- 1
# trying with parallel programming

library(foreach)
library(doParallel)
cl<-makeCluster(4)
registerDoParallel(cl)

Decomp.results <- NULL


#start time
strt<-Sys.time()
for (i in nms){
  for(j in c("f","m")){
    D1         <- Data[Data$PopName==i & Data$Sex == j,]
    mat        <- acast(D1, Age~Year, value.var="mx")  
    dime       <- dim(mat)[2]
    e0.decomp  <- foreach(i=1:(dim(mat)[2]-1)) %dopar% {Decomp(func=LifeExpectancy, rates1=mat[,i] , rates2=mat[,i+1], N=100,sex=j)}
    lb.decomp  <- foreach(i=1:(dim(mat)[2]-1)) %dopar% {Decomp(func=log.G.frommx, rates1=mat[,i] , rates2=mat[,i+1], N=100,sex=j)}
    leq.decomp <- foreach(i=1:(dim(mat)[2]-1)) %dopar% {Decomp(func=eta.frommx, rates1=mat[,i] , rates2=mat[,i+1], N=100,sex=j)}
    cv.decomp  <- foreach(i=1:(dim(mat)[2]-1)) %dopar% {Decomp(func=my.cv.frommx, rates1=mat[,i] , rates2=mat[,i+1], N=100,sex=j)}
  #Now unlist decomp
    e0         <- do.call(rbind, lapply(e0.decomp, data.frame, stringsAsFactors=FALSE))
    dime2      <- dim(e0)[1]
    lb         <- do.call(rbind, lapply(lb.decomp, data.frame, stringsAsFactors=FALSE))
    leq        <- do.call(rbind, lapply(leq.decomp, data.frame, stringsAsFactors=FALSE))
    cv         <- do.call(rbind, lapply(cv.decomp, data.frame, stringsAsFactors=FALSE))
    
    Dr         <- data.table(cbind(Name=rep(i,4*(dime-1)*111),Age =rep(0:110,(dime-1)*4),
                       measure=c(rep("e0",dime2),rep("log.G",dime2),rep("eta",dime2),rep("CV",dime2)),
                       value=c(e0$X..i..,lb$X..i..,leq$X..i..,cv$X..i..),Sex=rep(j,4*dime2),
                      Year=rep(rep(colnames(mat)[-dime],each=111),4)),stringsAsFactors=FALSE)
    Decomp.results     <- rbind(Decomp.results,Dr)
    print(i)
  }
}

Decomp.results$Age   <- as.numeric(as.character(Decomp.results$Age))
Decomp.results$value <- as.numeric(as.character(Decomp.results$value))
Decomp.results$Year  <- as.numeric(as.character(Decomp.results$Year))


save(Decomp.results, file = "Data/Decomp_results.Rdata")
print(Sys.time()-strt)
#Time difference of 3.801955 hours

