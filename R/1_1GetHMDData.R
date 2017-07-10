####### Program for getting most recent data from HMD

library(HMDHFDplus)
library(data.table)
setwd("C:/Users/jmaburto/Documents/GitHub/Lifespan-inequality-in-Denmark")

XYZ <- getHMDcountries()
us <- "jmaburto@colmex.mx"
pw <- "kolmogorov"

# now grab all the lifetables and mesh together..
# grab them all
HMDL <- do.call(rbind,lapply(XYZ, function(x, us, pw){
  cat(x,"\n")
  Males        <- readHMDweb(x,"mltper_1x1",username=us,password=pw)
  Females      <- readHMDweb(x,"fltper_1x1",username=us,password=pw)
  Males$Sex    <- "m"
  Females$Sex  <- "f"
  CTRY         <- rbind(Females, Males)
  CTRY$PopName <- x
  CTRY    
}, us = us, pw = pw))

HMDL <- data.table(HMDL)
save(HMDL,file="Data/HMD_Data.RData")
