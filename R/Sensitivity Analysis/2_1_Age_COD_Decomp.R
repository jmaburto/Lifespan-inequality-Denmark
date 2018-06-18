library(ggplot2)
library(data.table)
library(reshape2)

setwd("C:/Users/jmaburto/Documents/GitHub/Lifespan-inequality-Denmark")

#2  Cancer, amenable to smoking
#3  Cancer, not amenable to smoking
#5  Cardiovascular & Diabetes mellitus (move to 5)
#6  Respiratory, infectious
#7  Respiratory, non-infectious
#8  External
#9  Other & Infectious, non-respiratory



#load data
HMD       <- get(load('Data/HMD_Data.RData'))
Countries <- c('DNK', 'SWE')
Data      <- HMD[HMD$PopName %in% Countries,]
Data$Country.name <- Data$PopName
Data      <- Data[,c(1,12,14,2,3)]
Data      <- Data[Data$Year >= 1960 & Data$Year <= 2014,]

gdata::keep(Data,Countries,sure=T)
COD       <- local(get(load('Data/Single_COD.RData')))
COD$Country.name <- as.character(COD$Country.name)
COD       <- COD[COD$Year >= 1960 & COD$Year <= 2014,]
COD       <- COD[COD$Country.name %in% c('Denmark', 'Sweden'),]
COD$Sex   <- as.character(COD$Sex)
COD$Country.name <- as.factor(COD$Country.name)
levels(COD$Country.name) <- Countries
COD$Country.name         <- as.character(COD$Country.name)

#get proportions by age
COD  <- COD[, Dx.p:= Dx/sum(Dx), by= list(Country, Country.name, ICD, Year, Sex,Age)]
COD[is.na(Dx.p),]$Dx.p <- 0
COD       <- COD[,c(4,5,2,6,8,9)]
COD.cast <- dcast(COD,Year+Sex+Country.name+Age ~ Cat,value.var = 'Dx.p')


#Merge datsets
mx.COD        <- merge(COD,Data,by = c('Year','Sex','Country.name','Age'), all = T)
mx.COD$mx.COD <- mx.COD$Dx.p * mx.COD$mx 
mx.COD        <- mx.COD[order(Country.name, Year, Sex, Cat, Age),]
mx.COD.cast   <- dcast.data.table(mx.COD, Year+Sex+Country.name+Age ~ Cat, value.var = 'mx.COD')


# perform decomposition
sexes     <- unique(mx.COD.cast$Sex)
years     <- unique(mx.COD.cast$Year)
countries <- unique(mx.COD.cast$Country.name)
ages      <- sort(unique(mx.COD.cast$Age))
causes    <- colnames(mx.COD.cast)[5:11]
Empty     <- matrix(0,nrow = length(ages),ncol = length(causes),dimnames = list(ages,causes))

# get matrices in lists to do faster the calculation
Mat.list <- lapply(sexes, function(sx,LTC2,years,Empty,ages,causes){
  LTC    <- mx.COD.cast[mx.COD.cast$Sex == sx,]
Sex.List <- lapply(countries, function(st,LTC,Empty,years,ages,causes){
  Mat    <- LTC[LTC$Country == st,]
  YRlist <- lapply(years, function(yr,Mat,Empty,causes,ages){
    Mat2           <- as.matrix(Mat[Mat$Year == yr,c(causes), with = F])
    rownames(Mat2) <- ages
    Empty[rownames(Mat2),colnames(Mat2)] <- Mat2
    Empty
  }, Mat = Mat, Empty = Empty, causes = causes, ages= ages)
  names(YRlist) <- years
  YRlist
}, LTC = LTC, years=years, Empty = Empty, causes = causes, ages=ages)
names(Sex.List) <- countries
Sex.List
},LTC2 = mx.COD.cast, years=years, Empty = Empty, causes = causes, ages=ages)
names(Mat.list) <- sexes


library(parallelsugar)
source('R/Sensitivity Analysis/Functions_Sensitivity.R')

Decomp.results.sd <- list()
country.list          <- list()

  for (i in sexes){
    print(i)
    x <- Mat.list[[as.character(i)]]
    for (j in countries){
      print(j)
      y <- x[[as.character(j)]]
      
      Decomp.list <- mclapply(years[-length(years)],function(yr,y,sdfrommxc,i){
        
        contrib           <- Decomp(func = sdfrommxc,
                                      rates1 = c(y[[as.character(yr)]]),
                                      rates2 = c(y[[as.character(yr+1)]]),N = 50,sex = i)
        dim(contrib)      <- dim(y[[as.character(yr)]])
        dimnames(contrib) <- dimnames(y[[as.character(yr)]])
        contrib },y= y,sdfrommxc = sdfrommxc, i = i, mc.cores = 4)
      names(Decomp.list)  <- years[-length(years)]
      country.list[[as.character(j)]] <- Decomp.list
    }
    Decomp.results.sd[[as.character(i)]]  <- country.list
  }

save(Decomp.results.sd, file =  'Data/DecompResults_sd_List.RData')
gc()
# 
# used (Mb) gc trigger  (Mb) max used  (Mb)
# Ncells  955672 51.1    1770749  94.6  1770749  94.6
# Vcells 4792875 36.6   13290227 101.4 21420939 163.5
# 
