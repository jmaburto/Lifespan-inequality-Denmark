library(ggplot2)
library(data.table)
library(reshape2)

setwd("C:/Users/jmaburto/Documents/GitHub/Lifespan-inequality-Denmark")
# 1. 'Infectious, non-R',
# 2. 'Cancer AS',
# 3. 'Cancer NAS',
# 4. 'Diabetes',
# 5. 'Cardiovascular',
# 6. 'Respiratory I',
# 7. 'Respiratory NI',
# 8. 'External',
# 9. 'Other'
#

#load data
HMD       <- get(load('Data/HMD_Data.RData'))
Countries <- c('DNK', 'SWE', 'NOR')
Data      <- HMD[HMD$PopName %in% Countries,]
Data$Country.name <- Data$PopName
Data      <- Data[,c(1,12,14,2,3)]
Data      <- Data[Data$Year >= 1960 & Data$Year <= 2014,]

gdata::keep(Data,Countries,sure=T)
COD       <- local(get(load('Data/Single_COD.RData')))
COD       <- COD[COD$Year >= 1960 & COD$Year <= 2014,]
COD$Sex   <- as.character(COD$Sex)
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
countries2 <- unique(mx.COD.cast$Country.name)[1:2]
ages      <- sort(unique(mx.COD.cast$Age))
causes    <- colnames(mx.COD.cast)[5:13]
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



source('R/Functions_1.R')
library(parallelsugar)

Decomp.results.list   <- list()
country.list          <- list()
# i <-'f'
# j <- 'DNK'
# years <- 2014
  
  for (i in sexes){
    print(i)
    x <- Mat.list[[as.character(i)]]
    for (j in countries2){
      print(j)
      y <- x[[as.character(j)]]
      z <- x[['SWE']]
      
      Decomp.list <- mclapply(years,function(yr,y,z,e0frommxc,i){
        contrib           <- Decomp(func = e0frommxc,
                                      rates1 = c(y[[as.character(yr)]]),
                                      rates2 = c(z[[as.character(yr)]]),N = 50,sex = i)
        dim(contrib)      <- dim(y[[as.character(yr)]])
        dimnames(contrib) <- dimnames(y[[as.character(yr)]])
        contrib },y= y,z=z,e0frommxc = e0frommxc, i = i, mc.cores = 4)
      names(Decomp.list)  <- years
      country.list[[as.character(j)]] <- Decomp.list
    }
    Decomp.results.list[[as.character(i)]]  <- country.list
  }

save(Decomp.results.list, file =  'Data/Compare_DecompResults_ex_List.RData')
gc()
# bruger   system forløbet 
# 44.07    60.37  6009.63 




Decomp.results.cv <- list()
country.list          <- list()

  for (i in sexes){
    print(i)
    x <- Mat.list[[as.character(i)]]
    for (j in countries2){
      print(j)
      y <- x[[as.character(j)]]
      z <- x[['SWE']]
      
      Decomp.list <- mclapply(years,function(yr,y,z,cvfrommxc,i){
        
        contrib           <- Decomp(func = cvfrommxc,
                                      rates1 = c(y[[as.character(yr)]]),
                                      rates2 = c(z[[as.character(yr)]]),N = 50,sex = i)
        dim(contrib)      <- dim(y[[as.character(yr)]])
        dimnames(contrib) <- dimnames(y[[as.character(yr)]])
        contrib },y= y,z=z,cvfrommxc = cvfrommxc, i = i, mc.cores = 4)
      names(Decomp.list)  <- years
      country.list[[as.character(j)]] <- Decomp.list
    }
    Decomp.results.cv[[as.character(i)]]  <- country.list
  }

save(Decomp.results.cv, file =  'Data/Compare_DecompResults_cv_List.RData')
gc()


sum(Decomp.list[[1]])
