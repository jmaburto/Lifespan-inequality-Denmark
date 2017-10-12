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

Decomp.ex <- local(get(load("Data/DecompResults_ex_List.RData")))
Decomp.cv <- local(get(load("Data/DecompResults_cv_List.RData")))

Compare.ex <- local(get(load("Data/Compare_DecompResults_ex_List.RData")))
Compare.cv <- local(get(load("Data/Compare_DecompResults_cv_List.RData")))



source('R/Functions_1.R')

#get results for decomp in ex un a data.table
DT.Decomp.ex <- do.call(rbind,lapply(1:2,function(j,y){
  x <- y[[j]]
  names(x) <- 1:3
  #"DNK" "NOR" "SWE"
  w <- do.call(rbind,
               lapply(names(x), 
                      FUN = my_reshape.function, 
                      DecompIn = x))
  W <- cbind(sex = as.integer(j),w)
  W
},
y=Decomp.ex))
#warnings()

#get results for decomp in ed un a data.table
DT.Decomp.cv <- do.call(rbind,lapply(1:2,function(j,y){
  x <- y[[j]]
  names(x) <- 1:3
  #"DNK" "NOR" "SWE"
  w <- do.call(rbind,
               lapply(names(x), 
                      FUN = my_reshape.function, 
                      DecompIn = x))
  W <- cbind(sex = as.integer(j),w)
  W
},
y=Decomp.cv))
#warnings()


DT.compare.ex <- do.call(rbind,lapply(1:2,function(j,y){
  x <- y[[j]]
  names(x) <- 1:2
  #"DNK" "NOR" "SWE"
  w <- do.call(rbind,
               lapply(names(x), 
                      FUN = my_reshape.function, 
                      DecompIn = x))
  W <- cbind(sex = as.integer(j),w)
  W
},
y=Compare.ex))


DT.compare.cv <- do.call(rbind,lapply(1:2,function(j,y){
  x <- y[[j]]
  names(x) <- 1:2
  #"DNK" "NOR" "SWE"
  w <- do.call(rbind,
               lapply(names(x), 
                      FUN = my_reshape.function, 
                      DecompIn = x))
  W <- cbind(sex = as.integer(j),w)
  W
},
y=Compare.cv))



#DT       <- as.data.table(melt(D, 
#                               id.vars = list("state","year","age"),
#                               variable.name = "Cause"))

source('R/5_LifeExpectancy&LifespanInequality.R')
gdata::keep(DT.Decomp.ex,DT.Decomp.cv,Data,DT.compare.cv,DT.compare.ex,sure = T)
source('R/Functions_1.R')
Data$Country <- as.factor(Data$PopName)
levels(Data$Country) <- c('Denmark', 'Norway', 'Sweden')

names(DT.Decomp.cv) <- c('Sex','Country','Year','Age',Category.labels)
names(DT.Decomp.ex) <- c('Sex','Country','Year','Age',Category.labels)
names(DT.compare.cv) <- c('Sex','Country','Year','Age',Category.labels)
names(DT.compare.ex) <- c('Sex','Country','Year','Age',Category.labels)

DT.Decomp.ex <- melt(DT.Decomp.ex,id.vars = c('Sex','Country','Year','Age'),value.name = 'Contribution',variable.name = 'Cause')
DT.Decomp.cv <- melt(DT.Decomp.cv,id.vars = c('Sex','Country','Year','Age'),value.name = 'Contribution',variable.name = 'Cause')
DT.compare.ex <- melt(DT.compare.ex,id.vars = c('Sex','Country','Year','Age'),value.name = 'Contribution',variable.name = 'Cause')
DT.compare.cv <- melt(DT.compare.cv,id.vars = c('Sex','Country','Year','Age'),value.name = 'Contribution',variable.name = 'Cause')

DT.compare.ex$Country <- as.factor(DT.compare.ex$Country)
DT.compare.cv$Country <- as.factor(DT.compare.cv$Country)
levels(DT.compare.ex$Country) <- c('Denmark', 'Norway')
levels(DT.compare.cv$Country) <- c('Denmark', 'Norway')

DT.Decomp.ex$Country <- as.factor(DT.Decomp.ex$Country)
DT.Decomp.cv$Country <- as.factor(DT.Decomp.cv$Country)
levels(DT.Decomp.ex$Country) <- c('Denmark', 'Norway', 'Sweden')
levels(DT.Decomp.cv$Country) <- c('Denmark', 'Norway', 'Sweden')


DT.Decomp.ex$Sex <- as.factor(DT.Decomp.ex$Sex)
DT.Decomp.cv$Sex <- as.factor(DT.Decomp.cv$Sex)
levels(DT.Decomp.ex$Sex) <- c('Female', 'Male')
levels(DT.Decomp.cv$Sex) <- c('Female', 'Male')


DT.compare.ex$Sex <- as.factor(DT.compare.ex$Sex)
DT.compare.cv$Sex <- as.factor(DT.compare.cv$Sex)
levels(DT.compare.ex$Sex) <- c('Female', 'Male')
levels(DT.compare.cv$Sex) <- c('Female', 'Male')


single.ex <- DT.Decomp.ex
single.cv <- DT.Decomp.cv

Labels.age            <- c('0-4', '5-9', '10-14', '15-19', '20-24','25-29','30-34','35-39',
                           '40-44','45-49','50-54','55-59','60-64','65-69',
                           "70-74","75-79","80-84","85-89","90-94","95-99","100-104","105-109",'110+')

DT.Decomp.ex$Age2       <- (cut(DT.Decomp.ex$Age+1, breaks=c(seq(0,110,5),Inf),labels=Labels.age))
DT.Decomp.cv$Age2       <- (cut(DT.Decomp.cv$Age+1, breaks=c(seq(0,110,5),Inf),labels=Labels.age))
DT.Decomp.ex            <- DT.Decomp.ex[,list(Contribution = sum(Contribution)), by = list(Sex,Country,Year,Cause,Age2)]
DT.Decomp.cv            <- DT.Decomp.cv[,list(Contribution = sum(Contribution)), by = list(Sex,Country,Year,Cause,Age2)]
names(DT.Decomp.ex)     <- c("Sex","Country","Year","Cause","Age" ,"Contribution")
names(DT.Decomp.cv)     <- c("Sex","Country","Year","Cause","Age" ,"Contribution")

DT.compare.ex$Age2       <- (cut(DT.compare.ex$Age+1, breaks=c(seq(0,110,5),Inf),labels=Labels.age))
DT.compare.cv$Age2       <- (cut(DT.compare.cv$Age+1, breaks=c(seq(0,110,5),Inf),labels=Labels.age))
DT.compare.ex            <- DT.compare.ex[,list(Contribution = sum(Contribution)), by = list(Sex,Country,Year,Cause,Age2)]
DT.compare.cv            <- DT.compare.cv[,list(Contribution = sum(Contribution)), by = list(Sex,Country,Year,Cause,Age2)]
names(DT.compare.ex)     <- c("Sex","Country","Year","Cause","Age" ,"Contribution")
names(DT.compare.cv)     <- c("Sex","Country","Year","Cause","Age" ,"Contribution")



save(Data,DT.Decomp.cv, DT.Decomp.ex,DT.compare.cv,DT.compare.ex, file='Data/Results.RData')
save(Data,DT.Decomp.cv, DT.Decomp.ex,single.ex,single.cv,DT.compare.cv,DT.compare.ex, file='R/DK_App/Results.RData')


