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

Decomp.sd <- local(get(load("Data/DecompResults_sd_List.RData")))
Compare.sd <- local(get(load("Data/Compare_DecompResults_sd_List.RData")))

source('R/Sensitivity Analysis/Functions_Sensitivity.R')

#get results for decomp in ed un a data.table
DT.Decomp.sd <- do.call(rbind,lapply(1:2,function(j,y){
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
y=Decomp.sd))
#warnings()


DT.compare.sd <- do.call(rbind,lapply(1:2,function(j,y){
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
y=Compare.sd))

names(DT.Decomp.sd) <- c('Sex','Country','Year','Age',Category.labels)
names(DT.compare.sd) <- c('Sex','Country','Year','Age',Category.labels)

DT.Decomp.sd <- melt(DT.Decomp.sd,id.vars = c('Sex','Country','Year','Age'),value.name = 'Contribution',variable.name = 'Cause')
DT.compare.sd <- melt(DT.compare.sd,id.vars = c('Sex','Country','Year','Age'),value.name = 'Contribution',variable.name = 'Cause')

DT.Decomp.sd[DT.Decomp.sd$Age >= 85,]$Cause   <- 'Above age 85'
DT.compare.sd[DT.compare.sd$Age >= 85,]$Cause <- 'Above age 85'

DT.compare.sd$Country <- as.factor(DT.compare.sd$Country)
levels(DT.compare.sd$Country) <- c('Denmark','Sweden')

DT.Decomp.sd$Country <- as.factor(DT.Decomp.sd$Country)
levels(DT.Decomp.sd$Country) <- c('Denmark', 'Sweden')

DT.Decomp.sd$Sex <- as.factor(DT.Decomp.sd$Sex)
levels(DT.Decomp.sd$Sex) <- c('Female', 'Male')

DT.compare.sd$Sex <- as.factor(DT.compare.sd$Sex)
levels(DT.compare.sd$Sex) <- c('Female', 'Male')

single.sd <- DT.Decomp.sd

Labels.age            <- c('0','1-4', '5-9', '10-14', '15-19', '20-24','25-29','30-34','35-39',
                           '40-44','45-49','50-54','55-59','60-64','65-69',
                           "70-74","75-79","80-84","85-89","90-94","95-99","100-104","105-109",'110+')


DT.Decomp.sd$Age2       <- (cut(DT.Decomp.sd$Age+1, breaks=c(0,1,seq(5,110,5),Inf),labels=Labels.age))
DT.Decomp.sd            <- DT.Decomp.sd[,list(Contribution = sum(Contribution)), by = list(Sex,Country,Year,Cause,Age2)]
names(DT.Decomp.sd)     <- c("Sex","Country","Year","Cause","Age" ,"Contribution")

DT.compare.sd$Age2       <- (cut(DT.compare.sd$Age+1, breaks=c(0,1,seq(5,110,5),Inf),labels=Labels.age))
DT.compare.sd            <- DT.compare.sd[,list(Contribution = sum(Contribution)), by = list(Sex,Country,Year,Cause,Age2)]
names(DT.compare.sd)     <- c("Sex","Country","Year","Cause","Age" ,"Contribution")

save(DT.Decomp.sd,DT.compare.sd,file='Data/Sensitivity_Results.RData')


