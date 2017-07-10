###############################################################################
### Program to convert to proportion and analyze cause-specific trends        #
###############################################################################

# Classification of causes of death
# 1 total deaths
# 2 Certain infectious and parasitic diseases     A00-B99
# 3 Neoplasms                                     C00-D48
# 4 Diseases of the circulatory system            I00-I99
# 5 Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified                                                                             R00-R99
# 6 Mental and behavioural disorders F01-F99
# 7 Diseases of the nervous system            G00-G98
# 8 Endocrine, nutritional and metabolic diseases     E00-E88 
# 9 Diseases of the digestive system K00-K92  
# 10 Diseases of the genitourinary system               N00-N98
# 11 P00-P96          Certain conditions originating in the perinatal period & Q00-Q99    Congenital malformations, deformations and chromosomal abnormalities
# 12 Diseases of the respiratory system    J00-J98
# 13 External causes of morbidity and mortality     V01-Y89 minus homicide
# 14 X85-Y09 Assault - homicide
# 15 rest of causes


library(data.table)
library(reshape2)
library(latticeExtra)

setwd('C:/Users/jmaburto/Documents/GitHub/CoD-burden-on-LA')

source('R/1_Get_All_Data.R')

cause.name.vec <- c('Total', 'Infectious','Neoplasms', 'Circulatory','Abnormal', 'Mental',
                    'Nervous','Endocrine','Digestive',
                    'Genitourinary','Perinatal','Respiratory','External','Homicide','Rest')

Deaths.data$Cause.name <- Deaths.data$Cause
Deaths.data$Cause.name <- factor(Deaths.data$Cause.name, levels = 1:15, labels = cause.name.vec)
Deaths.data            <- Deaths.data[Deaths.data$Cause > 1,]
Deaths.data            <- Deaths.data[Deaths.data$Age2 < 96]
unique(Deaths.data$Age2)
unique(Deaths.data$Cause.name)
unique(Deaths.data$Country)
### First create same age groups as in the lifetable information, all sources used (UN, CEPAL and Lambda)
### CEPAL 2004 is 80+
### CEPAL 2010 is 100+
### UN is 85+
### Lambda is 85+

##because there might some years that CoD data closes at 85+ and oders 95+, 
##I am closing everything at 85+, after 85+ everything will be accumulated in cause of death 15
##for CEPAL2004, everything will close at 80+

names(Country.code.vec) <- Country.name.vec

DD <- Deaths.data
unique(DD$Age)
unique(DD$Age2)
DD[DD$Age2>=80]$Age2 <- 80
DD[DD$Age=="80 - 84" | DD$Age=="85 - 89"|DD$Age=='80-84'|DD$Age=="90 - 94" | DD$Age=="95+"]$Age <- "80+"

CEPAL2004 <- DD[,list(Female=sum(Female)),by = list(X,Year,Cause,Age,Country,Age2,Cause.name)]
sum(CEPAL2004$Female) - sum(DD$Female)
CEPAL2004$Male <- DD[,list(Male=sum(Male)),by = list(X,Year,Cause,Age,Country,Age2,Cause.name)]$Male
sum(CEPAL2004$Male) - sum(DD$Male)
unique(CEPAL2004$Age)
## Now for the rest, lets close CoD data at 85
DD <- Deaths.data
unique(DD$Age)
unique(DD$Age2)
DD[DD$Age2>=85]$Age2 <- 85
DD[DD$Age=="85 - 89"|DD$Age=='80-84'|DD$Age=="90 - 94" | DD$Age=="95+"]$Age <- "85+"

Rest <- DD[,list(Female=sum(Female)),by = list(X,Year,Cause,Age,Country,Age2,Cause.name)]
sum(Rest$Female) - sum(DD$Female)
Rest$Male <- DD[,list(Male=sum(Male)),by = list(X,Year,Cause,Age,Country,Age2,Cause.name)]$Male
sum(Rest$Male) - sum(DD$Male)
unique(Rest$Age)
### New CoD dataset 
CEPAL2004$Source <- 'CEPAL2004'
Rest$Source      <- 'Rest'

#Deaths.data <- rbind(CEPAL2004,Rest)

table(Deaths.data$Country,Deaths.data$Year)



#### aggregate ages 1-4
CEPAL2004[CEPAL2004$Age2 >= 1 & CEPAL2004$Age2 <= 4,]$Age2 <- 1
CEPAL2004[CEPAL2004$Age2==1,]$Age <- '1-4'
DD <- CEPAL2004[,list(Female=sum(Female)), by = list(X,Year,Cause,Age,Country,Age2,Cause.name,Source)]
DD$Male <- CEPAL2004[,list(Male=sum(Male)), by = list(X,Year,Cause,Age,Country,Age2,Cause.name,Source)]$Male
CEPAL2004 <- DD


Rest[Rest$Age2 >= 1 & Rest$Age2 <= 4,]$Age2 <- 1
Rest[Rest$Age2==1,]$Age <- '1-4'
DD <- Rest[,list(Female=sum(Female)), by = list(X,Year,Cause,Age,Country,Age2,Cause.name,Source)]
DD$Male <- Rest[,list(Male=sum(Male)), by = list(X,Year,Cause,Age,Country,Age2,Cause.name,Source)]$Male
Rest <- DD

save(Deaths.data,CEPAL2004,Rest,file = 'Outcomes/Harmonized_CoDData.RData')

gdata::keep(Deaths.data,Rest,CEPAL2004,cause.name.vec,Country.code.vec,Country.name.vec,sure = T)
