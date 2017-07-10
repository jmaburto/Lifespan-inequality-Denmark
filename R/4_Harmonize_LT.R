#Program to read excel files from lifetables converted from PDF
#The aim is to harmonize everything we have and be able to compare life expectancy
# and probabilites
library(data.table)
library(reshape2)
library(latticeExtra)
library(XLConnect)


Your_working_directory <- 'C:/Users/jmaburto/Documents/GitHub/CoD-burden-on-LA'

setwd(Your_working_directory)

#load useful function I created to convert excel to dataframes
source("R/Functions.R")

# CEPAL lifetables (2004 & 2010) ------------------------------------------
# gender, 1 = Male, 2 = Female
# read excel files from lifetables cjecked by Aakash
df_CEPAL2004     <- loadWorkbook("Data/Lifetables CEPAL 2004.xlsx")
df_CEPAL2010     <- loadWorkbook("Data/Lifetables CEPAL 2010.xlsx")
# get number of sheets
numberofsheets04 <- length(getSheets(df_CEPAL2004))
numberofsheets10 <- length(getSheets(df_CEPAL2010))

CEPAL2004 <- do.call(rbind,
                     lapply(1:numberofsheets04, 
                            FUN = excel.to.R, 
                            wb = df_CEPAL2004,
                            t = 'CEPAL2004'))

CEPAL2010 <- do.call(rbind,
                     lapply(1:numberofsheets10, 
                            FUN = excel.to.R, 
                            wb = df_CEPAL2010,
                            t = 'CEPAL2010'))

CEPAL_LT <- data.table(rbind(CEPAL2004,CEPAL2010))

table(CEPAL2004$Country,CEPAL2004$Year)
unique(CEPAL2004$Edad.Age)
table(CEPAL2010$Country,CEPAL2010$Year)
unique(CEPAL2010$Edad.Age)
# I need to create a reference year variable, the midpoint of the interval
ini.y             <- as.numeric(substr(CEPAL_LT$Year, 1, 4))
fin.y             <- as.numeric(substr(CEPAL_LT$Year, 6, 9))
CEPAL_LT$ref.year <- (fin.y + ini.y)/2
CEPAL_LT$Country  <- toupper(CEPAL_LT$Country)
CEPAL_LT$Edad.Age <- as.numeric(CEPAL_LT$Edad.Age)
CEPAL_LT$n        <- as.numeric(CEPAL_LT$n)
gdata::keep(CEPAL_LT,excel.to.R,excel.to.R2,Country.code.vec,Country.name.vec, sure=T)

#Harmonize names 
CEPAL_LT[CEPAL_LT$Country=="PLURINATIONAL STATE OF BOLIVIA"]$Country <- Country.name.vec[22]

# add code accordingly
names(Country.code.vec)   <- Country.name.vec
CEPAL_LT$code             <- Country.code.vec[as.character(CEPAL_LT$Country)]
unique(CEPAL_LT$code)
CEPAL_LT <- CEPAL_LT[CEPAL_LT$Year > 1989]

# UN lifetables  ----------------------------------------------------------
# read files and merge them
UN_female <- read.csv("Data/UN_Female_LT.csv",header = T,stringsAsFactors = F)
UN_female$Sex <- 2
UN_Male   <- read.csv("Data/UN_Male_LT.csv",header = T, stringsAsFactors = F)
UN_Male$Sex <- 1
UN_LT     <- data.table(rbind(UN_female,UN_Male))

#Transform strange symbol to NA
s               <- UN_LT[UN_LT$Age==85]$px[1]
UN_LT[UN_LT==s] <- NA
UN_LT$px        <- as.numeric(UN_LT$px)
UN_LT$qx        <- as.numeric(UN_LT$qx)
UN_LT$Sr        <- as.numeric(UN_LT$Sr)
UN_LT$Country   <- toupper(UN_LT$Country)
gdata::keep(CEPAL_LT,excel.to.R,excel.to.R2,Country.code.vec,Country.name.vec,UN_LT, sure=T)

# Check consistency with our codes, first identify those not in the UN datasaet
unique(UN_LT$Country)
unlist(lapply(1:24,function(x){unique(UN_LT$Country)[which(unique(UN_LT$Country) == Country.name.vec[x])]}))
# 20, 22 and 24 -> 2470 (Venezuela), 2060 (Bolivia) , 9999 (Latin America)
UN_LT[UN_LT$Country == "VENEZUELA (BOLIVARIAN REPUBLIC OF)"]$Country <- Country.name.vec[20]
UN_LT[UN_LT$Country == "BOLIVIA (PLURINATIONAL STATE OF)"]$Country   <- Country.name.vec[22]
UN_LT[UN_LT$Country == "LATIN AMERICA AND THE CARIBBEAN"]$Country    <- Country.name.vec[24]

# just keep those we are interested in
UN_LT <- UN_LT[UN_LT$Country %in% Country.name.vec,]

# I need to create a reference year variable, the midpoint of the interval
ini.y             <- as.numeric(substr(UN_LT$Period, 1, 4))
fin.y             <- as.numeric(substr(UN_LT$Period, 6, 9))
UN_LT$ref.year <- (fin.y + ini.y)/2

# keep info from 1990
UN_LT <- UN_LT[UN_LT$ref.year > 1989,]

# add code accordingly
names(Country.code.vec)   <- Country.name.vec
UN_LT$code                <- Country.code.vec[as.character(UN_LT$Country)]
UN_LT$Source <- "UN"
unique(UN_LT$code)

gdata::keep(CEPAL_LT,excel.to.R,excel.to.R2,Country.code.vec,Country.name.vec,UN_LT, sure=T)


table(UN_LT$Country,UN_LT$Period)
unique(UN_LT$Age)

# Lambda lifetables  ----------------------------------------------------------
Lambda_LT         <- read.csv("Data/LAMbDA_LF.csv",header = T,stringsAsFactors = F)
Lambda_LT         <- data.table(Lambda_LT)
Lambda_LT         <- Lambda_LT[Lambda_LT$Year > 1989,]
Lambda_LT[Lambda_LT$Sex == 'M',]$Sex <- 1
Lambda_LT[Lambda_LT$Sex == 'F',]$Sex <- 2
Lambda_LT$Sex     <- as.numeric(Lambda_LT$Sex)
Lambda_LT$Country <- toupper(Lambda_LT$Country)
Lambda_LT$code    <- Country.code.vec[as.character(Lambda_LT$Country)]
Lambda_LT$Source  <- "Lambda"

table(Lambda_LT$Country,Lambda_LT$Year)
unique(UN_LT$Age)

gdata::keep(CEPAL_LT,excel.to.R,excel.to.R2,Country.code.vec,Country.name.vec,UN_LT,Lambda_LT, sure=T)


### Merge all datasets together
names.1 <- c('Year','Period','Age','Sex','Country','Code','Source','mx','qx','ex')
DF1     <- CEPAL_LT[,c('ref.year','Year','Edad.Age','Gender','Country','code','Source','m.x.n.','q.x.n.','e.x.')]
DF2     <- UN_LT[,c('ref.year','Period','Age','Sex','Country','code','Source','mx','qx','ex')]
DF3     <- Lambda_LT[,c('Year','Year','Age','Sex','Country','code','Source','mx','qx','ex')]
colnames(DF1) <- names.1
colnames(DF2) <- names.1
colnames(DF3) <- names.1

Data.LT <- rbind(DF1,DF2,DF3)
Data.LT <- Data.LT[Data.LT$Year <= 2015,]


gdata::keep(CEPAL_LT,excel.to.R,excel.to.R2,Country.code.vec,Country.name.vec,UN_LT,Lambda_LT,Data.LT, sure=T)

# Statistical offices lifetables  ----------------------------------------------------------

# gender, 1 = Male, 2 = Female
# read excel files from lifetables cjecked by Aakash
df_SO1    <- loadWorkbook("Data/Lifetables SOfromPDF.xlsx")
df_SO2    <- loadWorkbook("Data/Lifetables SOfromExc.xlsx")
# get number of sheets
numberofsheetsSO1 <- length(getSheets(df_SO1))
numberofsheetsSO2 <- length(getSheets(df_SO2))

SO1 <- do.call(rbind,
                     lapply(1:numberofsheetsSO1, 
                            FUN = excel.to.R2, 
                            wb = df_SO1,
                            t = 'SO'))

SO2 <- do.call(rbind,
                     lapply(1:numberofsheetsSO2, 
                            FUN = excel.to.R2, 
                            wb = df_SO2,
                            t = 'SO'))

SO1$Year <- as.numeric(SO1$Year)
SO2$Year <- as.numeric(SO2$Year)
SO1$mx   <- as.numeric(SO1$mx)
SO1$qx   <- as.numeric(SO1$qx)
SO1$ex   <- as.numeric(SO1$ex)
SO2$mx   <- as.numeric(SO2$mx)

SO_LT   <- data.table(rbind(SO1,SO2))
Data.LT <- rbind(Data.LT,SO_LT)

gdata::keep(CEPAL_LT,excel.to.R,excel.to.R2,Country.code.vec,Country.name.vec,UN_LT,Lambda_LT,Data.LT,SO_LT, sure=T)

df_Mex    <- loadWorkbook("Data/Lifetables SOfromMex.xlsx")

# get number of sheets
numberofsheetsMex <- length(getSheets(df_Mex))

Mex <- do.call(rbind,
               lapply(1:numberofsheetsMex, 
                      FUN = excel.to.R2, 
                      wb = df_Mex,
                      t = 'SO'))

Mex$mx <- as.numeric(Mex$mx)
Mex$qx <- as.numeric(Mex$qx)

Data.LT <- rbind(Data.LT,Mex)

#load useful function I created to convert excel to dataframes
source("R/Functions_LT.R")

# Data with life expectancy -----------------------------------------------

ex     <- Data.LT[Data.LT$Age==0,]
ex$Sex <- as.numeric(ex$Sex)
ex$Sex <- factor(ex$Sex,levels = c(1, 2), labels = c("Males", "Females"))
ex <- as.data.frame(ex)
save(ex,file= "R/ex_App/Life_expectancy.RData")


# make life expectancy for all the countries in the same way
Data.LT <- Data.LT[Data.LT$Source != 'SO',]
Data.LT <- Data.LT[,ex:=e0.from.mx(mx,Age,Sex), by = list(Year,Sex,Country,Source)]
head(Data.LT)


save(Data.LT, file = 'Outcomes/Data_Lifetables.RData')

