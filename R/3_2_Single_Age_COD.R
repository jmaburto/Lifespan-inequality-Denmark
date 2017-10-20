# Code to ungroup death counts by cause of death to single ages from age 0 to 110
library(ggplot2)
library(data.table)
library(reshape2)
#library(parallelsugar)

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
Data <- local(get(load('Data/DT_COD.RData')))

source('R/Functions_Rizzi.R')

#unique(Data$Country.name)
#unique(Data$Sex)
#unique(Data$Age)
#unique(Data$Cat)


# get single age COD for everything
Single_COD <- Data[, list(Dx=Single_COD_fun(Dx), Age=0:110), by = list(Country,Country.name,ICD,Year,Sex,Cat)]
save(Single_COD, file = 'Data/Single_COD.RData')
gc()
