###############################################################################
### Get CoD data for Denmark, Sweden and Norway. Source: WHO (07/07/2017)     #
###############################################################################

# Classification of causes of death (see paper)
# put all the folders in one and set your working directory to this master folder
Your_working_directory <- "C:/Users/jmaburto/Documents/GitHub/Lifespan-inequality-in-Denmark"
library(data.table)
library(reshape2)

setwd(Your_working_directory)
# Create a vector with the countries we are interested in
Country.name.vec <- c('Denmark','Norway','Sweden')

# Create a vector with the countries' codes according to WHO
Country.code.vec <- c(4050,4220,4290)

# Information for ICD7
ICD7    <- data.table(read.table(  file = 'Data/WHO Data/MortIcd7.txt',header = T,sep = ',',stringsAsFactors = F))
ICD7    <- ICD7[(ICD7$Country==4050 | ICD7$Country==4290 | ICD7$Country==4220) & ICD7$Year >= 1960,]
ICD7$ICD<- 7
# Information for ICD8
ICD8    <- data.table(read.table(  file = 'Data/WHO Data/MortIcd8.txt',header = T,sep = ',',stringsAsFactors = F))
ICD8    <- ICD8[(ICD8$Country==4050 | ICD8$Country==4290 | ICD8$Country==4220) & ICD8$Year >= 1960,]
ICD8$ICD<- 8
# Information for ICD9
ICD9    <- data.table(read.table(  file = 'Data/WHO Data/MortIcd9.txt',header = T,sep = ',',stringsAsFactors = F))
ICD9    <- ICD9[(ICD9$Country==4050 | ICD9$Country==4290 | ICD9$Country==4220) & ICD9$Year >= 1960,]
ICD9$ICD<- 9
# Information for ICD10
ICD10_1 <- data.table(read.table(  file = 'Data/WHO Data/MortIcd10_part1.txt',header = T,sep = ',',stringsAsFactors = F))
ICD10_2 <- data.table(read.table(  file = 'Data/WHO Data/MortIcd10_part2.txt',header = T,sep = ',',stringsAsFactors = F))
ICD10   <- rbind(ICD10_1,ICD10_2)
ICD10   <- ICD10[(ICD10$Country==4050 | ICD10$Country==4290 | ICD10$Country==4220) & ICD10$Year >= 1960,]
ICD10$ICD<- 10
# No we have information on cause of death for Sweden, Norway and Denmark. Just anticipating in case we do any,
# comparison. But now just focusing on Denmark
COD_Data <- rbind(ICD7,ICD8,ICD9,ICD10)
COD_Data$Country.name <- as.factor(COD_Data$Country)
levels(COD_Data$Country.name) <- Country.name.vec

COD_Data <- COD_Data[COD_Data$Country == 4050,]
# sex 1 is male, 2 is female, 9 is unspecified
COD_Data <- COD_Data[COD_Data$Sex != 9,]
COD_Data$Sex <- as.factor(COD_Data$Sex)
levels(COD_Data$Sex) <- c('m', 'f')

COD_Data <- COD_Data[,c(1,41,40,4:39)]
save(COD_Data, file = 'Data/COD_Data.RData')

gdata::keep(COD_Data, sure = T)

