###############################################################################
### Get CoD data for Denmark, Sweden and Norway. Source: WHO (07/07/2017)     #
###############################################################################

# Classification of causes of death (see paper)
# 1. Infectious diseases
# 2. Circulatory diseases
# 3. Neoplasms
# 4. External causes
# 5. Other causes

library(data.table)
library(reshape2)

Your_working_directory <- "C:/Users/jmaburto/Documents/GitHub/Lifespan-inequality-in-Denmark"
setwd(Your_working_directory)

# Run this line to update the COD RData file
# source(file = 'R/2_Get_WHO_Data.R')

# Load data from 2_Get_WHO_Data.R
get(load('Data/COD_Data.RData'))

# We have to group causes according to the ICD classification
# for ICD7 we use the list 07A from WHO documentation for all years
# Ask for help to Maarten to classify causes of death following the Janssesn & Kunst paper
# and the WHO documentation.
ICD7 <- COD_Data[COD_Data$ICD == 7,]
unique(ICD7$Cause)



# We have similar age groups for all the years and causes of death and sexes




# Create a vector with the countries we are interested in
Country.name.vec <- c('Denmark','Sweden','Norway')

# Create a vector with the countries' codes according to WHO
Country.code.vec <- c(4050,4290,4220)

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
COD_Data <- COD_Data[COD_Data$Country == 4050,]
# sex 1 is male, 2 is female, 9 is unspecified
COD_Data <- COD_Data[COD_Data$Sex != 9,]
COD_Data <- COD_Data[,c(1,4,5:40)]
COD_Data$Sex <- as.factor(COD_Data$Sex)
levels(COD_Data$Sex) <- c('m', 'f')
save(COD_Data, file = 'Data/COD_Data.RData')


# Frmat refers to the age-group format 
unique(COD_Data$List)
unique(COD_Data$Frmat)
unique(COD_Data$IM_Frmat)



Deaths.data  <- NULL
# Create a loop to find the year of change between ICD9 and ICD10 and create homogeneous datasets
for (i in 1:length(Country.code.vec)){
  
  # read data for each country ICD9 and ICD10
  
  if(Country.name.vec[i] == 'Bolivia' | Country.name.vec[i]=='Haiti'){ ICD9.data <- NULL} else {ICD9.data  <- read.table(paste0('Data/ICD9/ICD9-',Country.code.vec[i],'.txt'),
                          header = T, sep = ',', stringsAsFactors = F)}
  
  ICD10.data <- read.table(paste0('Data/ICD10/ICD10-',Country.code.vec[i],'.txt'),
                          header = T, sep = ',', stringsAsFactors = F)
  
  # Peru also has a year 0 in ICD10.data
  ICD10.data <- ICD10.data[ICD10.data$Year > 0,]
  
  # Year of implementation OF ICD10
  ICD10.y    <- c(ICD10.Year=min(ICD10.data$Year),Code=Country.code.vec[i],Country=Country.name.vec[i])
  ICD10.year <- rbind(ICD10.year,ICD10.y)
  # rbind both datasets
  Deaths     <- rbind(ICD9.data,ICD10.data)
  
  # convert to data.table, it will make thinhk easier for applying functions
  Deaths     <- data.table(Deaths)
  
  # Cuba has a year == 0, restrict to values > 0
  Deaths              <- Deaths[Deaths$Year > 0,]
  
  # add a numeric age, makes thinks easier to order when needed by age
  code.age                     <- unique(Deaths$Age)

  # age2 is coded numerically, 96 is UNK and 97 is total
  code.age2                    <- c(97,0:4,seq(5,95,5),96)
  names(code.age2)             <- code.age
  Deaths$Age2                  <- code.age2[as.character(Deaths$Age)]
  Deaths                       <- Deaths[with(Deaths,order(Year,Age2,Cause)),]
  
  # take care of duplicates in Brazil (IT DID NOT WORK COMPLETELY, SOME STRANGE RESULTS)
  if ( i == 14) { Deaths <- unique(Deaths, by = colnames(Deaths)[1:4]) }
  
  # Complete ages that have no values of causes of death and fill them with 0
  print(unique(Deaths$Cause))
  mat.Deaths.f <- dcast(Deaths, Age2 + Cause ~ Year,value.var = 'Female',fill = 0, drop = F)
  mat.Deaths.m <- dcast(Deaths, Age2 + Cause ~ Year,value.var = 'Male',fill = 0, drop = F)
  
  # now return to original shape
  Deaths.f      <- melt(mat.Deaths.f,id.vars = c('Age2','Cause'),
                       variable.name = 'Year',value.name = 'Female')
  Deaths.m      <- melt(mat.Deaths.m,id.vars = c('Age2','Cause'),
                       variable.name = 'Year',value.name = 'Male')
  Deaths.f$Year <- as.numeric(as.character(Deaths.f$Year))
  Deaths.m$Year <- as.numeric(as.character(Deaths.m$Year))

  
  if (i == 1) {Deaths.cuba1 <- data.table(Deaths.f)
  Deaths.cuba2 <- data.table(Deaths.m)
    females <- Deaths.cuba1[,Complete.cuba(Female), by = list(Age2,Year)]
    males   <- Deaths.cuba2[,Complete.cuba(Male), by = list(Age2,Year)]
  Deaths.f$Female <- females$V1
  Deaths.m$Male <- males$V1
  }
  
  
  # correct cuba, there are inconsistencies with subtotals and totals 
  
  ###Make sure the category 15 containd the sum 2:14 -1
  yrs <- unique(Deaths.f$Year)
  ages <- unique(unique(Deaths.f$Age2))
  #k <- 0
  #j <- 1990
  for (k in ages){
  for (j in yrs){
  s1 <- (sum(Deaths.f[Deaths.f$Age2==k & Deaths.f$Year==j,][2:14,4]) + 
           Deaths.f[Deaths.f$Age2==k & Deaths.f$Year==j,][15,4])
  
  s2 <- s1 - Deaths.f[Deaths.f$Age2==k & Deaths.f$Year==j,][1,4]
  
  s1.1 <- (sum(Deaths.m[Deaths.m$Age2==k & Deaths.m$Year==j,][2:14,4]) + 
           Deaths.m[Deaths.m$Age2==k & Deaths.m$Year==j,][15,4])
  
  s2.1 <- s1.1 - Deaths.m[Deaths.m$Age2==k & Deaths.f$Year==j,][1,4]
  
  if (s2 != 0) print(c(k,s2,j))
  if (s2.1 != 0) print(c(k,s2.1,j))
  
  }}
  
  
  # add the code and name of the country to the dataset
  Deaths.complete         <- Deaths.f
  Deaths.complete$Male    <- Deaths.m$Male
  Deaths.complete$X       <- Country.code.vec[i]
  Deaths.complete$Country <- Country.name.vec[i]
  
  
  # add a the label to the age, for consistency with the original files
  code.age3                     <- c(97,0:4,seq(5,95,5),96)
  # age2 is coded numerically, 96 is UNK and 97 is total
  code.age4                    <- code.age
  names(code.age4)             <- code.age3
  Deaths.complete$Age          <- code.age4[as.character(Deaths.complete$Age2)]
  
  # go back to the original shape
  Deaths.complete              <- Deaths.complete[,c('X','Year','Cause','Age','Female', 'Male', 'Country', 'Age2')]
  Deaths.complete              <- Deaths.complete[with(Deaths.complete,order(Year,Age2,Cause)),]
  
  #store all countries in one dataset
  print(Country.name.vec[i])
  Deaths.data <- data.table(rbind(Deaths.data,Deaths.complete))
}

Deaths.data <- Deaths.data[with(Deaths.data, order(X,Year,Cause,Age2)),]

ICD10.year            <- data.table(ICD10.year)
ICD10.year$ICD10.Year <- as.numeric(ICD10.year$ICD10.Year)
ICD10.year$Code       <- as.numeric(ICD10.year$Code)
ICD10.year <- ICD10.year[with(ICD10.year, order(Code)),]

gdata:: keep (Deaths.data , ICD10.year,Country.name.vec,Country.code.vec, sure = T)

