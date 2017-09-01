###############################################################################
### Get CoD data for Denmark, Sweden and Norway. Source: WHO (07/07/2017)     #


library(data.table)
library(reshape2)

setwd("C:/Users/jmaburto/Documents/GitHub/Lifespan-inequality-Denmark")

# Run this line to update the COD RData file
# source(file = 'R/2_Get_WHO_Data.R')

# Load data from 2_Get_WHO_Data.R
get(load('Data/COD_Data.RData'))

unique(COD_Data$List)

# We have to group causes according to the ICD classification
# for ICD7 we use the list 07A from WHO documentation for all years
# Maarten to classify causes of death following the Janssesn & Kunst paper
# and the WHO documentation.

#Classification made by Marteen (10 categories), if soesn't work regroup to 
# He provided the bridged codes

#1  Infectious, non-respiratory
#2  Cancer, amenable to smoking
#3  Cancer, not amenable to smoking
#4  Diabetes mellitus
#5  Cardiovascular
#6  Respiratory, infectious
#7  Respiratory, non-infectious
#8  External
#9  Other


# ICD 7 classification ----------------------------------------------------


ICD7.data <- COD_Data[COD_Data$ICD==7,]

# Drop those code that do not have an A to avoid duplicates
bad.codes <-   unique(ICD7.data$Cause)[152:length(unique(ICD7.data$Cause))]
bad.codes <- c(bad.codes[!(bad.codes %in% c(155,156,158,159,160,164,165,175,176,178,179,180,181,192,193,194,195,198,199))], 'A057')
ICD7.data <-   ICD7.data[!(ICD7.data$Cause %in% bad.codes),]

# Exclude the cause A000, is all causes in documentation
ICD7.data <- ICD7.data[ICD7.data$Cause != 'A000']

# Bad codes
ICD7.data$Cat <- 10

########## category 1
ICD7.data[ICD7.data$Cause %in% c(paste0('A00',1:9),paste0('A0', 10:43)),]$Cat <- 1

########## category 2
ICD7.data[ICD7.data$Cause %in% c(paste0('A0',44:50),'A052','157'),]$Cat <- 2

########## category 3
ICD7.data[ICD7.data$Cause %in% c(paste0('A0',53:56),'A051','A058','A059',
                                 '155','156','158','159','160','164','165','175','176','178','179',
                                 '180','181','192','193','194','195','198','199'),]$Cat <- 3

########## category 4
ICD7.data[ICD7.data$Cause %in% c('A063'),]$Cat <- 4

########## category 5
ICD7.data[ICD7.data$Cause %in% c('A070',paste0('A0',79:86)),]$Cat <- 5

########## category 6
ICD7.data[ICD7.data$Cause %in% c('A095',paste0('A0',87:92)),]$Cat <- 6

########## category 7
ICD7.data[ICD7.data$Cause %in% c('A093','A094','A096','A097'),]$Cat <- 7

########## category 8
ICD7.data[ICD7.data$Cause %in% c(paste0('A',138:150)),]$Cat <- 8
unique(ICD7.data$Cat)

########## category 9
ICD7.data[ICD7.data$Cause %in% c(paste0('A0',60:62),paste0('A0',64:69),
                                 paste0('A0',71:78),paste0('A0',98:99),
                                 paste0('A',100:137)),]$Cat <- 9
unique(ICD7.data$Cat)


# ICD 8 classification ----------------------------------------------------

ICD8.data    <- COD_Data[COD_Data$ICD==8,]
good.codes8  <- unique(ICD8.data$Cause)[178:327]
good.codes8  <- good.codes8[good.codes8!='A058']
good.codes8  <- c(good.codes8, as.character(c(155,156,158,159,160,163,171,183,184,186:199,157)))
# Drop those code that do not have an A to avoid duplicates
ICD8.data <-   ICD8.data[ICD8.data$Cause %in% good.codes8,]

########## Bad codes
ICD8.data$Cat <- 10


########## category 1
ICD8.data[ICD8.data$Cause %in% c(paste0('A00',1:9),paste0('A0', 10:44)),]$Cat <- 1

########## category 2
ICD8.data[ICD8.data$Cause %in% c(paste0('A0',45:51),'A055','157'),]$Cat <- 2

########## category 3
ICD8.data[ICD8.data$Cause %in% c(paste0('A0',52:54),paste0('A0',56:57),
                                 paste0('A0',59:60),
                                 as.character(c(155,156,158,159,160,163,171,183,184,186:199))),]$Cat <- 3

########## category 4
ICD8.data[ICD8.data$Cause %in% c('A064'),]$Cat <- 4

########## category 5
ICD8.data[ICD8.data$Cause %in% c(paste0('A0',80:88)),]$Cat <- 5

########## category 6
ICD8.data[ICD8.data$Cause %in% c('A095',paste0('A0',89:92)),]$Cat <- 6

########## category 7
ICD8.data[ICD8.data$Cause %in% c('A093','A094','A096'),]$Cat <- 7

########## category 8
ICD8.data[ICD8.data$Cause %in% c(paste0('A',138:150)),]$Cat <- 8


########## Rest causes (category 9)
ICD8.data[ICD8.data$Cause %in% c(paste0('A0',61:63),paste0('A0',65:79),
                                 paste0('A0',97:99),paste0('A',100:137)),]$Cat <- 9

unique(ICD8.data$Cat)



# ICD 9 classification ----------------------------------------------------

ICD9.data    <- COD_Data[COD_Data$ICD==9,]
good.codes9  <- unique(ICD9.data$Cause)[73:392]

# Drop those code that do not have an A to avoid duplicates
ICD9.data <-   ICD9.data[ICD9.data$Cause %in% good.codes9,]

########## Rest of bad codes
ICD9.data$Cat <- 10

########## category 1
ICD9.data[ICD9.data$Cause %in% c(paste0('B0',1:7),paste0('B', 184:185)),]$Cat <- 1

########## category 2
ICD9.data[ICD9.data$Cause %in% c(paste0('B0',90:94),'B08','B096','B100','B101','B120'),]$Cat <- 2

########## category 3
ICD9.data[ICD9.data$Cause %in% c(paste0('B',121:126),paste0('B',13:14),'B095','B099','B109','B11','B129'),]$Cat <- 3

########## category 4
ICD9.data[ICD9.data$Cause %in% c('B181'),]$Cat <- 4

########## category 5
ICD9.data[ICD9.data$Cause %in% c(paste0('B',25:30)),]$Cat <- 5

########## category 6
ICD9.data[ICD9.data$Cause %in% c(paste0('B',310:312),paste0('B',320:322)),]$Cat <- 6

########## category 7
ICD9.data[ICD9.data$Cause %in% c(paste0('B',313:315),paste0('B',323:327),'B319','B329'),]$Cat <- 7

########## category 8
ICD9.data[ICD9.data$Cause %in% c(paste0('B',47:56)),]$Cat <- 8

########## category 9
ICD9.data[ICD9.data$Cause %in% c(paste0('B',15:17), 'B180', paste0('B',182:183),'B189', paste0('B',19:23),paste0('B',33:46)),]$Cat <- 9
unique(ICD9.data$Cat)
unique(ICD9.data[ICD9.data$Cat==10,]$Cause)
ICD9.data <- ICD9.data[ICD9.data$Cat!=10,]

# ICD 10 classification ----------------------------------------------------

ICD10.data      <- COD_Data[COD_Data$ICD==10,]
#get only the first 3 digits of the cause of death
sort(unique(ICD10.data$Cause))[2000:length(unique(ICD10.data$Cause))]
ICD10.data$Cause2 <- substr(ICD10.data$Cause,1,3)
ICD10.data$Cause3 <- substr(ICD10.data$Cause,4,4)
sort(unique(ICD10.data$Cause2))
sort(unique(ICD10.data$Cause3))

# Exclude the cause A000, is all causes in documentation
ICD10.data <- ICD10.data[ICD10.data$Cause != 'AAA',]

# bad codes
ICD10.data$Cat <- 10

########## category 1
ICD10.data[ICD10.data$Cause2 %in% c(paste0('A0',0:9),paste0('A', 10:99),
                                   paste0('B0', 0:9),paste0('B', 10:89),paste0('B', 99)),]$Cat <- 1

########## category 2
ICD10.data[ICD10.data$Cause2 %in% c(paste0('C0',0:9),paste0('C',10:21),paste0('C',25),paste0('C',30:34),'C53'),]$Cat <- 2

########## category 3
ICD10.data[ICD10.data$Cause2 %in% c(paste0('C',22:24),paste0('C',37:39),paste0('C',40:41),paste0('C',43:52),paste0('C',54:58),
                                   paste0('C',60:97)),]$Cat <- 3

########## category 4
ICD10.data[ICD10.data$Cause2 %in% c(paste0('E',10:14)),]$Cat <- 4

########## category 5
ICD10.data[ICD10.data$Cause2 %in% c(paste0('I0',0:9),paste0('I',10:99)),]$Cat <- 5

########## category 6
ICD10.data[ICD10.data$Cause2 %in% c(paste0('J0',0:6),paste0('J0',9),paste0('J',10:18),
                                    paste0('J',20:22),'J85','J86','J36'),]$Cat <- 6
ICD10.data[ICD10.data$Cause2=='J34' & ICD10.data$Cause3=='0',]$Cat <- 6
ICD10.data[ICD10.data$Cause2=='J39' & ICD10.data$Cause3=='0',]$Cat <- 6
ICD10.data[ICD10.data$Cause2=='J39' & ICD10.data$Cause3=='1',]$Cat <- 6

########## category 7
ICD10.data[ICD10.data$Cause2 %in% c(paste0('J',30:33),'J35','J37','J38',paste0('J',40:47),paste0('J',60:70),paste0('J',c(80,81,82)),
                                    paste0('J',90:99)),]$Cat <- 7
ICD10.data[ICD10.data$Cause2=='J34' & ICD10.data$Cause3=='1',]$Cat <- 7
ICD10.data[ICD10.data$Cause2=='J34' & ICD10.data$Cause3=='2',]$Cat <- 7
ICD10.data[ICD10.data$Cause2=='J34' & ICD10.data$Cause3=='3',]$Cat <- 7
ICD10.data[ICD10.data$Cause2=='J34' & ICD10.data$Cause3=='8',]$Cat <- 7
ICD10.data[ICD10.data$Cause2=='J39' & ICD10.data$Cause3=='2',]$Cat <- 7
ICD10.data[ICD10.data$Cause2=='J39' & ICD10.data$Cause3=='3',]$Cat <- 7
ICD10.data[ICD10.data$Cause2=='J39' & ICD10.data$Cause3=='8',]$Cat <- 7
ICD10.data[ICD10.data$Cause2=='J39' & ICD10.data$Cause3=='9',]$Cat <- 7

ICD10.data[ICD10.data$Cause2=='J84' & ICD10.data$Cause3=='0',]$Cat <- 7
ICD10.data[ICD10.data$Cause2=='J84' & ICD10.data$Cause3=='1',]$Cat <- 7

ICD10.data[ICD10.data$Cause2=='J84' & ICD10.data$Cause3=='8',]$Cat <- 7
ICD10.data[ICD10.data$Cause2=='J84' & ICD10.data$Cause3=='9',]$Cat <- 7

########## category 8
ICD10.data[ICD10.data$Cause2 %in% c(paste0('S0',0:9),paste0('T0',0:9),paste0('S',10:99),paste0('T',10:89),
                                    paste0('V',10:99),paste0('V0',1:9),
                                    paste0('W',10:99),paste0('W0',0:9),
                                    paste0('X',10:99),paste0('X0',0:9),
                                    paste0('Y',10:89),paste0('Y0',0:9)),]$Cat <- 8


########## Rest causes (category 9)
ICD10.data[ICD10.data$Cause2 %in% c(paste0('D0',0:9),paste0('D',10:48),paste0('D',50:89),
                                    paste0('E0',0:7),paste0('E',15:16),paste0('E',20:35),paste0('E',40:46),
                                    paste0('E',50:68),paste0('E',70:90),
                                    paste0('F0',0:9),paste0('F',10:99),
                                    paste0('G0',0:9),paste0('G',10:99),
                                    paste0('H0',0:9),paste0('H',10:59),paste0('H',60:95),
                                    paste0('K0',0:9),paste0('K',10:93),
                                    paste0('L0',0:9),paste0('L',10:99),
                                    paste0('M0',0:9),paste0('M',10:99),
                                    paste0('N0',0:9),paste0('N',10:99),
                                    paste0('O0',0:9),paste0('O',10:99),
                                    paste0('P0',0:9),paste0('P',10:96),
                                    paste0('Q0',0:9),paste0('Q',10:99),
                                    paste0('R0',0:9),paste0('R',10:99)),]$Cat <- 9
unique(ICD10.data$Cat)

##### Discard bad codes
check <- ICD10.data[ICD10.data$Cat==10,]
sort(unique(check$Cause))

ICD10.data <- ICD10.data[ICD10.data$Cat!=10,]

ICD10.data <- ICD10.data[,-c('Cause2','Cause3')]

# Get all ICDs together ---------------------------------------------------
DT_COD <- rbind(ICD7.data,ICD8.data,ICD9.data,ICD10.data)

gdata::keep(DT_COD, sure = T)


##### Now play with ages
unique(DT_COD$Frmat)

# groups ages 1:4 for format 0
DT_COD.0       <- DT_COD[DT_COD$Frmat == 0,]
DT_COD.0$A_1_4 <- DT_COD.0$Deaths3 + DT_COD.0$Deaths4 + DT_COD.0$Deaths5 + DT_COD.0$Deaths6

# groups ages 1:4 for format 1
DT_COD.1       <- DT_COD[DT_COD$Frmat == 1,]
DT_COD.1$A_1_4 <- DT_COD.1$Deaths3 + DT_COD.1$Deaths4 + DT_COD.1$Deaths5 + DT_COD.1$Deaths6

# groups ages 1:4 for format 2
DT_COD.2       <- DT_COD[DT_COD$Frmat == 2,]
DT_COD.2$A_1_4 <- DT_COD.2$Deaths3

# rbind the 2 datasets
DT_COD <- rbind(DT_COD.0,DT_COD.1,DT_COD.2)

#reduce to variables needed (age < 85), until Deaths22 
DT_COD           <- DT_COD[,c('Country','Country.name', 'ICD', 'Year', 'Sex', 'Cat', 'Deaths1', 'Deaths2', 'A_1_4', paste0('Deaths',7:22))]
colnames(DT_COD) <- c('Country','Country.name', 'ICD', 'Year', 'Sex', 'Cat','Total',as.character(c(0,1,seq(5,80,5))))
DT_COD           <- DT_COD[with(DT_COD,order(Country,Sex,Year,Cat))]

DT_COD.melt      <- melt(DT_COD, id.vars = c('Country','Country.name', 'ICD','Year','Sex','Cat'), variable.name = 'Age',value.name = 'Dx')


### Get total deaths by age, sex, category, year.
DT_COD.melt      <- DT_COD.melt[, list(Dx=sum(Dx)), by =  list(Country,Country.name,ICD,Year,Sex,Age,Cat)]

### get proportions of causes of death by age
DT_COD.melt      <- DT_COD.melt[DT_COD.melt$Age != 'Total',]

DT_COD.melt      <- DT_COD.melt[, Dx.p := Dx/sum(Dx), by = list(Country,Country.name,ICD,Year,Sex,Age)]

save(DT_COD.melt,file= 'Data/DT_COD.RData')

