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

Your_working_directory <- 'C:/Users/jmaburto/Documents/GitHub/CoD-burden-on-LA'

setwd(Your_working_directory)

source('R/1_Get_All_Data.R')

cause.name.vec <- c('Total', 'Infectious','Neoplasms', 'Circulatory','Abnormal', 'Mental',
                    'Nervous','Endocrine','Digestive',
                    'Genitourinary','Perinatal','Respiratory','External','Homicide','Rest')

# see the years of change from ICD9 to ICD10
ICD10.year

#see the new category 15
Cat.15         <- Deaths.data[Deaths.data$Cause == 15,]
Cat.15.s       <- Cat.15[,list(Female =sum(Female)), by = list(X,Year)]
Cat.15.s$Male  <- Cat.15[,list(Male =sum(Male)), by = list(X,Year)]$Male
# For all the cuntries, there is no informatino on catefory 15, so of course there is a rupture.

# A dataset without cause = 1, total. Makes no difference, but just be sure

Deaths.age       <- Deaths.data[Deaths.data$Age2 < 97,]
unique(Deaths.age$Age2)
unique(Deaths.age$Cause)
# get proportions for males and females

# Sum over ages to get totals by cause 
# first sum by Age 
#check <- Deaths[Deaths$Cause==1,] >0

Deaths          <- Deaths.age[,list(Female = sum(Female)), by = list(X,Year,Cause)]
Deaths$Male     <- Deaths.age[,list(Male = sum(Male)), by = list(X,Year,Cause)]$Male

# Convert to proportions
# create a function to calculate proportions
my.prop <- function(deaths){
  x <- length(deaths)
  z <- deaths[1L]
  y <- deaths/z
  y
}

Deaths          <- Deaths[,Female.p := my.prop(Female), by = list(X,Year)] 
Deaths          <- Deaths[,Male.p := my.prop(Male), by = list(X,Year)] 

## create a diagnosis graph
Deaths$X2 <- Deaths$X
Deaths$X2 <- factor(Deaths$X2, levels = Country.code.vec, labels = Country.name.vec)


library(randomcoloR)
n <- 14
col2 <- distinctColorPalette(n)

f1 <- xyplot(Female.p ~ Year | X2, data = Deaths[Deaths$Cause != 1,], 
       groups = Cause, type = 'l',layout =c(5,5),col=col2,main='Female, proportions by cause of death',
       key=list(x=.8,y=1,background="transparent",
                text=list(cause.name.vec[-1],
                          col="black"),cex=.7,
                lines=list(lty=1,lwd=2,col=col2)),
       panel = function(x, y,...){           
         panel.abline(v=ICD10.year$ICD10.Year[panel.number()],col='red',lty=2, lwd=2)
         panel.xyplot(x, y,lty=1,lwd = 2,...)  
       })
f1

pdf(file="Figures/Female_Prop.pdf",width=12,height=11,pointsize=12)
print(f1)
dev.off()


f2 <- xyplot(Male.p ~ Year | X2, data = Deaths[Deaths$Cause != 1,], 
             groups = Cause, type = 'l',layout =c(5,5),col=1:14,main='Male, proportions by cause of death',
             key=list(x=.8,y=1,background="transparent",
                      text=list(cause.name.vec[-1],
                                col="black"),cex=.7,
                      lines=list(lty=1,lwd=2,col=1:14)),
             panel = function(x, y,...){           
               panel.abline(v=ICD10.year$ICD10.Year[panel.number()],col='red',lty=2, lwd=2)
               panel.xyplot(x, y,lty=1,lwd = 2,...)  
             })
f2

pdf(file="Figures/Male_Prop.pdf",width=12,height=11,pointsize=12)
print(f2)
dev.off()



pdf(file="Figures/Female_CoD_Country.pdf",width=12,height=11,pointsize=12)
for (i in Country.code.vec){
  
  f3 <- xyplot(Female.p ~ Year | Cause2, data = Deaths[Deaths$Cause != 1 & X == i,],
               main=paste0(as.character(ICD10.year$Country[which(ICD10.year$Code==i)]), ', Female'),
               groups = Cause, type = 'l',layout =c(4,4),col=1:14,
               panel = function(x, y,...){           
                 panel.abline(v=ICD10.year$ICD10.Year[which(ICD10.year$Code==i)],col='red',lty=2, lwd=2)
                 panel.xyplot(x, y,lty=1,lwd = 2,...)  
               })
  print(f3)
}
dev.off()

pdf(file="Figures/Male_CoD_Country.pdf",width=12,height=11,pointsize=12)
for (i in Country.code.vec){
  
  f4 <- xyplot(Male.p ~ Year | Cause2, data = Deaths[Deaths$Cause != 1 & X == i,],
               main=paste0(as.character(ICD10.year$Country[which(ICD10.year$Code==i)]), ', Male'),
               groups = Cause, type = 'l',layout =c(4,4),col=1:14,
               panel = function(x, y,...){           
                 panel.abline(v=ICD10.year$ICD10.Year[which(ICD10.year$Code==i)],col='red',lty=2, lwd=2)
                 panel.xyplot(x, y,lty=1,lwd = 2,...)  
               })
  print(f4)
}
dev.off()


Deaths$Cause2 <- Deaths$Cause
Deaths$Cause2 <- factor(Deaths$Cause2, levels = 1:15, labels = cause.name.vec)
Deaths  <- Deaths[Deaths$Cause != 1,c("X","X2","Year","Cause","Cause2","Female","Male","Female.p","Male.p")]
Deaths2 <- melt.data.table(Deaths[,-c(8:9)],id.vars =1:5,variable.name = 'Sex',value.name = 'Deaths')
Deaths3 <- melt.data.table(Deaths[,-c(6:7)],id.vars =1:5,variable.name = 'Sex',value.name = 'Proportions')
Deaths2$Proportion <- Deaths3$Proportions

vline.data <- ICD10.year[,1:3]

names.c <- unique(Deaths2$X2)

save(Deaths2,vline.data, file = 'R/CoD_App/Prop_Data.RData')

gdata:: keep(Deaths2,ICD10.year,vline.data,sure=T)
dev.off()
