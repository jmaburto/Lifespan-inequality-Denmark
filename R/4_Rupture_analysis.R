###############################################################################
### Get CoD data for Denmark, Sweden and Norway. Source: WHO (07/07/2017)     #
library(ggplot2)
library(data.table)
library(reshape2)

setwd("C:/Users/jmaburto/Documents/GitHub/Lifespan-inequality-Denmark")

# Run this line to update the COD RData file
get(load('Data/DT_COD.RData'))
source('R/Functions_1.R')

#2  Cancer, amenable to smoking
#3  Cancer, not amenable to smoking
#5  Cardiovascular & Diabetes mellitus (move to 5)
#6  Respiratory, infectious
#7  Respiratory, non-infectious
#8  External
#9  Other & Infectious, non-respiratory

#rename categories, sexes and Age_groups

DT_COD.melt$ICD <- as.factor(DT_COD.melt$ICD)
levels(DT_COD.melt$ICD) <- c('ICD 7', 'ICD 8', 'ICD 9', 'ICD 10')
levels(DT_COD.melt$Sex) <- c('Males', 'Females')
levels(DT_COD.melt$Age) <- Age.labels
DT_COD.melt$Cat         <- as.factor(DT_COD.melt$Cat)
levels(DT_COD.melt$Cat) <- Category.labels
unique(DT_COD.melt$Age)
#DT_COD.melt <- DT_COD.melt[DT_COD.melt$Age!= '80-84'& DT_COD.melt$Age!= '75-79',]

# Analysis only for Denmark -----------------------------------------------
Prop.data <- DT_COD.melt[,list(Dx=sum(Dx)), by = list(Country.name, ICD, Year,Sex,Cat)]
Prop.data <- Prop.data[Prop.data$Country.name=='Denmark',]
  
change.ICD <- NULL
change.ICD$Year   <- c(1969,NA,1994)
change.ICD        <- data.frame(change.ICD)
change.ICD$ICD    <- 8:10
change.ICD$ICD    <- as.factor(change.ICD$ICD)
levels(change.ICD$ICD)  <- c('ICD 8','ICD 9', 'ICD 10')

f1 <- ggplot(Prop.data, aes(Year,Dx))+
  ggtitle('A) Denmark',subtitle = 'Years of change in ICDs: 1969 and 1994')+
  geom_line(aes(colour = Sex), lwd=1,show.legend =T)+
  theme_light()+
  geom_point(aes(colour = Sex), lwd=1,show.legend =F)+
  theme(text = element_text(size=20))+
  facet_wrap(~Cat,scales = "free",ncol = 3)+
  xlim(c(1960, 2014))+
  geom_vline(data=change.ICD, 
             aes(xintercept=Year,color=ICD), show.legend = F)+
  theme(legend.title=element_blank())+
  coord_fixed(1.5)
f1

# Analysis only for Sweden -----------------------------------------------

Prop.data <- DT_COD.melt[,list(Dx=sum(Dx)), by = list(Country.name, ICD, Year,Sex,Cat)]
Prop.data <- Prop.data[Prop.data$Country.name=='Sweden',]

change.ICD <- NULL
change.ICD$Year   <- c(1969,1987,1997)
change.ICD        <- data.frame(change.ICD)
change.ICD$ICD    <- 8:10
change.ICD$ICD    <- as.factor(change.ICD$ICD)
levels(change.ICD$ICD)  <- c('ICD 8','ICD 9', 'ICD 10')


f2 <- ggplot(Prop.data, aes(Year,Dx))+
  ggtitle('B) Sweden',subtitle = 'Years of change in ICDs: 1969, 1987 and 1997')+
  geom_line(aes(colour = Sex), lwd=1,show.legend =T)+theme_light()+
  geom_point(aes(colour = Sex), lwd=1,show.legend =F)+
  theme(text = element_text(size=20))+
  facet_wrap(~Cat,scales = "free",ncol = 3)+ xlim(c(1960, 2015))+
  geom_vline(data=change.ICD, 
             aes(xintercept=Year, 
                 #linetype=ICD,
                 colour = ICD),
             show.legend = T)+
  theme(legend.title=element_blank())+
  coord_fixed(1.5)



# Analysis only for Norway -----------------------------------------------

Prop.data <- DT_COD.melt[,list(Dx=sum(Dx)), by = list(Country.name, ICD, Year,Sex,Cat)]
Prop.data <- Prop.data[Prop.data$Country.name=='Norway',]

change.ICD <- NULL
change.ICD$Year   <- c(1969,1986,1996)
change.ICD        <- data.frame(change.ICD)
change.ICD$ICD    <- c('ICD 8', 'ICD 9', 'ICD 10')

f3 <- ggplot(Prop.data, aes(Year,Dx))+
  ggtitle('Norway', subtitle = 'Years of change in ICDs: 1969, 1986 and 1996')+
  geom_line(aes(colour = Sex), lwd=1,show.legend =T)+theme_light()+
  geom_point(aes(colour = Sex), lwd=1,show.legend =F)+
  theme(text = element_text(size=20))+
  facet_wrap(~Cat,scales = "free",ncol = 3)+ xlim(c(1960, 2015))+
  geom_vline(data=change.ICD, 
             aes(xintercept=Year, 
                 #linetype=ICD,
                 colour = ICD),
             show.legend = T)+
  theme(legend.title=element_blank())+
  
  coord_fixed(1.5)



pdf(file="R/Figures/Sensitivity.pdf",width=13,height=11,pointsize=6,useDingbats = F)
print(f1)
print(f2)
#print(f3)
dev.off()




