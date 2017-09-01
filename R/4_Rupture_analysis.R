###############################################################################
### Get CoD data for Denmark, Sweden and Norway. Source: WHO (07/07/2017)     #

library(ggplot2)
library(data.table)
library(reshape2)

setwd("C:/Users/jmaburto/Documents/GitHub/Lifespan-inequality-Denmark")

# Run this line to update the COD RData file
get(load('Data/DT_COD.RData'))
source('R/Functions_1.R')
#1  Contagious, non-respiratory
#2  Tentatively opportunistic infections
#3  Cancer, amenable to smoking
#4  Cancer, not amenable to smoking
#5  Diabetes mellitus
#6  Cardiovascular
#7  Respiratory, infectious
#8  Respiratory, non-infectious
#9  External
#10 Other

#rename categories, sexes and Age_groups

levels(DT_COD.melt$Sex) <- c('Males', 'Females')
levels(DT_COD.melt$Age) <- Age.labels
DT_COD.melt$Cat         <- as.factor(DT_COD.melt$Cat)
levels(DT_COD.melt$Cat) <- Category.labels


# Analysis only for Denmark -----------------------------------------------

Prop.data <- DT_COD.melt[,list(Dx=sum(Dx)), by = list(Country.name, ICD, Year,Sex,Cat)]
Prop.data <- Prop.data[Prop.data$Country.name=='Denmark',]
  
change.ICD <- NULL
change.ICD$Year   <- c(1969,1994)
change.ICD        <- data.frame(change.ICD)
change.ICD$ICD    <- c('ICD 8', 'ICD 10')

f1 <- ggplot(Prop.data, aes(Year,Dx))+
  ggtitle('Denmark',subtitle = 'Years of change in ICDs: 1969 and 1994')+
  geom_line(aes(colour = Sex), lwd=1,show.legend =T)+
  geom_point(aes(colour = Sex), lwd=1,show.legend =F)+
  theme(text = element_text(size=20))+
  facet_wrap(~Cat,scales = "free",ncol = 3)+ xlim(c(1960, 2014))+
  geom_vline(data=change.ICD, 
             aes(xintercept=Year, 
                 colour = ICD),
             show.legend = T)+
  theme(legend.title=element_blank())+
  coord_fixed(1.5)

# Analysis only for Sweden -----------------------------------------------

Prop.data <- DT_COD.melt[,list(Dx=sum(Dx)), by = list(Country.name, ICD, Year,Sex,Cat)]
Prop.data <- Prop.data[Prop.data$Country.name=='Sweden',]

change.ICD <- NULL
change.ICD$Year   <- c(1969,1987,1997)
change.ICD        <- data.frame(change.ICD)
change.ICD$ICD    <- c('ICD 8', 'ICD 9', 'ICD 10')

f2 <- ggplot(Prop.data, aes(Year,Dx))+
  ggtitle('Sweden',subtitle = 'Years of change in ICDs: 1969, 1987 and 1997')+
  geom_line(aes(colour = Sex), lwd=1,show.legend =T)+
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
  geom_line(aes(colour = Sex), lwd=1,show.legend =T)+
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



pdf(file="R/Figures/Sensitivity.pdf",width=17,height=15,pointsize=6,useDingbats = F)
print(f1)
print(f2)
print(f3)
dev.off()




