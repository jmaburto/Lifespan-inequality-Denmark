#rm(list=ls(all=TRUE))
library(data.table)
library(reshape2)
library(ggplot2)

setwd("C:/Users/jmaburto/Documents/GitHub/Lifespan-inequality-Denmark")

HMD <- get(load('Data/HMD_Data.RData'))

## subset the countries that I am interested in
unique(HMD$PopName)

Countries <- c('DNK', 'SWE')

Data <- HMD[HMD$PopName %in% Countries,]
#gdata::keep(Data,Countries,sure=T)


source('R/Sensitivity Analysis/Functions_Sensitivity.R')

#get the standard deviation
Data <- data.table(Data)
Data$Sex2 <- Data$Sex
Data <- Data[, sd:= sd.frommx(.SD$mx,.SD$Sex2[1]), by = list(Year,Sex,PopName)]
Data <- Data[Data$Age == 0,]
Data <- Data[Data$Year >= 1960,]
head(Data)

Data[Data$Sex2 == 'f',]$Sex2 <- 'Females'
Data[Data$Sex2 == 'm',]$Sex2 <- 'Males'

q <-ggplot(Data, aes(x = Year,y = sd)) +
  ggtitle('Trends in standard deviation') +
  geom_rect(aes(xmin=1975, xmax=1995, ymin=-Inf, ymax=Inf), 
            fill = "pink", alpha = 0.01, show.legend = F) +
  geom_line(aes(group = PopName,colour=PopName), size= 1.2,show.legend = F) +
  scale_x_continuous(breaks = c(1960, 1975, 1995,2005,2014))+
  facet_wrap(~Sex2)+
  theme_light()+
  labs(y = "Years")+
  scale_colour_manual('Country', values = c('red','green'), labels = c('Denmark', 'Sweden')) + 
  theme(text = element_text(size=14),
        strip.text.x = element_text(size = 14, colour = "black"))
q

pdf(file="R/Sensitivity Analysis/Figures Sensitivity/Trends_SD.pdf",width=9,height=5,pointsize=4,useDingbats = F)
q
dev.off()




