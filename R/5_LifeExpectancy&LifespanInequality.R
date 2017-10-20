
#rm(list=ls(all=TRUE))
library(data.table)
library(reshape2)
library(ggplot2)

setwd("C:/Users/jmaburto/Documents/GitHub/Lifespan-inequality-Denmark")

HMD <- get(load('Data/HMD_Data.RData'))

## subset the countries that I am interested in
unique(HMD$PopName)

Countries <- c('DNK', 'SWE', 'NOR')

Data <- HMD[HMD$PopName %in% Countries,]
#gdata::keep(Data,Countries,sure=T)


source('R/Functions_1.R')
# Get total for the country
#mx <- DT.mxCOD[year==1995 & sex == 1 & state==0,]$mx
#sex = 'f'

#get the coefficient of variation
Data <- data.table(Data)
Data$Sex2 <- Data$Sex
Data <- Data[, cv:= cv.frommx(.SD$mx,.SD$Sex2[1]), by = list(Year,Sex,PopName)]
Data <- Data[Data$Age == 0,]
Data <- Data[Data$Year >= 1960,]
head(Data)

Data[Data$Sex2 == 'f',]$Sex2 <- 'Females'
Data[Data$Sex2 == 'm',]$Sex2 <- 'Males'


p <-ggplot(Data, aes(x = Year,y = ex)) +
  ggtitle('A Life expectancy at birth') +
  geom_rect(aes(xmin=1975, xmax=1995, ymin=-Inf, ymax=Inf), 
             fill = "pink", alpha = 0.01, show.legend = F) +
  geom_line(aes(group = PopName,colour=PopName), size= 1.2) +
  scale_x_continuous(breaks = c(1960, 1975, 1995,2005,2014))+
  facet_wrap(~Sex2)+
  theme_light()+
  labs(y = "Years")+
  scale_colour_manual('Country', values = c('red','blue','green'), labels = c('Denmark','Norway', 'Sweden')) + 
  theme(text = element_text(size=14),legend.position = c(0.57, 0.8),
        strip.text.x = element_text(size = 14, colour = "black"))
p


q <-ggplot(Data, aes(x = Year,y = cv)) +
  ggtitle('B Lifespan inequality (CoV)') +
  geom_rect(aes(xmin=1975, xmax=1995, ymin=-Inf, ymax=Inf), 
            fill = "pink", alpha = 0.01, show.legend = F) +
  geom_line(aes(group = PopName,colour=PopName), size= 1.2,show.legend = F) +
  scale_x_continuous(breaks = c(1960, 1975, 1995,2005,2014))+
  facet_wrap(~Sex2)+
  theme_light()+
  labs(y = "Units")+
  scale_colour_manual('Country', values = c('red','blue','green'), labels = c('Denmark','Norway', 'Sweden')) + 
  theme(text = element_text(size=14),
        strip.text.x = element_text(size = 14, colour = "black"))
q

require(gridExtra)
pdf(file="R/Figures/Trends.pdf",width=9,height=10,pointsize=4,useDingbats = F)
grid.arrange(p,q,nrow=2)
dev.off()




