library(ggplot2)
library(data.table)
library(reshape2)
library(RColorBrewer)

setwd("C:/Users/jmaburto/Documents/GitHub/Lifespan-inequality-Denmark")
#2  Cancer, amenable to smoking
#3  Cancer, not amenable to smoking
#5  Cardiovascular & Diabetes mellitus (move to 5)
#6  Respiratory, infectious
#7  Respiratory, non-infectious
#8  External
#9  Other & Infectious, non-respiratory


load('Data/Sensitivity_Results.RData')

new.level.order<- c("Smoking related cancer","Non-Smoking related cancer","Cardiovascular","Respiratory-Non-infectious","Respiratory-Infectious","External","Other","Above age 85")

Decomp.cv  <- local(get(load("Data/DecompResults_sd_List.RData")))
Compare.cv <- local(get(load("Data/Compare_DecompResults_sd_List.RData")))

Labels.periods            <- c('1960-1975','1975-1995','1995-2014')

DT.Decomp.sd$Period     <- (cut(DT.Decomp.sd$Year+1, breaks=c(1960,1975,1995,Inf),labels=Labels.periods))

DT.Decomp.sd            <- DT.Decomp.sd[,list(Contribution = sum(Contribution)), by = list(Sex,Country,Period,Cause,Age)]


base2 <- c(rev(brewer.pal(8,name = 'Spectral'))[1:5],rev(brewer.pal(8,name = 'Spectral'))[8],'lightgrey','lightpink')
base2[2] <- 'lightseagreen'
base2[4] <- 'tan1'

DT.Decomp.sd$Cause <- factor(DT.Decomp.sd$Cause, levels = new.level.order)
DT.compare.sd$Cause <- factor(DT.compare.sd$Cause, levels = new.level.order)

q <- ggplot(DT.Decomp.sd[DT.Decomp.sd$Sex == 'Female' & DT.Decomp.sd$Country=='Denmark',], aes(x = Age, y = Contribution, fill = Cause)) +
  ggtitle('Decomposition of standard deviation', subtitle = 'Danish females. Negative (positive) values decrease (increase) SD')+
  facet_wrap(~Period)+
  scale_fill_manual('Cause of death', values = base2) + 
  geom_bar(stat = "identity",position = "stack", show.legend = T)+
  theme_light()+  
  coord_cartesian(ylim=c(-.3, .3))+
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=45, hjust=1),panel.grid.minor.x = element_blank())+
  labs(x = "Age group", y = "Years",size=10)+
  theme(text = element_text(size=10),
        strip.text.x = element_text(size = 10, colour = "black"))+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
        legend.position = 'bottom')+
  geom_hline(yintercept = 0)+
  coord_flip(ylim =c(-.3, .3) )

pdf(file="R/Sensitivity Analysis/Figures Sensitivity/DecompSDtimeFemales.pdf",width=12,height=5.5,pointsize=4,useDingbats = F)
q
dev.off()


u <- ggplot(DT.Decomp.sd[DT.Decomp.sd$Sex == 'Male' & DT.Decomp.sd$Country=='Denmark',], aes(x = Age, y = Contribution, fill = Cause)) +
  ggtitle('Decomposition of standard deviation', subtitle = 'Danish males. Negative (positive) values decrease (increase) SD')+
  facet_wrap(~Period)+
  scale_fill_manual('Cause of death', values = base2) + 
  geom_bar(stat = "identity",position = "stack", show.legend = T)+
  theme_light()+  
  coord_cartesian(ylim=c(-.3, .4))+
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=45, hjust=1),panel.grid.minor.x = element_blank())+
  labs(x = "Age group", y = "Years",size=10)+
  theme(text = element_text(size=10),
        strip.text.x = element_text(size = 10, colour = "black"))+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
        legend.position = 'bottom')+
  geom_hline(yintercept = 0)+
  coord_flip(ylim =c(-.3, .4) )
u


pdf(file="R/Sensitivity Analysis/Figures Sensitivity/DecompSDtimeMales.pdf",width=12,height=5.5,pointsize=4,useDingbats = F)
u
dev.off()



s <- ggplot(DT.compare.sd[DT.compare.sd$Country=='Denmark' & DT.compare.sd$Year == 2014,], aes(x = Age, y = -Contribution, fill = Cause)) +
  ggtitle('Decomposition ofstandard deviation', subtitle = 'Denmark - Sweden, 2014. Negative (positive) values decrease (increase) the gap in SD with Sweden.')+
  facet_wrap(~Sex)+
  scale_fill_manual('Cause of death', values = base2) + 
  geom_bar(stat = "identity",position = "stack", show.legend = T)+
  theme_light()+
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=45, hjust=1),panel.grid.minor.x = element_blank())+
  labs(x = "Age group", y = "Years",size=10)+
  theme(text = element_text(size=10),
        strip.text.x = element_text(size = 10, colour = "black"))+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
        legend.position = 'bottom')+
  geom_hline(yintercept = 0)+
  coord_flip()
s

require(gridExtra)
pdf(file="R/Sensitivity Analysis/Figures Sensitivity/CompareSD.pdf",width=10,height=5,pointsize=4,useDingbats = F)
s
dev.off()


#Table of potential gains in life expectancy
