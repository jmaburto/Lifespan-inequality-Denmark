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


load('Data/Results.RData')

new.level.order<- c("Smoking related cancer","Non-Smoking related cancer","Cardiovascular","Respiratory-Non-infectious","Respiratory-Infectious","External","Other","Above age 85")


Decomp.ex  <- local(get(load("Data/DecompResults_ex_List.RData")))
Decomp.cv  <- local(get(load("Data/DecompResults_cv_List.RData")))
Compare.ex <- local(get(load("Data/Compare_DecompResults_ex_List.RData")))
Compare.cv <- local(get(load("Data/Compare_DecompResults_cv_List.RData")))

Labels.periods            <- c('1960-1975','1975-1995','1995-2014')

DT.Decomp.ex$Period     <- (cut(DT.Decomp.ex$Year+1, breaks=c(1960,1975,1995,Inf),labels=Labels.periods))
DT.Decomp.cv$Period     <- (cut(DT.Decomp.cv$Year+1, breaks=c(1960,1975,1995,Inf),labels=Labels.periods))

DT.Decomp.ex            <- DT.Decomp.ex[,list(Contribution = sum(Contribution)), by = list(Sex,Country,Period,Cause,Age)]
DT.Decomp.cv            <- DT.Decomp.cv[,list(Contribution = sum(Contribution)), by = list(Sex,Country,Period,Cause,Age)]


base2 <- c(rev(brewer.pal(8,name = 'Spectral'))[1:5],rev(brewer.pal(8,name = 'Spectral'))[8],'lightgrey','lightpink')
base2[2] <- 'lightseagreen'
base2[4] <- 'tan1'

levels(DT.compare.ex$Cause)

DT.Decomp.ex$Cause <- factor(DT.Decomp.ex$Cause, levels = new.level.order)
DT.Decomp.cv$Cause <- factor(DT.Decomp.cv$Cause, levels = new.level.order)
DT.compare.ex$Cause <- factor(DT.compare.ex$Cause, levels = new.level.order)
DT.compare.cv$Cause <- factor(DT.compare.cv$Cause, levels = new.level.order)


#base2 <- c("#66C2A5",   "#ABDDA4",'peachpuff2', '#fc8d62','coral2',   "#D53E4F" ,  "lightgrey" ,"lightpink")

# Changes in Denmark in life expectancy
p <- ggplot(DT.Decomp.ex[DT.Decomp.ex$Sex == 'Female' & DT.Decomp.ex$Country=='Denmark',], aes(x = Age, y = Contribution, fill = Cause)) +
  ggtitle('A Decomposition of life expectancy', subtitle = 'Danish females. Negative (positive) values decrease (increase) life expectancy')+
  facet_wrap(~Period)+
  scale_fill_manual('Cause of death', values = base2) +
  geom_bar(stat = "identity",position = "stack")+
  theme_light()+
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=45, hjust=1),panel.grid.minor.x = element_blank())+
  labs(x = "Age group", y = "Years",size=10)+
  theme(text = element_text(size=10),
        strip.text.x = element_text(size = 10, colour = "black"))+
   theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
         legend.position = c(0.9, 0.18))+
  geom_hline(yintercept = 0)+
  coord_flip()
p


q <- ggplot(DT.Decomp.cv[DT.Decomp.cv$Sex == 'Female' & DT.Decomp.cv$Country=='Denmark',], aes(x = Age, y = Contribution, fill = Cause)) +
  ggtitle('B Decomposition of lifespan inequality (CoV)', subtitle = 'Danish females. Negative (positive) values decrease (increase) CoV')+
  facet_wrap(~Period)+
  scale_fill_manual('Cause of death', values = base2) + 
  geom_bar(stat = "identity",position = "stack", show.legend = F)+
  theme_light()+  coord_cartesian(ylim=c(-.0035, .003))+
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=45, hjust=1),panel.grid.minor.x = element_blank())+
  labs(x = "Age group", y = "Units",size=10)+
  theme(text = element_text(size=10),
        strip.text.x = element_text(size = 10, colour = "black"))+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
        legend.position = c(0.93, 0.25))+
  geom_hline(yintercept = 0)+
  coord_flip(ylim =c(-.0035, .003) )
q

require(gridExtra)
pdf(file="R/Figures/DX_Decomp.pdf",width=12,height=11,pointsize=4,useDingbats = F)
grid.arrange(p,q,nrow=2)
dev.off()

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


load('Data/Results.RData')

new.level.order<- c("Smoking related cancer","Non-Smoking related cancer","Cardiovascular","Respiratory-Non-infectious","Respiratory-Infectious","External","Other","Above age 85")


Decomp.ex  <- local(get(load("Data/DecompResults_ex_List.RData")))
Decomp.cv  <- local(get(load("Data/DecompResults_cv_List.RData")))
Compare.ex <- local(get(load("Data/Compare_DecompResults_ex_List.RData")))
Compare.cv <- local(get(load("Data/Compare_DecompResults_cv_List.RData")))

Labels.periods            <- c('1960-1975','1975-1995','1995-2014')

DT.Decomp.ex$Period     <- (cut(DT.Decomp.ex$Year+1, breaks=c(1960,1975,1995,Inf),labels=Labels.periods))
DT.Decomp.cv$Period     <- (cut(DT.Decomp.cv$Year+1, breaks=c(1960,1975,1995,Inf),labels=Labels.periods))

DT.Decomp.ex            <- DT.Decomp.ex[,list(Contribution = sum(Contribution)), by = list(Sex,Country,Period,Cause,Age)]
DT.Decomp.cv            <- DT.Decomp.cv[,list(Contribution = sum(Contribution)), by = list(Sex,Country,Period,Cause,Age)]


base2 <- c(rev(brewer.pal(8,name = 'Spectral'))[1:5],rev(brewer.pal(8,name = 'Spectral'))[8],'lightgrey','lightpink')
base2[2] <- 'lightseagreen'
base2[4] <- 'tan1'

levels(DT.compare.ex$Cause)

DT.Decomp.ex$Cause <- factor(DT.Decomp.ex$Cause, levels = new.level.order)
DT.Decomp.cv$Cause <- factor(DT.Decomp.cv$Cause, levels = new.level.order)
DT.compare.ex$Cause <- factor(DT.compare.ex$Cause, levels = new.level.order)
DT.compare.cv$Cause <- factor(DT.compare.cv$Cause, levels = new.level.order)


#base2 <- c("#66C2A5",   "#ABDDA4",'peachpuff2', '#fc8d62','coral2',   "#D53E4F" ,  "lightgrey" ,"lightpink")

# Changes in Denmark in life expectancy
t <- ggplot(DT.Decomp.ex[DT.Decomp.ex$Sex == 'Male' & DT.Decomp.ex$Country=='Denmark',], aes(x = Age, y = Contribution, fill = Cause)) +
  ggtitle('A Decomposition of life expectancy', subtitle = 'Danish males. Negative (positive) values decrease (increase) life expectancy')+
  facet_wrap(~Period)+
  scale_fill_manual('Cause of death', values = base2) +
  geom_bar(stat = "identity",position = "stack")+
  theme_light()+
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=45, hjust=1),panel.grid.minor.x = element_blank())+
  labs(x = "Age group", y = "Years",size=10)+
  theme(text = element_text(size=10),
        strip.text.x = element_text(size = 10, colour = "black"))+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
        legend.position = c(0.9, 0.18))+
  geom_hline(yintercept = 0)+
  coord_flip()
t


u <- ggplot(DT.Decomp.cv[DT.Decomp.cv$Sex == 'Male' & DT.Decomp.cv$Country=='Denmark',], aes(x = Age, y = Contribution, fill = Cause)) +
  ggtitle('B Decomposition of lifespan inequality (CoV)', subtitle = 'Danish Males. Negative (positive) values decrease (increase) CoV')+
  facet_wrap(~Period)+
  scale_fill_manual('Cause of death', values = base2) + 
  geom_bar(stat = "identity",position = "stack", show.legend = F)+
  theme_light()+  
  #coord_cartesian(ylim=c(-.0035, .003))+
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=45, hjust=1),panel.grid.minor.x = element_blank())+
  labs(x = "Age group", y = "Units",size=10)+
  theme(text = element_text(size=10),
        strip.text.x = element_text(size = 10, colour = "black"))+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
        legend.position = c(0.93, 0.25))+
  geom_hline(yintercept = 0)+
  coord_flip(ylim =c(-.004, .0035) )
u

require(gridExtra)
pdf(file="R/Figures/DX_Decomp_males.pdf",width=12,height=11,pointsize=4,useDingbats = F)
grid.arrange(t,u,nrow=2)
dev.off()


DT.Decomp.ex[,list(Contribution = sum(Contribution)), by = list(Sex,Country,Period)]
DT.Decomp.cv[,list(Contribution = sum(Contribution)), by = list(Sex,Country,Period)]

### Decomp with Sweden
# Changes in Denmark in life expectancy
r <- ggplot(DT.compare.ex[DT.compare.ex$Country=='Denmark' & DT.compare.ex$Year == 2014,], aes(x = Age, y = Contribution, fill = Cause)) +
  ggtitle('A Decomposition of life expectancy', subtitle = 'Sweden - Denmark, 2014. Negative (positive) values decrease (increase) the gap in life expectancy with Sweden.')+
  facet_wrap(~Sex)+
  scale_fill_manual('Cause of death', values = base2) + 
  geom_bar(stat = "identity",position = "stack")+
  theme_light()+
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=45, hjust=1),panel.grid.minor.x = element_blank())+
  labs(x = "Age group", y = "Years",size=10)+
  theme(text = element_text(size=10),
        strip.text.x = element_text(size = 10, colour = "black"))+
  theme(legend.text=element_text(size=9), legend.title = element_text(size=9),
        legend.position = c(0.39, 0.18))+
  geom_hline(yintercept = 0)+
  coord_flip()
r


s <- ggplot(DT.compare.cv[DT.compare.cv$Country=='Denmark' & DT.compare.cv$Year == 2014,], aes(x = Age, y = -Contribution, fill = Cause)) +
  ggtitle('B Decomposition of lifespan inequality (CoV)', subtitle = 'Denmark - Sweden, 2014. Negative (positive) values decrease (increase) the gap in CoV with Sweden.')+
  facet_wrap(~Sex)+
  scale_fill_manual('Cause of death', values = base2) + 
  geom_bar(stat = "identity",position = "stack", show.legend = F)+
  theme_light()+
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=45, hjust=1),panel.grid.minor.x = element_blank())+
  labs(x = "Age group", y = "Units",size=10)+
  theme(text = element_text(size=10),
        strip.text.x = element_text(size = 10, colour = "black"))+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
        legend.position = c(0.93, 0.25))+
  geom_hline(yintercept = 0)+
  coord_flip()
s

require(gridExtra)
pdf(file="R/Figures/DK_Compare.pdf",width=10,height=10,pointsize=4,useDingbats = F)
grid.arrange(r,s,nrow=2)
dev.off()


#Table of potential gaind in life expectancy


Data2014ex <- DT.compare.ex[DT.compare.ex$Year == 2014,]
Data2014ex$ind <- 'ex'
Data2014cv <- DT.compare.cv[DT.compare.cv$Year == 2014,]
Data2014cv$ind <- 'cv'

Data2014 <- rbind(Data2014ex,Data2014cv)
Data2014 <- Data2014[Data2014$Country == 'Denmark',]

table1 <- Data2014[, sum(Contribution), by = list(Sex,Cause,ind)]
table1
write.csv(table1,file = 'R/Figures/Table1.cvs')

Data2014[, sum(Contribution), by = list(Sex,Age,ind)]


