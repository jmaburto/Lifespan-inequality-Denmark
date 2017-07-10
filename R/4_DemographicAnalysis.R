############# Written by JMA
############# Project with Marteen and Rune
library(data.table)
library(reshape)
library(latticeExtra)
library(ggplot2)

setwd("C:/Users/jmaburto/Documents/GitHub/Lifespan-inequality-in-Denmark")

source("R/Functions_1.R")

# Run this line to update the HMD RData file
# source(file = 'R/1_1GetHMDData.R.R')

# Loading data
load("Data/HMD_Data.RData")

# subset countries that might be interesting Norway, Sweden, Denmark and Japan
Data  <- HMDL[(HMDL$PopName == "DNK" | HMDL$PopName =="SWE" |  HMDL$PopName =="NOR" |  HMDL$PopName =="JPN"),]

# Get the % of the change in life expectancy below age 85 for Denmark
DK1960    <- Data[Data$PopName=='DNK' & Data$Sex=='f' & Data$Year==1960,]$mx
DK2014    <- Data[Data$PopName=='DNK' & Data$Sex=='f' & Data$Year==2014,]$mx
Decomp.DK <- data.table(Decomp(LifeExpectancy, DK1960, DK2014,sex = 'f',N=100))
sum(Decomp.DK) - (LifeExpectancy(DK2014) - LifeExpectancy(DK1960))
Decomp.DK$Age <- 0:110
sum(Decomp.DK[Decomp.DK$Age < 85,]$V1)/sum(Decomp.DK$V1)*100

# Get the coefficient of variation by year, country and sex

############## Calculate lifespan inequality measures, I think cv (for the paper) and Gini (for the appendix)
DK.cv            <- Data[,list(cv=cv.frommx(mx = mx,sex = Sex[1])), by = list(PopName,Sex,Year)]
DK.SD            <- Data[,list(cv=cv.frommx(mx = mx,sex = Sex[1])), by = list(PopName,Sex,Year)]
DK.Gini          <- Data[,list(Gini=Gini.frommx(mx = mx,sex = Sex[1])), by = list(PopName,Sex,Year)]
#substract life expectancy
e0 <- Data[Age==0,]

#### label sexes
DK.cv$Sex1[DK.cv$Sex=="f"]               <- "Females"
DK.cv$Sex1[DK.cv$Sex=="m"]               <- "Males"
DK.SD$Sex1[DK.SD$Sex=="f"]               <- "Females"
DK.SD$Sex1[DK.SD$Sex=="m"]               <- "Males"
DK.Gini$Sex1[DK.Gini$Sex=="f"]           <- "Females"
DK.Gini$Sex1[DK.Gini$Sex=="m"]           <- "Males"

# Trends Analysis ---------------------------------------------------------
# have a graph for each measure over time
col.male   <- brewer.pal(7,"Blues")[c(7)]
col.female <- brewer.pal(7,"Reds")[c(7)]


#####################################################################################################
fig.e0.male <-       xyplot(ex ~ Year, data = e0[e0$Sex == 'm'], type = 'l',main = expression('Life expectancy'),
                            key = list(x=.05,y=.95, title="Sex",background="white",
                                       text=list(c("Females","Males"))
                                       ,cex=1,points=list(pch=c(19),col=c(col.female,col.male))),
                            group=PopName,pch=c(19),
                            col=makeTransparent(col.male,100),
                            ylim=c(22,88),
                            ylab=list("Years",cex=1.5),
                            panel = function(x, y, ...){            
                              panel.abline(v=c(seq(1900,2000,20)),col='dark grey',lty=3)
                              panel.abline(h=c(seq(30,90,10)),col='dark grey',lty=3)                                                                                                          
                              panel.xyplot(x, y, ...)         
                            })
fig.e0.female <-       xyplot(ex ~ Year, data = e0[e0$Sex == 'f'], type = 'l',main = expression('Life expectancy'),
                            group=PopName,pch=c(19),
                            col=makeTransparent(col.female,100),
                            ylim=c(22,88),
                            ylab=list("Years",cex=1.5),
                            panel = function(x, y, ...){            
                              panel.abline(v=c(seq(1900,2000,20)),col='dark grey',lty=3)
                              panel.abline(h=c(seq(30,90,10)),col='dark grey',lty=3)                                                                                                          
                              panel.xyplot(x, y, ...)         
                            })

Fig.e0 <- fig.e0.male+fig.e0.female


############################## Figure with lbar
fig.lbar.male <-       xyplot(lbar ~ Year, data = my.lbar[my.lbar$Sex == 'm'], type = 'l',
                              main = expression('Lifespan equality logG'),
                            #key = list(x=.05,y=.95, title="Sex",background="white",
                            #           text=list(c("Females","Males"))
                            #          ,cex=1,points=list(pch=c(19),col=c(col.female,col.male))),
                            group=PopName,pch=c(19),
                            col=makeTransparent(col.male,100),
                            ylim=c(.4,.95),
                            ylab=list("Years",cex=1.5),
                            panel = function(x, y, ...){            
                              panel.abline(v=c(seq(1900,2000,20)),col='dark grey',lty=3)
                              panel.abline(h=c(seq(.4,1,.1)),col='dark grey',lty=3)                                                                                                          
                              panel.xyplot(x, y, ...)         
                            })
fig.lbar.female <-       xyplot(lbar ~ Year, data = my.lbar[my.lbar$Sex == 'f'], type = 'l',
                                main = expression('Lifespan equality logG'),
                              group=PopName,pch=c(19),
                              col=makeTransparent(col.female,100),
                              ylim=c(.4,.95),
                              ylab=list("Years",cex=1.5),
                              panel = function(x, y, ...){            
                                panel.abline(v=c(seq(1900,2000,20)),col='dark grey',lty=3)
                                panel.abline(h=c(seq(.4,1,.1)),col='dark grey',lty=3)                                                                                                          
                                panel.xyplot(x, y, ...)         
                              })

Fig.lbar <- fig.lbar.male+fig.lbar.female


#####################################################################################################
fig.cv.male <-       xyplot(cv ~ Year, data = my.cv[my.cv$Sex == 'm'], type = 'l',
                              main = expression('Lifespan equality (-log(coefficient of variation))'),
                            #key = list(x=.05,y=.95, title="Sex",background="white",
                                         #            text=list(c("Females","Males"))
                            #            ,cex=1,points=list(pch=c(19),col=c(col.female,col.male))),
                              group=PopName,pch=c(19),
                              col=makeTransparent(col.male,100),
                              ylim=c(0,2),
                              ylab=list("Years",cex=1.5),
                              panel = function(x, y, ...){            
                                panel.abline(v=c(seq(1900,2000,20)),col='dark grey',lty=3)
                                panel.abline(h=c(seq(0,2,.5)),col='dark grey',lty=3)                                                                                                          
                                panel.xyplot(x, y, ...)         
                              })
fig.cv.female <-       xyplot(cv ~ Year, data = my.cv[my.cv$Sex == 'f'], type = 'l',
                              main = expression('Lifespan equality (-log(coefficient of variation))'),
                                group=PopName,pch=c(19),
                                col=makeTransparent(col.female,100),
                              ylim=c(0,2),
                                ylab=list("Years",cex=1.5),
                                panel = function(x, y, ...){            
                                  panel.abline(v=c(seq(1900,2000,20)),col='dark grey',lty=3)
                                  panel.abline(h=c(seq(0,2,.5)),col='dark grey',lty=3)                                                                                                          
                                  panel.xyplot(x, y, ...)         
                                })

Fig.cv <- fig.cv.male+fig.cv.female


#####################################################################################################
fig.leq.male <-       xyplot(ls.eq ~ Year, data = my.eta[my.eta$Sex == 'm'], type = 'l',
                             main= expression(paste('Lifespan equality (', eta,')')),
                           #key = list(x=.05,y=.95, title="Sex",background="white",
                            #           text=list(c("Females","Males"))
                             #          ,cex=1,points=list(pch=c(19),col=c(col.female,col.male))),
                            group=PopName,pch=c(19),
                            col=makeTransparent(col.male,100),
                            ylim=c(0,2.2),
                            ylab=list("Years",cex=1.5),
                            panel = function(x, y, ...){            
                              panel.abline(v=c(seq(1900,2000,20)),col='dark grey',lty=3)
                              panel.abline(h=c(seq(0,2,.5)),col='dark grey',lty=3)                                                                                                          
                              panel.xyplot(x, y, ...)         
                            })
fig.leq.female <-       xyplot(ls.eq ~ Year, data = my.eta[my.eta$Sex == 'f'], type = 'l',
                               main= expression(paste('Lifespan equality (', eta,')')),
                              group=PopName,pch=c(19),
                              col=makeTransparent(col.female,100),
                              ylim=c(0,2.2),
                              ylab=list("Years",cex=1.5),
                              panel = function(x, y, ...){            
                                panel.abline(v=c(seq(1900,2000,20)),col='dark grey',lty=3)
                                panel.abline(h=c(seq(0,2,.5)),col='dark grey',lty=3)                                                                                                          
                                panel.xyplot(x, y, ...)         
                              })

Fig.leq <- fig.leq.male+fig.leq.female



############################## Figure with log(lbar)
fig.G.male <-       xyplot(log.G ~ Year, data = my.log.G[my.log.G$Sex == 'm'], type = 'l',
                              main = expression('Lifespan equality (-log(G))'),
                              #key = list(x=.05,y=.95, title="Sex",background="white",
                              #           text=list(c("Females","Males"))
                              #          ,cex=1,points=list(pch=c(19),col=c(col.female,col.male))),
                              group=PopName,pch=c(19),
                              col=makeTransparent(col.male,100),
                              ylim=c(.5,2.75),
                              ylab=list("Years",cex=1.5),
                              panel = function(x, y, ...){            
                                panel.abline(v=c(seq(1900,2000,20)),col='dark grey',lty=3)
                                panel.abline(h=c(seq(.5,2.5,.5)),col='dark grey',lty=3)                                                                                                          
                                panel.xyplot(x, y, ...)         
                              })

fig.G.female <-       xyplot(log.G ~ Year, data = my.log.G[my.log.G$Sex == 'f'], type = 'l',
                             main = expression('Lifespan equality (-log(G))'),
                                group=PopName,pch=c(19),
                                col=makeTransparent(col.female,100),
                                ylim=c(.5,2.75),
                                ylab=list("Years",cex=1.5),
                                panel = function(x, y, ...){            
                                  panel.abline(v=c(seq(1900,2000,20)),col='dark grey',lty=3)
                                  panel.abline(h=c(seq(.5,2.5,.5)),col='dark grey',lty=3)                                                                                                          
                                  panel.xyplot(x, y, ...)         
                                })

Fig.G <- fig.G.male+fig.G.female



require(gridExtra)
pdf(file="R/Figures/Figure_e0_and_lifespanEq.pdf",width=13,height=7.5,pointsize=4)
grid.arrange(Fig.e0,Fig.leq, ncol=2)
grid.arrange(Fig.e0,Fig.G, ncol=2)
grid.arrange(Fig.e0,Fig.cv, ncol=2)
grid.arrange(Fig.e0,Fig.lbar, ncol=2)
dev.off()

# Won't use lbar anymore, eta will be the main one in the paper. We will replicate everything for -log(G) and -log(CV)
# as sensitivity analyses

# Construct first graph of the manuscript;
# Note: the periods are taken arbitrarly, should find a much intelligent way of sibsetting.

##################### Plot both together
##### Order all the datasets equally
e0         <- e0[with(e0,order(PopName,Sex,Year)),]
my.eta     <- my.eta[with(my.eta,order(PopName,Sex,Year)),]
my.log.G   <- my.log.G[with(my.log.G,order(PopName,Sex,Year)),]
my.cv      <- my.cv[with(my.cv,order(PopName,Sex,Year)),]


## check consistency with the data sets
Results      <- e0
Results$leq  <- my.eta$ls.eq
Results$log.G<- my.log.G$log.G
Results$cv   <- my.cv$cv

save(Results, file = "Data/Results.Rdata")

# Recode periods, just for visualization
Results$Period                                           <- 0
Results$Period[Results$Year<1921]                        <- 1
Results$Period[Results$Year>=1921 & Results$Year<=1959]  <- 2
Results$Period[Results$Year>=1960]                       <- 3
Results$Period <- factor(Results$Period,levels=c(1:3),labels=c("1900-1921","1921-1959","1960 onwards"))

### Now create the plot
range(Results$leq)
range(Results$ex)
Fig1.eta <- ggplot(data = Results, aes(x = ex, y = leq,color=Period)) +  
  geom_point(alpha=I(1/3))+
  geom_line(data = subset(Results, PopName=="JPN" & Sex=="f"),aes(x = ex, y = leq),colour="black",size=1.5, lty=1)+
  #geom_line(data = subset(Results, PopName=="USA" & Sex=="f"),aes(x = ex, y = leq),colour="black",size=2,lty=2)+
  #geom_line(data = subset(Results, PopName=="RUS" & Sex=="m"),aes(x = ex, y = leq),colour="red",size=2,lwd=1)+
  scale_x_continuous(expression("Life expectancy"), limits=c(23,90))+
  scale_colour_manual(values=c(col.female, "grey",col.male))+
  scale_y_continuous(expression("Lifespan equality"), limits=c(-.01,2.20))+
  theme(legend.key.height=unit(2,"line"))+  
  theme(legend.position = c(0.15, 0.85))+
  ggtitle(expression(paste("Life expectancy (", e[0],") vs lifespan equality (",eta,')')))+
  theme(text = element_text(size = 15))+
  #stat_smooth(method = "lm",formula = y ~ x + I(x^2),size=.5,col="black")+ # If I want a quadratic line
  #geom_smooth(data=lbar,aes(x = lbar, y = e0,color=D), method = "lm", se=FALSE,col="black") + # if I want a linear one
  #eliminates background, gridlines, and chart border
  theme(plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,legend.key = element_blank()
    ,axis.line.x = element_line(color="black", size = .5)
    ,axis.line.y = element_line(color="black", size = .5))
 
previous_theme <- theme_set(theme_bw())

Fig1.eta

range(Results$log.G)

Fig1.log.G <- ggplot(data = Results, aes(x = ex, y = log.G,color=Period)) +  
  geom_point(alpha=I(1/3))+
  geom_line(data = subset(Results, PopName=="JPN" & Sex=="f"),aes(x = ex, y = log.G),colour="black",size=1.5, lty=1)+
  scale_x_continuous(expression("Life expectancy"), limits=c(23,90))+
  scale_colour_manual(values=c(col.female, "grey",col.male))+
  scale_y_continuous(expression("Lifespan equality"), limits=c(.6,2.65))+
  theme(legend.key.height=unit(2,"line"))+  
  theme(legend.position = c(0.15, 0.85))+
  ggtitle(expression(paste("Life expectancy (", e[0],") vs lifespan equality (",-log(G),')')))+
  theme(text = element_text(size = 15))+
  #stat_smooth(method = "lm",formula = y ~ x + I(x^2),size=.5,col="black")+ # If I want a quadratic line
  #geom_smooth(data=lbar,aes(x = lbar, y = e0,color=D), method = "lm", se=FALSE,col="black") + # if I want a linear one
  #eliminates background, gridlines, and chart border
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,legend.key = element_blank()
        ,axis.line.x = element_line(color="black", size = .5)
        ,axis.line.y = element_line(color="black", size = .5))

previous_theme <- theme_set(theme_bw())

Fig1.log.G


range(Results$cv)

Fig1.cv<- ggplot(data = Results, aes(x = ex, y = cv,color=Period)) +  
  geom_point(alpha=I(1/3))+
  geom_line(data = subset(Results, PopName=="JPN" & Sex=="f"),aes(x = ex, y = cv),colour="black",size=1.5, lty=1)+
  scale_x_continuous(expression("Life expectancy"), limits=c(23,90))+
  scale_colour_manual(values=c(col.female, "grey",col.male))+
  scale_y_continuous(expression("Lifespan equality"), limits=c(-.012,1.96))+
  theme(legend.key.height=unit(2,"line"))+  
  theme(legend.position = c(0.15, 0.85))+
  ggtitle(expression(paste("Life expectancy (", e[0],") vs lifespan equality (",-log(CV),')')))+
  theme(text = element_text(size = 15))+
  #stat_smooth(method = "lm",formula = y ~ x + I(x^2),size=.5,col="black")+ # If I want a quadratic line
  #geom_smooth(data=lbar,aes(x = lbar, y = e0,color=D), method = "lm", se=FALSE,col="black") + # if I want a linear one
  #eliminates background, gridlines, and chart border
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,legend.key = element_blank()
        ,axis.line.x = element_line(color="black", size = .5)
        ,axis.line.y = element_line(color="black", size = .5))

previous_theme <- theme_set(theme_bw())

Fig1.cv

require(gridExtra)
pdf(file="R/Figures/Scatter_e0_vs_equality.pdf",width=8,height=8,pointsize=4,useDingbats = F)
grid.arrange(Fig1.eta,ncol=1)
grid.arrange(Fig1.log.G,ncol=1)
grid.arrange(Fig1.cv,ncol=1)
dev.off()

#### Figure for Paa presentation

##################### Plot both together
##### Order all the datasets equally
e0         <- e0[with(e0,order(PopName,Sex,Year)),]
my.eta     <- my.eta[with(my.eta,order(PopName,Sex,Year)),]
my.log.G   <- my.log.G[with(my.log.G,order(PopName,Sex,Year)),]
my.cv      <- my.cv[with(my.cv,order(PopName,Sex,Year)),]


## check consistency with the data sets
Results      <- e0
Results$leq  <- my.eta$ls.eq
Results$log.G<- my.log.G$log.G
Results$cv   <- my.cv$cv

save(Results, file = "Data/Results.Rdata")

# Recode periods, just for visualization
Results$Period                                           <- 0
Results$Period[Results$Year<1921]                        <- 1
Results$Period[Results$Year>=1921 & Results$Year<=1959]  <- 2
Results$Period[Results$Year>=1960]                       <- 3
Results$Period <- factor(Results$Period,levels=c(1:3),labels=c("1900-1921","1921-1959","1960 onwards"))

### Now create the plot
range(Results$leq)
range(Results$ex)
Fig1.eta <- ggplot(data = Results, aes(x = ex, y = leq,color=Period)) +  
  geom_point(alpha=I(1/3))+
  #geom_line(data = subset(Results, PopName=="USA" & Sex=="f"),aes(x = ex, y = leq),colour="black",size=2,lty=2)+
  #geom_line(data = subset(Results, PopName=="RUS" & Sex=="m"),aes(x = ex, y = leq),colour="red",size=2,lwd=1)+
  scale_x_continuous(expression("Life expectancy"), limits=c(23,90))+
  scale_colour_manual(values=c(col.female, "grey",col.male))+
  scale_y_continuous(expression("Lifespan equality"), limits=c(-.01,2.20))+
  theme(legend.key.height=unit(2,"line"))+  
  theme(legend.position = c(0.15, 0.85))+
  ggtitle(expression(paste("Life expectancy (", e[0],") vs lifespan equality (",eta,')')))+
  theme(text = element_text(size = 15))+
  #stat_smooth(method = "lm",formula = y ~ x + I(x^2),size=.5,col="black")+ # If I want a quadratic line
  #geom_smooth(data=lbar,aes(x = lbar, y = e0,color=D), method = "lm", se=FALSE,col="black") + # if I want a linear one
  #eliminates background, gridlines, and chart border
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,legend.key = element_blank()
        ,axis.line.x = element_line(color="black", size = .5)
        ,axis.line.y = element_line(color="black", size = .5))

previous_theme <- theme_set(theme_bw())

Fig1.eta


Fig1.log.G.paa4 <- ggplot(data = Results, aes(x = ex, y = log.G,color=Period)) +  
  geom_point(data = subset(Results, PopName=="JPN" & Sex=="f" & Year == 2000),aes(x = ex, y = log.G),colour="red",size=3, lty=1)+
  geom_point(data = subset(Results, PopName=="RUS" & Sex=="m" & Year == 1994),aes(x = ex, y = log.G),colour="blue",size=3, lty=1)+
  scale_x_continuous(expression("Life expectancy"), limits=c(37,90))+
  scale_colour_manual(values=c(col.female, "grey",col.male))+
  scale_y_continuous(expression("Lifespan equality"), limits=c(.8,2.65))+
  theme(legend.key.height=unit(2,"line"))+  
  theme(legend.position = c(0.15, 0.85))+
  ggtitle(expression(paste("Life expectancy (", e[0],") vs lifespan equality (",bar(l),')')))+
  theme(text = element_text(size = 15))+
  stat_smooth(method = "lm",formula = y ~ x,size=1.5,col="black",lty=1)+
  #stat_smooth(method = "lm",formula = y ~ x + I(x^2),size=.5,col="black")+ # If I want a quadratic line
  #geom_smooth(data=lbar,aes(x = lbar, y = e0,color=D), method = "lm", se=FALSE,col="black") + # if I want a linear one
  #eliminates background, gridlines, and chart border
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,legend.key = element_blank()
        ,axis.line.x = element_line(color="black", size = .5)
        ,axis.line.y = element_line(color="black", size = .5))

previous_theme <- theme_set(theme_bw())


Fig1.log.G.paa4


require(gridExtra)
pdf(file="R/Figures/Model_PAA.pdf",width=8,height=8,pointsize=4,useDingbats = F)
grid.arrange(Fig1.log.G.paa1,ncol=1)
grid.arrange(Fig1.log.G.paa2,ncol=1)
grid.arrange(Fig1.log.G.paa3,ncol=1)
grid.arrange(Fig1.log.G.paa4,ncol=1)

dev.off()

# First differences Analysis ----------------------------------------------


#Calculate differences on life expectancy and lifespan equality indicators
Dif.data           <- Results[,list(dif.le = diff(ex)), by = list(PopName,Sex)]
Dif.data$dif.log.G <- Results[,list(x = diff(log.G)), by = list(PopName,Sex)]$x
Dif.data$dif.eta   <- Results[,list(x = diff(leq)), by = list(PopName,Sex)]$x
Dif.data$dif.cv    <- Results[,list(x = diff(cv)), by = list(PopName,Sex)]$x
Dif.data$Year      <- Results[,list(x = Y(Year,lag.2 = 1)), by = list(PopName,Sex)]$x

# get correlation coefficient for the manuscript
p.c.1         <- cor(Results$ex,Results$leq,use="pairwise.complete.obs")
p.c.2         <- cor(Results$ex, Results$log.G,use="pairwise.complete.obs")
p.c.3         <- cor(Results$ex, Results$cv,use="pairwise.complete.obs")

print(paste0(paste0('eta=',round(p.c.1,2)),paste0('log.G=',round(p.c.2,2)),paste0('cv=',round(p.c.3,2))))

# Now run linear model on first differences and calculate correlation
# remember to change the values in the paper, y= .007615 with R^2=.8412 ans Pearson correlation coefficiente of .9143109
#lm.lbar           <- lm(dif.lbar ~ dif.le -1 , data = Dif.data)
#p.c.1         <- cor(Dif.data$dif.le, Dif.data$dif.lbar,use="pairwise.complete.obs")


# Now make figure 2 in the paper

Dif.data$Period[Dif.data$Year<1921] <- 1
Dif.data$Period[Dif.data$Year >= 1921 & Dif.data$Year <= 1959] <- 2
Dif.data$Period[Dif.data$Year >= 1960] <- 3
Dif.data$Period <- factor(Dif.data$Period,levels=c(1:3),labels=c("1900-1921","1921-1959","1960 onwards"))

save(Dif.data,file= 'Data/first differences results.RData')


#### Figure of first differences for the various lifespan equality measures

my.col.f <- c(col.female,
              "darkgrey",
              col.male)



R.sq <- as.character(round(summary(lm(Dif.data$dif.le ~ Dif.data$dif.eta - 1))$r.squared,3))
Fig.first.dif.eta <- ggplot(data = Dif.data, aes(y = dif.eta, x = dif.le,color=Period)) +  
  annotate("text", label = paste ('R square =', R.sq), x = -7, y = -.4, size = 4, colour = "black")+
  geom_point(alpha=I(1/3))+
  scale_x_continuous(expression(group('(',Delta*e[0],')')), limits=c(-10,10))+
  scale_colour_manual(values=my.col.f)+
  scale_y_continuous(expression(group('(',Delta*eta,')')), limits=c(-.5,.5))+
  theme(legend.key.height=unit(2,"line"))+  
  theme(legend.position = c(0.15, 0.85))+
  ggtitle("Changes in life expectancy  and lifespan equality")+
  theme(text = element_text(size = 15))+
  stat_smooth(method = "lm",formula = y ~ x-1,size=.5,col="black",lty=2)+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  theme(plot.background = element_blank())

previous_theme <- theme_set(theme_bw())

Fig.first.dif.eta

R.sq <- as.character(round(summary(lm(Dif.data$dif.le ~ Dif.data$dif.log.G - 1))$r.squared,3))
Fig.first.dif.log.G <- ggplot(data = Dif.data, aes(y = dif.log.G, x = dif.le,color=Period)) + 
  annotate("text", label = paste ('R square =', R.sq), x = -7, y = -.4, size = 4, colour = "black")+
  geom_point(alpha=I(1/3))+
  scale_x_continuous(expression(group('(',Delta*e[0],')')), limits=c(-10,10))+
  scale_colour_manual(values=my.col.f)+
  scale_y_continuous(expression(group('(',Delta*(-logG),')')), limits=c(-.42,.42))+
  theme(legend.key.height=unit(2,"line"))+  
  theme(legend.position = c(0.15, 0.85))+
  ggtitle("Changes in life expectancy  and lifespan equality")+
  theme(text = element_text(size = 15))+
  stat_smooth(method = "lm",formula = y ~ x-1,size=.5,col="black",lty=2)+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  theme(plot.background = element_blank())

previous_theme <- theme_set(theme_bw())

Fig.first.dif.log.G


R.sq <- as.character(round(summary(lm(Dif.data$dif.le ~ Dif.data$dif.cv - 1))$r.squared,3))
Fig.first.dif.cv <- ggplot(data = Dif.data, aes(y = dif.cv, x = dif.le,color=Period)) + 
  annotate("text", label = paste ('R square =', R.sq), x = -7, y = -.3, size = 4, colour = "black")+
  geom_point(alpha=I(1/3))+
  scale_x_continuous(expression(group('(',Delta*e[0],')')), limits=c(-10,10))+
  scale_colour_manual(values=my.col.f)+
  scale_y_continuous(expression(group('(',Delta*(-logCV),')')), limits=c(-.3,.3))+
  theme(legend.key.height=unit(2,"line"))+  
  theme(legend.position = c(0.15, 0.85))+
  ggtitle("Changes in life expectancy  and lifespan equality")+
  theme(text = element_text(size = 15))+
  stat_smooth(method = "lm",formula = y ~ x-1,size=.5,col="black",lty=2)+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  theme(plot.background = element_blank())

previous_theme <- theme_set(theme_bw())

Fig.first.dif.cv


require(gridExtra)
pdf(file="R/Figures/Scatter_1stDif.pdf",width=9,height=8,pointsize=4)
grid.arrange(Fig.first.dif.eta, ncol=1)
grid.arrange(Fig.first.dif.log.G, ncol=1)
grid.arrange(Fig.first.dif.cv, ncol=1)
dev.off()

################# Quantify proportions of the quadrants by decade
################# First code all decades

Dif.data$decade         <- cut(Dif.data$Year,breaks = c(seq(1900,2020,by = 10)))
Dif.data$quadrant.eta   <- 4
Dif.data$quadrant.log.G <- 4
Dif.data$quadrant.cv    <- 4

Dif.data[Dif.data$dif.eta < 0 &  Dif.data$dif.le < 0]$quadrant.eta       <- 1
Dif.data[Dif.data$dif.log.G < 0 &  Dif.data$dif.le < 0]$quadrant.log.G   <- 1
Dif.data[Dif.data$dif.cv < 0 &  Dif.data$dif.le < 0]$quadrant.cv         <- 1

Dif.data[Dif.data$dif.eta < 0 &  Dif.data$dif.le >= 0]$quadrant.eta       <- 2
Dif.data[Dif.data$dif.log.G < 0 &  Dif.data$dif.le >= 0]$quadrant.log.G   <- 2
Dif.data[Dif.data$dif.cv < 0 &  Dif.data$dif.le >= 0]$quadrant.cv         <- 2

Dif.data[Dif.data$dif.eta >= 0 &  Dif.data$dif.le > 0]$quadrant.eta       <- 3
Dif.data[Dif.data$dif.log.G >= 0 &  Dif.data$dif.le > 0]$quadrant.log.G   <- 3
Dif.data[Dif.data$dif.cv >= 0 &  Dif.data$dif.le > 0]$quadrant.cv         <- 3

table.eta           <- table(Dif.data$decade,Dif.data$quadrant.eta)
rownames(table.eta) <- c(seq(1900,2010,by = 10),'W?')
table.eta           <- table.eta/rowSums(table.eta)*100

table.log.G           <- table(Dif.data$decade,Dif.data$quadrant.log.G)
rownames(table.log.G) <- c(seq(1900,2010,by = 10),'W?')
table.log.G           <- table.log.G/rowSums(table.log.G)*100

table.CV           <- table(Dif.data$decade,Dif.data$quadrant.cv)
rownames(table.CV) <- c(seq(1900,2010,by = 10),'W?')
table.CV           <- table.CV/rowSums(table.CV)*100
y <- dim(table.CV)[1]



same.eta       <- table.eta[,1] +  table.eta[,3]
same.log.G     <- table.log.G[,1] +  table.log.G[,3]
same.CV        <- table.CV[,1] +  table.CV[,3]

round(100-same.eta[-y],2)
round(100-same.log.G[-y],2)
round(100-same.CV[-y],2)
