
library(ggplot2)
library(data.table)
library(reshape2)

setwd("C:/Users/jmaburto/Documents/GitHub/Lifespan-inequality-Denmark")
# 1. 'Infectious, non-R',
# 2. 'Cancer AS',
# 3. 'Cancer NAS',
# 4. 'Diabetes',
# 5. 'Cardiovascular',
# 6. 'Respiratory I',
# 7. 'Respiratory NI',
# 8. 'External',
# 9. 'Other'
#

HMD <- get(load('Data/HMD_Data.RData'))

## subset the countries that I am interested in
unique(HMD$PopName)

Countries <- c('DNK', 'SWE', 'NOR')

Data <- HMD[HMD$PopName %in% Countries,]
gdata::keep(Data,Countries,sure=T)

source('R/Functions_1.R')
get(load('Data/DT_COD.RData'))



### Just for PAA
DKN <- read.table('Data/DK_Mx_5x1.txt',header = T,stringsAsFactors = F)
DKN <- data.table(DKN)
mx  <- DKN[DKN$Year >= 1960,c('Year','Age', 'mx')]

COD <- DT_COD.melt[DT_COD.melt$Year >= 1960 & DT_COD.melt$Year <= 2014 & DT_COD.melt$Country.name =='Denmark' & DT_COD.melt$Sex=='f', ]
unique(DT_COD.melt$Age)
unique(DT_COD.melt$Year)
unique(mx$Age)
Years <- unique(mx$Year)

Results <- NULL
#i <- 1960

for (i in Years[-length(Years)]){
  mx1 <- mx[Year==i,]
  mx2 <- mx[Year==i+1,]
  
  COD1 <- COD[COD$Year == i,]
  COD2 <- COD[COD$Year == i+1,]
  
  m0 <- matrix(0,6,8)
  m0 <- cbind(m0,1)
  rownames(m0) <- seq(85,110,5)
  
  m1 <- acast(data = COD1,formula = Age ~ Cat,value.var = 'Dx.p')
  m1 <- rbind(m1,m0)
  
  m2 <- acast(data = COD2,formula = Age ~ Cat,value.var = 'Dx.p')
  m2 <- rbind(m2,m0)
  
  m1 <- m1*mx1$mx
  m2 <- m2*mx2$mx
  
  Contribution <- Decomp(cvfrommxc,rates1 = c(m1), rates2 = c(m2), N = 50, sex = 'f')
  
  dim(Contribution) <- dim(m1)
  rownames(Contribution) <- rownames(m1)
  
  R1 <- melt(Contribution,varnames = c('Age', 'Cause'), value.name = 'Contribution')
  R1$Year <- i
  
  Results <- rbind(Results, R1)
  
}

Results$Cause <- as.factor(Results$Cause)
levels(Results$Cause) <- Category.labels

cut(ro.melt$year+1, breaks=c(seq(1750,2010,10),Inf),labels=as.character(seq(1750,2010,10)))

Results$Period <- cut(Results$Year+1, breaks = c(1960,1975,1995,Inf), labels = c('1960-1975',
                                                                                 '1975-1995',
                                                                                 '1995-2013'))
                      

Results <- data.table(Results)
Results2 <- Results[, sum(Contribution), by = list(Period,Age,Cause)]



save(Results, file = 'Data/Resuls_PAA.RData')

library(RColorBrewer)
display.brewer.all()
base2 <- c(1:15)
base2 <- c(rev(brewer.pal(8,name = 'Spectral'))[1:5],rev(brewer.pal(8,name = 'Spectral'))[8],rev(brewer.pal(8,name = 'Spectral'))[7],'green', 'lightgrey')
base3 <- c(base2[5],base2[6],base2[7],base2[8],base2[4],base2[1],base2[2],base2[3],base2[9])

Category.labels2 <-c("Infectious, non-Respiratory", "Cancer-Amenable to Smoking", "Cancer-Non amenable","Diabetes","Cardiovascular","Respiratory-Infectious",
    "Respiratory-Non infectious","External","Rest")

f1 <- ggplot(Results2[Results2$Age!=0], aes(x = factor(Age), y = V1, fill = Cause)) +
  ggtitle('Contributions to changes in lifespan inequality by age and cause of death' )+
  scale_fill_manual(name= ' Cause of death (Contribution)',values=base3,labels = Category.labels2)+
  geom_bar(aes(group = Cause), stat = "identity",position = "stack")+
  theme_light()+
  facet_wrap(~Period)+
  theme(text = element_text(size=12),
        axis.text.x = element_text(angle=45, hjust=1))+
  labs(x = "Age group", y = "Contribution (years)",size=12)+
  theme(axis.title.y = element_text(size = 12, angle = 90))+
  theme(axis.title.x = element_text(size = 12, angle = 00))+
  theme(text = element_text(size=14),
        strip.text.x = element_text(size = 14, colour = "black"))+
  theme(legend.position = c(.75,.15))+
  geom_hline(yintercept = 0)+coord_flip()
f1

require(gridExtra)
pdf(file="R/Figures/Decomp.pdf",width=18,height=8,pointsize=4,useDingbats = F)
f1
dev.off()


ex.males1


# Get total for the country
#mx <- DT.mxCOD[year==1995 & sex == 1 & state==0,]$mx
#sex = 'f'

#training data
#DT.mxCOD <- DT.mxCOD[state == 0,]
#training.data

#get decompositin of changes in life expectancy and life disparity by age and cause of death

# some useful indicators
states <- sort(unique(DT.mxCOD$state))
sexes  <- sort(unique(DT.mxCOD$sex))
years  <- sort(unique(DT.mxCOD$year))
ages   <- sort(unique(DT.mxCOD$age))
causes <- colnames(DT.mxCOD)[8:22]
Empty  <- matrix(0,nrow = length(ages), ncol = length(causes), dimnames = list(ages,causes)) 

#st <- 0
#yr <- 1995
# get matrices in lists to do faster the calculation
Mat.list <- lapply(sexes, function(sx,LTC2,years,Empty,ages,causes){
  LTC    <- LTC2[LTC2$sex == sx,]
Sex.List <- lapply(states, function(st,LTC,Empty,years,ages,causes){
  Mat    <- LTC[LTC$state == st,]
  YRlist <- lapply(years, function(yr,Mat,Empty,causes,ages){
    Mat2           <- as.matrix(Mat[Mat$year == yr,c(causes), with = F])
    rownames(Mat2) <- ages
    Empty[rownames(Mat2),colnames(Mat2)] <- Mat2
    Empty
  }, Mat = Mat, Empty = Empty, causes = causes, ages= ages)
  names(YRlist) <- years
  YRlist
}, LTC = LTC, years=years, Empty = Empty, causes = causes, ages=ages)
names(Sex.List) <- states
Sex.List
},LTC2 = DT.mxCOD, years=years, Empty = Empty, causes = causes, ages=ages)
names(Mat.list) <- sexes



# sex 
#x <- Mat.list[[as.character(1)]]
# state
#y <- x[[as.character(0)]]
# year 
#z <- y[[as.character(1995)]]
#i <- 1
#j <- 0
#years <- 1995:1996
#yr <- 1995

#install.packages('devtools')
#library(devtools)
#install_github('nathanvan/parallelsugar')

library(parallelsugar)

Decomp.results.list <- list()
state.list          <- list()

system.time(


for (i in sexes){
  print(i)
  x <- Mat.list[[as.character(i)]]
  for (j in states){
    print(j)
    y <- x[[as.character(j)]]
    
    Decomp.list <- mclapply(years[-length(years)],function(yr,y,e0frommxc,i){
      
      contrib           <- mydecomp(func = e0frommxc,
                          rates1 = c(y[[as.character(yr)]]),
                          rates2 = c(y[[as.character(yr+1)]]),N = 50,sex = i)
      dim(contrib)      <- dim(y[[as.character(yr)]])
      dimnames(contrib) <- dimnames(y[[as.character(yr)]])
      contrib },y= y,e0frommxc = e0frommxc, i = i, mc.cores = 4)
    names(Decomp.list)  <- years[-length(years)]
    state.list[[as.character(j)]] <- Decomp.list
  }
  Decomp.results.list[[as.character(i)]]  <- state.list
}
)
save(Decomp.results.list, file =  'Data/DecompResults_ex_List.RData')
gc()
# bruger   system forløbet 
# 44.07    60.37  6009.63 




Decomp.results.edagger <- list()
state.list          <- list()

system.time(
  
  
  for (i in sexes){
    print(i)
    x <- Mat.list[[as.character(i)]]
    for (j in states){
      print(j)
      y <- x[[as.character(j)]]
      
      Decomp.list <- mclapply(years[-length(years)],function(yr,y,e0frommxc,i){
        
        contrib           <- mydecomp(func = e0frommxc,
                                      rates1 = c(y[[as.character(yr)]]),
                                      rates2 = c(y[[as.character(yr+1)]]),N = 50,sex = i)
        dim(contrib)      <- dim(y[[as.character(yr)]])
        dimnames(contrib) <- dimnames(y[[as.character(yr)]])
        contrib },y= y,e0frommxc = edaggerfrommxc, i = i, mc.cores = 4)
      names(Decomp.list)  <- years[-length(years)]
      state.list[[as.character(j)]] <- Decomp.list
    }
    Decomp.results.edagger[[as.character(i)]]  <- state.list
  }
)
save(Decomp.results.edagger, file =  'Data/DecompResults_ed_List.RData')
gc()
