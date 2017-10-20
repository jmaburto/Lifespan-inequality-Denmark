
#setwd( "C:/Users/jmaburto/Documents/GitHub/CoD-burden-on-LA/R/Decomp_App")
library(ggplot2)
library(DT)
library(plotly)
library(RColorBrewer)
load('Results.RData')

 # Data2    <- DT.Decomp.ex
 # state.ind   <- 'Denmark'
 # initial.ind <- 1975
 # final.ind   <- 1995
 # 
 # Data.fig <- single.ex
 # 
 # unique(Data.fig$Cause)[1]
 # 
 # cols <- rev(brewer.pal(11, 'RdYlBu'))
 # romi.m <- ggplot(Data.fig[Data.fig$Sex == 'Female'  & Data.fig$Cause == unique(Data.fig$Cause)[3] & Data.fig$Age >= 30, ], aes(Year, Age, z = Contribution )) +
 #   ggtitle('Age-specific contributiono to the change in lifespan inequality', subtitle = 'Females')+
 #   facet_wrap(~ Cause)+ 
 #   theme_light()+
 #   scale_x_continuous('Year', expand = c(0, 0)) +
 #   #scale_y_continuous('Age', expand = c(0, 0)) +
 #   geom_raster(aes(fill = Contribution),interpolate = T)+
 #   scale_fill_gradientn(colours = cols,name= 'Change (%)')
 # romi.m

getTable.function <- function(Data = DT.Decomp.ex,state = state.ind,initial = initial.ind,final = final.ind){
  
  Data.1 <- Data[Data$Year >= initial & Data$Year < final & Data$Country == state,]
  
  DT.out              <-  Data.1[, list(Contribution = sum(Contribution)), by = list(Country,Sex,Cause)]
  total               <- DT.out[,list(Contribution= sum(Contribution)),by = list(Country,Sex)]
  total$Cause         <- 'Total'
  total               <- total[,c('Country','Sex','Cause','Contribution')]
  DT.out              <- rbind(DT.out,total) 
  
  DT.out
}


shinyServer(
  function(input, output) {
    var1 <- reactive(seq(as.integer(input$initial.ind)+1,2014,1))

    output$vx <- renderUI({
      selectInput('final.ind','Final year',choices = var1(),selected = 1995)
    })
  
    
    output$e0.trends <- renderPlotly({
      
      Data2       <- Data
      Data2$ex    <- round(Data2$ex,2)
      
      p <-ggplot(Data2, aes(x = Year,y = ex,colour=(Country))) +
        ggtitle('Life expectancy at birth') +
        geom_line(aes(group = Country), size= 1) +
        facet_wrap(~Sex2)+
        theme_light()+
        labs(y = "Years")+
        scale_colour_manual('Country', values = c('blue', 'green', 'red')) + 
        theme(text = element_text(size=14),
              strip.text.x = element_text(size = 14, colour = "black"))
      #p
      print(ggplotly(p,width = 1350, height = 400))
      
    })
    
    output$ed.trends <- renderPlotly({
      
      Data2       <- Data
      
      
      q <-ggplot(Data2, aes(x = Year,y = cv,colour=(Country))) +
        ggtitle('Lifespan inequality (cv)') +
        geom_line(aes(group = Country), size= 1) +
        facet_wrap(~Sex2)+
        theme_light()+
        labs(y = "Years")+
        scale_colour_manual('Country', values = c('blue', 'green', 'red')) + 
        theme(text = element_text(size=14),
              strip.text.x = element_text(size = 14, colour = "black"))
      #p
      print(ggplotly(q,width = 1350, height = 400))
      
    })
    
    output$e0.decomp <- renderPlotly({
      
      state.ind   <- input$state.ind
      initial.ind <- input$initial.ind
      final.ind   <- input$final.ind
      Data        <- DT.Decomp.ex
      
      Data.fig   <- Data[Data$Year >= initial.ind & Data$Year < final.ind & Data$Country == state.ind, ]
      Data.fig   <- Data.fig[, list(Contribution = sum(Contribution)), by = list(Country,Sex,Cause,Age)] 
      #Data.fig        <- Data.fig[Data.fig$Age!= '0',]
      
      Total.Age <- Data.fig[,sum(Contribution), by = list(Age,Sex)]
      Total.Age$V1 <- round(Total.Age$V1,2)

      base2 <- c(rev(brewer.pal(8,name = 'Spectral'))[1:5],rev(brewer.pal(8,name = 'Spectral'))[8],'lightgrey','lightpink')
      Data.fig$Contribution <- round(Data.fig$Contribution,2)
      p <- ggplot(Data.fig, aes(x = Age, y = Contribution, fill = Cause)) +
        ggtitle('Decomposition of life expectancy (years)', subtitle = paste0(state.ind,', ', initial.ind,'-',final.ind))+
        facet_wrap(~Sex)+
        scale_fill_manual('Cause of death', values = base2) + 
        geom_bar(stat = "identity",position = "stack")+
        theme_light()+
        theme(text = element_text(size=10),
              axis.text.x = element_text(angle=45, hjust=1))+
        labs(x = " ", y = " ",size=10)+
        theme(text = element_text(size=10),
              strip.text.x = element_text(size = 12, colour = "black"))+
        geom_hline(yintercept = 0)
      #  coord_flip()
      rr <- ggplotly(p,width = 1200, height = 380)
      rr$x$layout$xaxis$title = "Age group"
      rr$x$layout$xaxis2$title = "Age group"
      
      print(rr)
      
    })
    
    output$ed.decomp <- renderPlotly({
      
      state.ind   <- input$state.ind
      initial.ind <- input$initial.ind
      final.ind   <- input$final.ind
      Data        <- DT.Decomp.cv
      
      Data.fig   <- Data[Data$Year >= initial.ind & Data$Year < final.ind & Data$Country == state.ind, ]
      Data.fig   <- Data.fig[, list(Contribution = sum(Contribution)), by = list(Country,Sex,Cause,Age)] 
      #Data.fig        <- Data.fig[Data.fig$Age!= '0-4',]
      
      Total.Age <- Data.fig[,sum(Contribution), by = list(Age,Sex)]
      Total.Age$V1 <- Total.Age$V1
      
      base2 <- c(rev(brewer.pal(8,name = 'Spectral'))[1:5],rev(brewer.pal(8,name = 'Spectral'))[8],'lightgrey','lightpink')
      Data.fig$Contribution <- Data.fig$Contribution
      q <- ggplot(Data.fig, aes(x = Age, y = Contribution, fill = Cause)) +
        ggtitle('Decomposition of lifespan inequality', subtitle = paste0(state.ind,', ', initial.ind,'-',final.ind))+
        facet_wrap(~Sex)+
        scale_fill_manual('Cause of death', values = base2) + 
        geom_bar(stat = "identity",position = "stack")+
        theme_light()+
        theme(text = element_text(size=10),
              axis.text.x = element_text(angle=45, hjust=1))+
        labs(x = " ", y = " ",size=10)+
        theme(text = element_text(size=10),
              strip.text.x = element_text(size = 12, colour = "black"))+
        geom_hline(yintercept = 0)
      #  coord_flip()
      r <- ggplotly(q,width = 1200, height = 380)
      r$x$layout$xaxis$title = "Age group"
      r$x$layout$xaxis2$title = "Age group"
      
      
      print(r)
      
      
      
    })
    
    output$ex.compare <- renderPlotly({
      
      state.ind   <- input$state.ind
      Yr          <- input$year.compare
      final.ind   <- input$final.ind
      Data        <- DT.compare.ex
      
      Data.fig   <- Data[Data$Year == Yr & Data$Country == state.ind, ]
      
      base2 <- c(rev(brewer.pal(8,name = 'Spectral'))[1:5],rev(brewer.pal(8,name = 'Spectral'))[8],'lightgrey','lightpink')
      Data.fig$Contribution <- Data.fig$Contribution
      q <- ggplot(Data.fig, aes(x = Age, y = Contribution, fill = Cause)) +
        ggtitle('Decomposition of life expectancy (years)', subtitle = paste0(state.ind,'vs Sweden, ', Yr))+
        facet_wrap(~Sex)+
        scale_fill_manual('Cause of death', values = base2) + 
        geom_bar(stat = "identity",position = "stack")+
        theme_light()+
        theme(text = element_text(size=10),
              axis.text.x = element_text(angle=45, hjust=1))+
        labs(x = " ", y = " ",size=10)+
        theme(text = element_text(size=10),
              strip.text.x = element_text(size = 12, colour = "black"))+
        geom_hline(yintercept = 0)
      #  coord_flip()
      r <- ggplotly(q,width = 1200, height = 380)
      r$x$layout$xaxis$title = "Age group"
      r$x$layout$xaxis2$title = "Age group"
      
      print(r)
    })
    
    output$cv.compare <- renderPlotly({
      
      state.ind   <- input$state.ind
      Yr          <- input$year.compare
      final.ind   <- input$final.ind
      Data        <- DT.compare.cv
      
      Data.fig   <- Data[Data$Year == Yr & Data$Country == state.ind, ]
      
      
      base2 <- c(rev(brewer.pal(8,name = 'Spectral'))[1:5],rev(brewer.pal(8,name = 'Spectral'))[8],'lightgrey','lightpink')
      Data.fig$Contribution <- -Data.fig$Contribution
      
      q <- ggplot(Data.fig, aes(x = Age, y = Contribution, fill = Cause)) +
        ggtitle('Decomposition of lifespan inequality', subtitle = paste0(state.ind,'vs Sweden, ', Yr))+
        facet_wrap(~Sex)+
        scale_fill_manual('Cause of death', values = base2) + 
        geom_bar(stat = "identity",position = "stack")+
        theme_light()+
        theme(text = element_text(size=10),
              axis.text.x = element_text(angle=45, hjust=1))+
        labs(x = " ", y = " ",size=10)+
        theme(text = element_text(size=10),
              strip.text.x = element_text(size = 12, colour = "black"))+
        geom_hline(yintercept = 0)
      #  coord_flip()
      r <- ggplotly(q,width = 1200, height = 380)
      r$x$layout$xaxis$title = "Age group"
      r$x$layout$xaxis2$title = "Age group"
      
      print(r)
    })
    
    output$mytable = renderDataTable({
    
      state.ind   <- input$state.ind
      initial.ind <- input$initial.ind
      final.ind   <- input$final.ind
      
      DT.table.ex <- getTable.function(Data = DT.Decomp.ex,state = state.ind,initial = initial.ind,final = final.ind) 
      DT.table.ed <- getTable.function(Data = DT.Decomp.cv,state = state.ind,initial = initial.ind,final = final.ind) 
      
      Dmales.ex   <- DT.table.ex[DT.table.ex$Sex == 'Male', c('Cause','Contribution')]
      Dmales.ed   <- DT.table.ed[DT.table.ed$Sex == 'Male', c('Cause','Contribution')]
      DT.males    <- cbind(Dmales.ex,Dmales.ed$Contribution)
      colnames(DT.males) <- c('Cause','Life expectancy','Lifespan inequality')
      DT.males[,2] <- round(DT.males[,2],2)
      DT.males[,3] <- round(DT.males[,3],5)
      # 
      # Data <- Decomp_results[Decomp_results$Period1==period & Decomp_results$Sex == sx & 
      #                          Decomp_results$Country.name==country & Decomp_results$Sources==source1 &
      #                          Decomp_results$Age < 80,]
      # Data <- data.table(Data)
      # Total.cause2 <- Data[,sum(Contribution), by = list(Cause,Sex)]
      # 
      # cause.name.vec2  <-c('a) (A00-B99) Certain infectious and parasitic diseases', 'b) (C00-D48) Neoplasms',
      #                      'f) (I00-I99) Diseases of the circulatory system','k) (R00-R99) Not elsewhere classified',
      #                      'd) (F01-F99) Mental and behavioural disorders','e) (G00-G98) Diseases of the nervous system',
      #                      'c) (E00-E88) Endocrine, nutritional and metabolic diseases' ,'g) (K00-K92) Diseases of the digestive system',  
      #                      'i) (N00-N98) Diseases of the genitourinary system','j) (P00-P96) Perinatal  & (Q00-Q99) Congenital malformations',
      #                      'h) (J00-J98) Diseases of the respiratory system','l) (V01-Y89) External mortality: accidents and suicide',
      #                      'm) (X85-Y09) Homicide', 'n) Rest of causes')
      # 
      # Total.cause2$Cause <- cause.name.vec2
      # Total.cause2$Contribution <- round(Total.cause2$V1,2)
      # Total.cause2<- data.frame(Total.cause2[,c('Cause','Contribution')])
      # Cause.Total <- data.frame(cbind(Cause='Total',Contribution=sum(Total.cause2$Contribution)))
      # 
      # Total.cause2 <- rbind(Total.cause2,Cause.Total)
      # Total.cause2 <- Total.cause2[with(Total.cause2,order(Cause)),]
      # Total.cause2 <- data.frame(Total.cause2)
      # rownames(Total.cause2) <- NULL
      # Total.cause2$Contribution <- as.numeric(Total.cause2$Contribution)
      datatable(DT.males, options = list(paging=FALSE,ordering=T,dom = 't'),rownames = F,caption = 'Males')
    })

    output$mytable2 = renderDataTable({
      
      state.ind   <- input$state.ind
      initial.ind <- input$initial.ind
      final.ind   <- input$final.ind
      
      DT.table.ex <- getTable.function(Data = DT.Decomp.ex,state = state.ind,initial = initial.ind,final = final.ind) 
      DT.table.ed <- getTable.function(Data = DT.Decomp.cv,state = state.ind,initial = initial.ind,final = final.ind) 
      
      Dfemales.ex   <- DT.table.ex[DT.table.ex$Sex == 'Female', c('Cause','Contribution')]
      Dfemales.ed   <- DT.table.ed[DT.table.ed$Sex == 'Female', c('Cause','Contribution')]
      DT.females    <- cbind(Dfemales.ex,Dfemales.ed$Contribution)
      colnames(DT.females) <- c('Cause','Life expectancy','Lifespan inequality')
      
      DT.females[,2] <- round(DT.females[,2],2)
      DT.females[,3] <- round(DT.females[,3],5)
      
      
      datatable(DT.females, options = list(paging=FALSE,ordering=T,dom = 't'),rownames = F,caption = 'Females')
    })
    
    output$text4 <- renderText({
      state.ind   <- input$state.ind
      initial.ind <- input$initial.ind
      final.ind   <- input$final.ind
      
      t2 <- paste0('Cause-specific contributions in life expectancy and lifespan inequality. ',
             paste0(state.ind,', ',initial.ind,'-',final.ind,'.'))
      t2
      
    })
      
    output$DT.sum.males2 = renderDataTable({
      
      state.ind <- input$state.ind
      #years <- c(1975,1995)
      years     <- c(input$initial.ind,input$final.ind)
      Data2      <- Data
      
      DT.info   <- Data2[Data2$Country == state.ind & Data2$Year %in% years & Data2$Age==0,]
      Males     <- DT.info[DT.info$Sex2 == 'Males',c('Year','ex','cv')]
      Dif       <- Males[2,2:3]-Males[1,2:3]
      Dif       <- cbind(Year='Difference',Dif)
      DT.males  <- rbind(Males,Dif)
      colnames(DT.males) <- c('Year', 'L. Exp', 'L. Inequality')
      rownames(DT.males) <- NULL
      DT.males[,2:3] <- round(DT.males[,2:3],5)
      datatable(DT.males, options = list(paging=FALSE,ordering=T, dom = 't'),rownames = F,caption = 'Males')
    })
    
    output$DT.sum.females2 = renderDataTable({
      
      state.ind <- input$state.ind
      years     <- c(input$initial.ind,input$final.ind)
      Data2      <- Data
      
      DT.info   <- Data2[Data2$Country == state.ind & Data2$Year %in% years & Data2$Age==0,]
      Females     <- DT.info[DT.info$Sex2 == 'Females',c('Year','ex','cv')]
      Dif       <- Females[2,2:3]-Females[1,2:3]
      Dif       <- cbind(Year='Difference',Dif)
      DT.females  <- rbind(Females,Dif)
      colnames(DT.females) <- c('Year', 'L. Exp', 'L. Inequality')
      rownames(DT.females) <- NULL
      DT.females[,2:3] <- round(DT.females[,2:3],5)
      datatable(DT.females, options = list(paging=FALSE,ordering=T, dom = 't'),rownames = F,caption = 'Females')
    })
    
    output$DT.dif.Swe = renderDataTable({
      
      state.ind <- input$state.ind
      
      years     <- input$year.compare
      
      Data1      <- Data
      
      DT.info   <- Data1[Data1$Country %in% c(state.ind,'Sweden') & Data1$Year %in% years & Data1$Age==0,]
      
      dif.e0 <- -DT.info[DT.info$Country == state.ind,]$ex + DT.info[DT.info$Country == 'Sweden',]$ex
      dif.cv <- -DT.info[DT.info$Country == state.ind,]$cv + DT.info[DT.info$Country == 'Sweden',]$cv
      dif.cv <- round(dif.cv,4)
      dif.e0 <- round(dif.e0,2)
      sexes  <- c('Females', 'Males')
      
      DT.out <- rbind(sexes,dif.e0,dif.cv)
      
      label3 <- c('', 'Life expectancy','cv' )
      DT.out <- cbind(label3,DT.out)
      colnames(DT.out) <- c('Indicator', 'Sex',' ' )
      
      
      datatable(DT.out, options = list(paging=FALSE,ordering=T, dom = 't'),rownames = F,caption = 'Difference with Sweden')
    })
    
    output$early <- renderPlotly({
      #years <- 2014
      years     <- input$year.compare
      Data2       <- ASMR[ASMR$Year == years,]
      Data2$mx    <- round(Data2$mx,4)
      Data2$Sex   <- as.factor(Data2$Sex)
      levels(Data2$Sex) <- c('Females', 'Males')
      Data2$PopName   <- as.factor(Data2$PopName)
      levels(Data2$PopName) <- c('Denmark', 'Norway', 'Sweden')
      
      p <-ggplot(Data2, aes(x = Age,y = mx,colour=(PopName))) +
        ggtitle('ASMR') +
        geom_line(aes(group = PopName), size= 1) +
        facet_wrap(~Sex)+
        theme_light()+
        labs(y = "Years")+
        scale_colour_manual('Country', values = c('blue', 'green', 'red')) + 
        theme(text = element_text(size=14),
              strip.text.x = element_text(size = 14, colour = "black"))
      #p
      print(ggplotly(p,width = 1350, height = 400))
      
    })
    
    
})
