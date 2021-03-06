library(shiny)
library(data.table)
library(DT)
library(plotly)



  states <- c('Denmark', 'Norway', 'Sweden')
  Initial.year <- 1960:2013
  causes <- c(
    'Infectious, non-R',
    'Cancer AS',
    'Cancer NAS',
    'Diabetes',
    'Cardiovascular',
    'Respiratory I',
    'Respiratory NI',
    'External',
    'Other')
  
  shinyUI(
    fluidPage(
      titlePanel('Lifespan inequality in Denmark, Sweden and Norway'),
      navbarPage(
        'Aburto JM, Wensink M, van Raalte A & Lindahl-Jacobsen R.  "Potential gains in life expectancy by reducing lifespan inequality in Denmark: A cause of death analysis". 
       Institute of Public Health, SDU, 2017',
        position = c("fixed-bottom")),
      
      sidebarLayout(
        sidebarPanel(
          selectInput( 'state.ind','Country',states, selected = 'Denmark'),
          br(),
          selectInput( 'initial.ind','Initial year',Initial.year, selected = 1975),
          br(),
          uiOutput('vx'),
          #br(),
          #selectInput( 'cause','Cause (heatmap)',causes, selected =  'Cancer AS'),
          br(),
          selectInput( 'year.compare','Comparison year',1960:2014, selected =  '2014'),
          br(),
          
            dataTableOutput('DT.sum.females2'),
            dataTableOutput('DT.sum.males2'),
          dataTableOutput('DT.dif.Swe'),
          width = 2
        ),

        
        mainPanel(
        tabsetPanel(
        tabPanel("Life expectancy and lifespan inequality trends",
                 plotlyOutput('e0.trends'),
                 plotlyOutput('ed.trends')),
        tabPanel("Decomposition results",
                 plotlyOutput("e0.decomp"),
                 plotlyOutput("ed.decomp")),
        tabPanel("Cause-specific summary",
                 fluidRow(h3(textOutput('text4')),
                          column(6, dataTableOutput("mytable2")),
                          column(6, dataTableOutput("mytable")))),
        tabPanel("Comparison with Sweden",
                 plotlyOutput("ex.compare"),
                 plotlyOutput("cv.compare"),
                 p('Positive (negative) values increase (decrease) the gap in life expectancy and lifespan inequality with Sweden')),
        tabPanel("Early life ASMR",
                 plotlyOutput("early"))
        
        )
        )
        )
      )
    )
  
  
#  devtools::install_github('hadley/ggplot2')
  