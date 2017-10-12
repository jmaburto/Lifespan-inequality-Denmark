library(shiny)
library(ggplot2)
library(data.table)
library(RColorBrewer)


setwd("C:/Users/jmaburto/Documents/GitHub/Lifespan-inequality-Denmark/R/DK_App")


rsconnect::setAccountInfo(name='jmaburto',
                          token='7310E08D0D081D3C3CABCAA90D18045E',
                          secret='Vzlie6RN39/THGhWKatGf/C68yZp+RENdTdOl/ey')

load('Results.RData')




runApp()
