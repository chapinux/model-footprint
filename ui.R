#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)



# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    # Application title
    titlePanel("PSE results exploration"),
    

    fluidRow(
      column(4,
      
        sliderInput(
          inputId = "Dim1",
          label = "Dim 1",
          min = 0,
          max = 0.71,
          step=0.01,
          animate = TRUE,
          value = c(0, 1)
        )
        
        ,
        sliderInput(
          inputId = "Dim2",
          label = "Dim 2",
          min = -0.05,
          max = 0.14,
          step=0.01,
          animate = TRUE,
          value = c(-1, 1)
        )
        
        ,
        sliderInput(
          inputId = "Dim3",
          label = "Dim 3",
          min = 0.2,
          max = 7.7,
          step=0.1,
          animate = TRUE,
          value = c(0, 8)
        ),
        
        sliderInput(
          inputId = "nbpoints",
          label = "nb points",
          min = 10,
          max = 1000,
          value = 1000
        )
      ),
       column(8,
       plotlyOutput("nuagePlot"))
    ),
      
        fluidRow(column(4,
                         plotlyOutput("i1i2")),
                  column(4,
                        plotlyOutput("i3i4")),
                 column(4,
                        plotlyOutput("i5i6"))
       , width=12
      )
    
  )
)