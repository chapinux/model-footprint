#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(scales)
library(RColorBrewer)
library(dplyr)

dfpse <- read.csv("population400000.csv",header = T)  
dfpse<- dfpse[,2:12]
names(dfpse)<- c("i1","i2","i3","i4","i5","i6","i7","dim1","dim2","dim3","dim4")

dfpse <-sample_n(dfpse, size = 961)

# scatterplot 3D dimension names  
dimScatter3D <- c("dim1", "dim2", "dim3")
# labels of extrema points in the scatter3D plot 
labelExtrema  <- c("mindim1", "minDim2", "minDim3",  "maxDim1", "maxDim2", "maxDim3")




minis <- data.frame(t(apply(dfpse[,8:11],MARGIN = 2,min)))
maxis <- data.frame(t(apply(dfpse[,8:11],MARGIN = 2,max)))
# coordonnées points du plan

  


p3d<- plot_ly(dfpse,  x = ~dim1,y = ~dim2, z = ~dim3, 
              mode="markers", 
              type="scatter3d",
              hoverinfo="text",
              text = ~paste("", 'dim1: ', round(dim1, digits = 3), 
                            '</br> dim2: ', round(dim2, digits = 3),
                            '</br> dim3: ', round(dim3, digits  =3)),
              showlegend=F,
              marker = list(size = 2, 
                            opacity=0.5, 
                            line = list(width = 0.1, color="gray25") )
              
              ,width = 500, height = 400)

p3d


minis
maxis
vv <- 0.4

planDim1_min <- list(
  x=rep(vv,31),
  y=rep(c(minis$dim2,maxis$dim2),31) ,
  z=rep(c(minis$dim3,minis$dim3,maxis$dim3,maxis$dim3),250),
  colorscale = list(c(0, "rgba(0,0,255,.3)"),list(1, "rgba(0,0,255,.3)")), 
  name = "First Plane", 
  type = 'surface'
) 
plot_ly(data=planDim1_min)

length(planDim1_min$z)

yy <-seq(from = minis$dim2,to = maxis$dim2,length.out = 31)
yy

planDim1_min$type

p3d <- add_trace(p3d,x=planDim1_min$x, y=planDim1_min$y, z=planDim1_min$z,colorscale=planDim1_min$colorscale, type=planDim1_min$type)
p3d

pp <- plot_ly()
pp <- add_trace(pp,x=planDim1_min$x, y=planDim1_min$y, z=planDim1_min$z,colorscale=planDim1_min$colorscale, type=planDim1_min$type)
pp


z <- c(
  rep(1,12),
  c(0,0,0,0,0,0),
  c(0,0,0,0,0,0),
  c(0,0,0,0,0,0),
  c(0,0,0,0,0,0),  
  c(0,0,0,0,0,0),  
  c(0,0,0,0,0,0),
    c(0,0,0,0,0,0),
  c(0,0,0,0,0,0),
    c(0,0,0,0,0,0),
  c(0,0,0,0,0,0),
  c(0,0,0,0,0,0),
  c(0,0,0,0,0,0),
  c(0,0,0,0,0,0)
  
)
length(z)
dim(z) <- c(15,6)
z2 <- z + 1
z3 <- z - 1

p <- plot_ly(showscale = FALSE) %>%
  add_surface(z = ~z) 
p

#==============================================




shinyServer(function(input, output) {
  
  subsetdfpse <- reactive({
    currentdfpse <- sample_n(dfpse, size = input$nbpoints)
    currentdfpse[
      currentdfpse$dim1 >= input$Dim1[1]
      & currentdfpse$dim1 <= input$Dim1[2]
      & currentdfpse$dim2 >= input$Dim2[1]
      & currentdfpse$dim2 <= input$Dim2[2]
      & currentdfpse$dim3 >= input$Dim3[1]
      & currentdfpse$dim3 <= input$Dim3[2]
      ,]
    
  })
                                  
  
  
  output$nuagePlot <- renderPlotly({
  
     subsetdfpse <- sample_n(dfpse, size = input$nbpoints)
 
    idxmin <- sapply(subsetdfpse[,dimScatter3D],which.min)
    idxmax <- sapply(subsetdfpse[,dimScatter3D],which.max)
    pointsInteret <- subsetdfpse[append(idxmin,idxmax),dimScatter3D]
    pointsInteret$label <- labelExtrema
    
    
    planDim1_min <- list(
      x=rep(input$Dim1[1],1000),
      y=rep(c(minis$dim2,maxis$dim2),2) ,
      z=c(minis$dim3,minis$dim3,maxis$dim3,maxis$dim3),
      colorscale = list(c(0, "rgba(0,0,255,.3)"),list(1, "rgba(0,0,255,.3)")), 
      name = "First Plane", 
      type = "surface"
    ) 
    
    
    p3d<- plot_ly(subsetdfpse,  x = ~dim1,y = ~dim2, z = ~dim3, 
 
                mode="markers", 
                
            type="scatter3d",
            hoverinfo="text",
            text = ~paste("", 'dim1: ', round(dim1, digits = 3), 
                          '</br> dim2: ', round(dim2, digits = 3),
                          '</br> dim3: ', round(dim3, digits  =3)),
             showlegend=F,
            marker = list(size = 2, 
                          opacity=0.5, 
                          line = list(width = 0.1, color="gray25") )
     
    ,width = 500, height = 400)
     add_markers(p3d,
              data=pointsInteret, type="scatter3d",
              marker=list(size=2, opacity=0.8, color="red", text ),
              text = ~label, textposition = 'bottom',
              textfont = list(color = '#FF0000', size = 10), showlegend=F, name="")
     p3d <- add_trace(p3d,x=planDim1_min$x, y=planDim1_min$y, z=planDim1_min$z,colorscale=planDim1_min$colorscale, type=planDim1_min$surface)
      p3d
  })
 
  
  mymarker <- list(size = 5,
                    color = 'rgba(10, 10, 200, .9)')
  
  output$i1i2 <- renderPlotly({

    currentNuage <- sample_n(dfpse, size = input$nbpoints)
    subsetdfpse <- subsetdfpse()
    p12 <- plot_ly(data = currentNuage, x = ~i1, y = ~i2, type='scatter', mode="markers", showlegend=FALSE)
    p12 <- add_trace(p12, data = subsetdfpse, x=~i1, y=~i2, mode='markers', marker= list(color = 'rgba(255, 82, 93, .9)'))
    p12
    
    })
  
  output$i3i4 <- renderPlotly({
    currentNuage <- sample_n(dfpse, size = input$nbpoints)
    subsetdfpse <- subsetdfpse()
    p34 <- plot_ly(data = currentNuage, x = ~i3, y = ~i4, type='scatter', mode="markers", showlegend=FALSE)
    p34 <- add_trace(p34, data = subsetdfpse, x=~i3, y=~i4, mode='markers', marker= list(color = 'rgba(255, 82, 93, .9)'))
    p34
  })
  
  output$i5i6 <- renderPlotly({
    currentNuage <- sample_n(dfpse, size = input$nbpoints)
    subsetdfpse <- subsetdfpse()
    p56 <- plot_ly(data = currentNuage, x = ~i5, y = ~i6, type='scatter', mode="markers", showlegend=FALSE)
    p56 <- add_trace(p56, data = subsetdfpse, x=~i5, y=~i6, mode='markers', marker= list(color = 'rgba(255, 82, 93, .9)'))
    p56
  })
  
  
})
