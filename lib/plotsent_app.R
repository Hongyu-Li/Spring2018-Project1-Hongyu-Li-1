library(shiny)
library(dplyr)
source('E:/Columbia University/2018Spring/Applied Data Science/Github/Spring2018-Project1-Hongyu-Li/lib/plotsent_func.R')
library(scales)

sentiment.df<-read.csv('E:/Columbia University/2018Spring/Applied Data Science/Github/Spring2018-Project1-Hongyu-Li/output/sentimentdata.csv',
                       as.is=TRUE)

inaugu.info<-read.csv('E:/Columbia University/2018Spring/Applied Data Science/Github/Spring2018-Project1-Hongyu-Li/output/inauguinfo.csv',
                      as.is=TRUE)
inaugu.info$Filename<-substr(inaugu.info$Filename, 19, nchar(inaugu.info$Filename))
inaugu.info.d<-inaugu.info[inaugu.info$Party=='Democratic',]
inaugu.info.r<-inaugu.info[inaugu.info$Party=='Republican',]

shinyApp(
  ui = fluidPage(
    fluidRow(style = "padding-bottom: 20px;",
             column(4, selectInput('speech1', 'Democratic',
                                   inaugu.info.d$Filename,
                                   selected=inaugu.info.d$Filename[1])),
             column(4, selectInput('speech2', 'Republican',
                                   inaugu.info.r$Filename,
                                   selected=inaugu.info.r$Filename[1]))),
    fluidRow(
      plotOutput('sentiment', height = "1000px")
    )
  ),
  
  server = function(input, output, session) {
    
    # Combine the selected variables into a new data frame
    selectedData <- reactive({
      list(term1=as.numeric(substr(input$speech1,nchar(input$speech1)-4,
                                   nchar(input$speech1)-4)),
           term2=as.numeric(substr(input$speech2,nchar(input$speech2)-4,
                                   nchar(input$speech2)-4)),
           file1=substr(input$speech1,6,nchar(input$speech1)-6),
           file2=substr(input$speech2,6,nchar(input$speech2)-6),
           president1=sentiment.df$President[which(sentiment.df$Filename==paste0('InauguralSpeeches/',input$speech1))][1],
           president2=sentiment.df$President[which(sentiment.df$Filename==paste0('InauguralSpeeches/',input$speech2))][1]
      )})
    
    output$sentiment <- renderPlot(height = 500, {
      par(mfrow=c(1,2))
      f.plotsent(In.list=sentiment.df, 
                 InFile=selectedData()$file1, 
                 InTerm=selectedData()$term1, 
                 President=selectedData()$president1)
      f.plotsent(In.list=sentiment.df, 
                 InFile=selectedData()$file2, 
                 InTerm=selectedData()$term2, 
                 President=selectedData()$president2)
    })
  },
  options = list(height = 700)
)
