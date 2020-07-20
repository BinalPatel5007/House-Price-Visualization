# Data
library(plotly)
library(shiny)
library(ggplot2)
library(MPV)
library(KernSmooth)
h<-table.b4
head(h)
names(h)[1] <- "sale_price"
names(h)[2] <- "taxes"
names(h)[3] <- "bath"
names(h)[4] <- "lot_size"
names(h)[5] <- "living_space"
names(h)[6] <- "garage"
names(h)[7] <- "rooms"
names(h)[8] <- "bedrooms"
names(h)[9] <- "age"
names(h)[10] <- "fireplace"
head(h)
dataset<-h
# Application

library(shiny)
library(shinythemes)
shinyApp(
  ui = tagList(
    #shinythemes::themeSelector(cerulean),
    navbarPage(
      theme = "cerulean",  # <--- To use a theme, uncomment this
      "House For All",
      tabPanel("Plots",
               fluidPage(
                 
                 title = "House Data",
                 
                 plotlyOutput("plot"),
                 
                 hr(),
                 
                 fluidRow(
                   column(3,
                          h4("House Data"),
                          sliderInput('sampleSize', 'Sample Size', 
                                      min=1, max=nrow(dataset),
                                      value=min(1000, nrow(dataset)), 
                                      step=500, round=0),
                          br(),
                          #checkboxInput('jitter', 'Jitter'),
                          #checkboxInput('smooth', 'Smooth'),
                          checkboxInput('abline','line')
                   ),
                   column(4, offset = 1,
                          selectInput('x', 'X', names(dataset)),
                          selectInput('y', 'Y', names(dataset), names(dataset)[[2]]),
                          selectInput('color', 'Facilities', c('None', names(dataset)))
                   ),
                   column(2, offset = 1,
                          numericInput(inputId = "priceValue",
                                       label = "Estimated House Price",
                                       min = 0, max = 11, value = 11),
                          actionButton('go',"Estimate")
                   ),
                   
                   
                   column(4,
                          mainPanel(
                            #verbatimTextOutput(outputId = "RegSum"),
                            
                            textOutput('value')
                            #,plotOutput("plot2")
                          ))
                 )
               )
      ),tabPanel("Data",
                 fluidPage(
                   title = "Tracy House dataset",
                   sidebarLayout(
                     sidebarPanel(
                       conditionalPanel(
                         'input.dataset === "h"',
                         checkboxGroupInput("show_vars", "Columns in Data to show:", choices= names(h), selected = names(h))
                       )
                     ),
                     mainPanel(
                       tabsetPanel(
                         id = 'dataset',
                         tabPanel("Housing data", DT::dataTableOutput("mytable1"))
                       )
                     )
                   )
                 )
      )
    )
  ),
  server<-function(input, output) {
    
    dataset <- reactive({
      h[sample(nrow(h), input$sampleSize),]
    })
    
    output$plot <- renderPlotly({
      
      p <- ggplot(dataset(), aes_string(x=input$x, y=input$y)) + geom_point(size=2)
      
      if (input$color != 'None')
        p <- p + aes_string(color=input$color)
      
      # if (input$jitter)
      #   p <- p + geom_jitter()
      # if (input$smooth)
      #   p <- p + geom_smooth()
      if (input$abline)
        p <- p + geom_smooth(method = "lm")
      
      print(p)
      
    })
    
    data <- reactiveValues()
    observeEvent(input$go,{
      #browser()
      data$var <-input$incomeValue
      
      newPredict<-data.frame(lot_size=c(input$priceValue))
      
      modelLM<-lm(sale_price~lot_size,data=h)
      
      data$op = predict(modelLM, newPredict)[[1]]
    })
    
    lstat = renderText({data$var})
    
    output$value <- renderPrint({data$op})
    
    output$plot2 <- renderPlot({
      plot(h$taxes, h$sale_price, xlab = "Lot size (in thousands of square feet)", ylab = "sale price (in thousands of dollars)", 
           main = "Housing Prices",
           cex = 1.5, pch = 16, bty = "n")
      #add regression line from model
      modelLM<-lm(sale_price~lot_size,data=h)
      abline(modelLM, col = "blue", lwd = 2)
    })
    
    diamonds2 = h[sample(nrow(h)), ]
    output$mytable1 <- DT::renderDataTable({
      DT::datatable(diamonds2[, input$show_vars, drop = FALSE])
    })
    
    
    
  }
)
