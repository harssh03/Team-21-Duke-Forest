library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Data Visualization App"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose CSV File', accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
      tags$hr(),
      selectInput('plotType', 'Select Plot Type', choices = c('scatter', 'box', 'histogram')),
      uiOutput("xvar"),
      uiOutput("yvar"),
      uiOutput("colorvar")
    ),
    mainPanel(
      plotOutput('plot')
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$file1)
    df <- read.csv(input$file1$datapath)
    return(df)
  })
  
  output$xvar <- renderUI({
    req(data())
    selectInput('xvar', 'Select X Variable', choices = names(data()))
  })
  
  output$yvar <- renderUI({
    req(data())
    if (input$plotType == 'scatter' || input$plotType == 'box') {
      selectInput('yvar', 'Select Y Variable', choices = names(data()))
    }
  })
  
  output$colorvar <- renderUI({
    req(data())
    selectInput('colorvar', 'Select Color Variable', choices = c(None = '', names(data())))
  })
  
  output$plot <- renderPlot({
    req(input$xvar)
    df <- data()
    
    if (input$plotType == 'scatter') {
      req(input$yvar)
      ggplot(df, aes_string(x = input$xvar, y = input$yvar, color = input$colorvar)) +
        geom_point() +
        theme_minimal()
    } else if (input$plotType == 'box') {
      req(input$yvar)
      ggplot(df, aes_string(x = input$xvar, y = input$yvar, fill = input$colorvar)) +
        geom_boxplot() +
        theme_minimal()
    } else if (input$plotType == 'histogram') {
      ggplot(df, aes_string(x = input$xvar, fill = input$colorvar)) +
        geom_histogram(bins = 30) +
        theme_minimal()
    }
  })
}

shinyApp(ui = ui, server = server)