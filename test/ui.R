library(shiny)
library(shinythemes)
library(ggplot2)
library(DT)

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  
  titlePanel("Duke Forest Observation Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose CSV File', accept = c('.csv')),
      selectInput('plotType', 'Select Plot Type', 
                  choices = c('Monthly Herp Count', 'Transect Herp Count', 'Effort Calculation', 'Trends Analysis', 'Ants and Herps Correlation')),
      dateRangeInput('dateRange', 'Select Date Range'),
      actionButton('generateReport', 'Generate CSV Report'),
      downloadButton('downloadData', 'Download CSV'),
      helpTextOutput("helpText")
    ),
    mainPanel(
      plotOutput('plot'),
      DTOutput('dataTable')
    )
  )
)