library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(lubridate)
library(DT)

# Define UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  
  titlePanel(tags$h1(tags$span(style = "color:#001A57", "Duke"), "Forest Data+")),
  
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose CSV File', accept = c('.csv')),
      selectInput('plotType', 'Select Plot Type', 
                  choices = c('Monthly Herp Count', 'Transect Herp Count', 'Effort Calculation', 'Trends Analysis', 'Ants and Herps Correlation')),
      dateRangeInput('dateRange', 'Select Date Range'),
      actionButton('generateReport', 'Generate CSV Report'),
      downloadButton('downloadData', 'Download CSV'),
      textOutput("helpText"),
      textOutput("debugText") # Added for debugging
    ),
    mainPanel(
      plotOutput('plot'),
      DTOutput('dataTable')
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  data <- reactive({
    req(input$file1)
    df <- read.csv(input$file1$datapath)
    return(df)
  })
  
  filteredData <- reactive({
    req(data())
    df <- data()
    
    # Check if Date column exists and is correctly formatted
    if (!"Date" %in% names(df)) {
      stop("Date column not found in the uploaded file.")
    }
    
    df$Date <- as.Date(df$Date, format="%Y-%m-%d")
    
    if (any(is.na(df$Date))) {
      stop("There are missing or invalid dates in the Date column.")
    }
    
    df <- df %>% filter(Date >= input$dateRange[1] & Date <= input$dateRange[2])
    return(df)
  })
  
  output$helpText <- renderText({
    switch(input$plotType,
           'Monthly Herp Count' = 'This plot shows the count of herp species observed on a monthly basis.',
           'Transect Herp Count' = 'This plot shows the count of herp species observed per transect.',
           'Effort Calculation' = 'Effort calculation is defined as the number of visits per transect divided by the total sightings of an animal or all animals.',
           'Trends Analysis' = 'This plot shows the trends in temperature, sky condition, time and types of species found.',
           'Ants and Herps Correlation' = 'This plot shows the correlation between the counts of ants and herps observed at different transects.')
  })
  
  output$debugText <- renderText({
    # This output is for debugging purposes
    req(data())
    df <- data()
    paste("Date range in dataset:", min(df$Date), "to", max(df$Date))
  })
  
  output$plot <- renderPlot({
    req(filteredData())
    df <- filteredData()
    
    if (input$plotType == 'Monthly Herp Count') {
      df %>% 
        mutate(month = floor_date(ymd(Date), "month")) %>%
        group_by(month) %>%
        
        # Adjust the column name for 'Herp_obs'
        summarise(herp_count = sum(Herp_obs != "No")) %>%
        
        ggplot(aes(x = month, y = herp_count)) +
        geom_line(color="#00539B") +
        labs(title = "Monthly Herp Count", x = "Month", y = "Herp Count") +
        theme_minimal()
      
    } else if (input$plotType == 'Transect Herp Count') {
      df %>%
        
        # Adjust the column name for 'Transect_ID' and 'Herp_obs'
        group_by(Transect_ID) %>%
        summarise(herp_count = sum(Herp_obs != "No")) %>%
        
        ggplot(aes(x = Transect_ID, y = herp_count, fill = Transect_ID)) +
        geom_bar(stat = "identity") +
        labs(title = "Transect Herp Count", x = "Transect ID", y = "Herp Count") +
        theme_minimal() +
        scale_fill_manual(values = c("#001A57", "#00539B", "#002E5D"))
      
    } else if (input$plotType == 'Effort Calculation') {
      df %>%
        
        # Adjust the column name for 'Transect_ID' and 'Herp_obs'
        group_by(Transect_ID) %>%
        summarise(visits = n(),
                  sightings = sum(Herp_obs != "No")) %>%
        mutate(effort = visits / sightings) %>%
        
        ggplot(aes(x = Transect_ID, y = effort, fill = Transect_ID)) +
        geom_bar(stat = "identity") +
        labs(title = "Effort Calculation", x = "Transect ID", y = "Effort (Visits per Sightings)") +
        theme_minimal() +
        scale_fill_manual(values = c("#001A57", "#00539B", "#002E5D"))
      
    } else if (input$plotType == 'Trends Analysis') {
      df %>%
        
        # Adjust the column names for 'Temperature', 'Herp_obs', and 'Sky_condition'
        ggplot(aes(x = Temperature, y = as.numeric(Herp_obs != "No"), color = Sky_condition)) +
        geom_point() +
        labs(title = "Trends Analysis", x = "Temperature", y = "Herp Observations") +
        theme_minimal() +
        scale_color_manual(values = c("#001A57", "#00539B", "#002E5D"))
      
    } else if (input$plotType == 'Ants and Herps Correlation') {
      df %>%
        
        # Adjust the column names for 'Herp_obs', 'Ant_obs', and 'Transect_ID'
        filter(Herp_obs != "No" | Ant_obs != "No") %>%
        ggplot(aes(x = as.numeric(Herp_obs != "No"), y = as.numeric(Ant_obs != "No"), color = Transect_ID)) +
        geom_point() +
        labs(title = "Ants and Herps Correlation", x = "Herp Observations", y = "Ant Observations") +
        theme_minimal() +
        scale_color_manual(values = c("#001A57", "#00539B", "#002E5D"))
    }
  })
  
  output$dataTable <- renderDT({
    req(filteredData())
    datatable(filteredData())
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("report-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filteredData(), file)
    }
  )
  
  observeEvent(input$generateReport, {
    updateDateRangeInput(session, 'dateRange', start = min(data()$Date), end = max(data()$Date))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)