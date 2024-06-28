library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(lubridate)
library(DT)
library(viridis)
library(gridExtra)

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
    
    # Exclude rows where SP contains "Ant"
    df <- df %>% filter(!grepl("Ant", SP, ignore.case = TRUE))
    
    return(df)
  })
  
  output$helpText <- renderText({
    switch(input$plotType,
           'Monthly Herp Count' = 'This plot shows the count of herp species observed on a monthly basis.',
           'Transect Herp Count' = 'This plot shows the count of herp species observed per transect.',
           'Effort Calculation' = 'Effort calculation is defined as the number of visits per transect divided by the total sightings of an animal or all animals.',
           'Trends Analysis' = 'This plot shows the trends in temperature, sky condition, and time, and types of species found.',
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
    
    # Calculate Herp observations based on the requirements
    df <- df %>%
      group_by(Response_ID, Date, Transect_ID) %>%
      summarise(Herp_obs_count = sum(Herp_obs != "No", na.rm = TRUE)) %>%
      ungroup()
    
    if (input$plotType == 'Monthly Herp Count') {
      df %>% 
        mutate(month = floor_date(ymd(Date), "month")) %>%
        group_by(month) %>%
        summarise(herp_count = sum(Herp_obs_count)) %>%
        ggplot(aes(x = month, y = herp_count)) +
        geom_line(color="#00539B") +
        labs(title = "Monthly Herp Count", x = "Month", y = "Herp Count") +
        theme_minimal()
      
    } else if (input$plotType == 'Transect Herp Count') {
      df %>%
        group_by(Transect_ID) %>%
        summarise(herp_count = sum(Herp_obs_count)) %>%
        ggplot(aes(x = Transect_ID, y = herp_count, fill = Transect_ID)) +
        geom_bar(stat = "identity") +
        labs(title = "Transect Herp Count", x = "Transect ID", y = "Herp Count") +
        theme_minimal() +
        scale_fill_viridis_d() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else if (input$plotType == 'Effort Calculation') {
      df %>%
        group_by(Transect_ID) %>%
        summarise(visits = n(),
                  sightings = sum(Herp_obs_count)) %>%
        mutate(effort = visits / sightings) %>%
        ggplot(aes(x = Transect_ID, y = effort, fill = Transect_ID)) +
        geom_bar(stat = "identity") +
        labs(title = "Effort Calculation", x = "Transect ID", y = "Effort (Sightings per Visit)") +
        theme_minimal() +
        scale_fill_viridis_d() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else if (input$plotType == 'Trends Analysis') {
      # Clean and convert the temperature data
      if (!"Temperature" %in% names(df)) {
        stop("Temperature column not found in the dataset.")
      }
      
      if (!"Sky_condition" %in% names(df)) {
        stop("Sky_condition column not found in the dataset.")
      }
      
      df <- df %>% 
        mutate(Temperature_Clean = as.numeric(gsub("[^0-9.]", "", Temperature))) %>%
        filter(!is.na(Temperature_Clean))  # Exclude observations with missing temperature values
      
      # Plot 1: Trends in Temperature and Types of Species Found
      p1 <- df %>%
        ggplot(aes(x = Temperature_Clean, y = Herp_obs_count)) +
        geom_point(color = "#00539B") +
        labs(title = "Trends in Temperature and Types of Species Found", x = "Temperature (F)", y = "Herp Observations") +
        theme_minimal()
      
      # Plot 2: Sky Condition and Types of Species Found
      p2 <- df %>%
        ggplot(aes(x = Sky_condition, y = Herp_obs_count)) +
        geom_point(color = "#00539B") +
        labs(title = "Sky Condition and Types of Species Found", x = "Sky Condition", y = "Herp Observations") +
        theme_minimal()
      
      # Plot 3: Time and Types of Species Found
      p3 <- df %>%
        ggplot(aes(x = Date, y = Herp_obs_count)) +
        geom_line(color = "#00539B") +
        labs(title = "Time and Types of Species Found", x = "Date", y = "Herp Observations") +
        theme_minimal()
      
      gridExtra::grid.arrange(p1, p2, p3, ncol = 1)
      
    } else if (input$plotType == 'Ants and Herps Correlation') {
      df %>%
        ggplot(aes(x = as.numeric(Herp_obs_count), y = as.numeric(Ant_obs != "No"), color = Transect_ID)) +
        geom_point(size = 3, alpha = 0.6) +
        geom_smooth(method = "lm", se = FALSE, color = "black") +
        labs(title = "Ants and Herps Correlation", x = "Herp Observations", y = "Ant Observations") +
        theme_minimal() +
        scale_color_viridis_d()
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