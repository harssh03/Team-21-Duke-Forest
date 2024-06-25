library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(DT)

server <- function(input, output, session) {
  data <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath)
  })
  
  filteredData <- reactive({
    req(data())
    df <- data()
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
  
  output$plot <- renderPlot({
    req(filteredData())
    df <- filteredData()
    
    if (input$plotType == 'Monthly Herp Count') {
      df %>% mutate(month = floor_date(ymd(Date), "month")) %>%
        group_by(month) %>%
        summarise(herp_count = sum(Herp_obs == "Yes")) %>%
        ggplot(aes(x = month, y = herp_count)) +
        geom_line() +
        labs(title = "Monthly Herp Count", x = "Month", y = "Herp Count")
      
    } else if (input$plotType == 'Transect Herp Count') {
      df %>%
        group_by(Transect_ID) %>%
        summarise(herp_count = sum(Herp_obs == "Yes")) %>%
        ggplot(aes(x = Transect_ID, y = herp_count)) +
        geom_bar(stat = "identity") +
        labs(title = "Transect Herp Count", x = "Transect ID", y = "Herp Count")
      
    } else if (input$plotType == 'Effort Calculation') {
      df %>%
        group_by(Transect_ID) %>%
        summarise(visits = n(),
                  sightings = sum(Herp_obs == "Yes")) %>%
        mutate(effort = visits / sightings) %>%
        ggplot(aes(x = Transect_ID, y = effort)) +
        geom_bar(stat = "identity") +
        labs(title = "Effort Calculation", x = "Transect ID", y = "Effort (Visits per Sightings)")
      
    } else if (input$plotType == 'Trends Analysis') {
      df %>% ggplot(aes(x = Temperature, y = Herp_obs, color = Sky_condition)) +
        geom_point() +
        labs(title = "Trends Analysis", x = "Temperature", y = "Herp Observations")
      
    } else if (input$plotType == 'Ants and Herps Correlation') {
      df %>% filter(Herp_obs == "Yes" | Ant_obs == "Yes") %>%
        ggplot(aes(x = Herp_obs, y = Ant_obs, color = Transect_ID)) +
        geom_point() +
        labs(title = "Ants and Herps Correlation", x = "Herp Observations", y = "Ant Observations")
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

shinyApp(ui = ui, server = server)