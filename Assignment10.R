library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)

accident <- read.csv("midwest_accident.csv")

ui <- fluidPage(
  titlePanel("Midwest Accidents Map and Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("state", "Select State:",
                  choices = unique(accident$STATENAME)),
      selectInput("day", "Select Day of the Week:",
                  choices = c("All", unique(accident$DAY_WEEKNAME))),
      selectInput("weather", "Select Weather Condition:",
                  choices = c("All", unique(accident$WEATHERNAME)))
    ),
    
    mainPanel(
      leafletOutput("accidentMap"), 
      plotOutput("timeSeriesPlot"),  
      plotOutput("weatherBarPlot")   
    )
  )
)


server <- function(input, output) {
  
  filtered_accidents <- reactive({
    data <- accident
    
    if (input$state != "All") {
      data <- data %>% filter(STATENAME == input$state)
    }
    
    if (input$day != "All") {
      data <- data %>% filter(DAY_WEEKNAME == input$day)
    }
    
    if (input$weather != "All") {
      data <- data %>% filter(WEATHERNAME == input$weather)
    }
    
    return(data)
  })
  
  output$accidentMap <- renderLeaflet({
    data <- filtered_accidents()
    
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        lat = ~LATITUDE, lng = ~LONGITUD,
        popup = ~paste(STATENAME, "<br>City: ", CITYNAME, "<br>Day: ", DAY_WEEKNAME, "<br>Weather: ", WEATHERNAME, "<br>Fatalities: ", FATALS),
        radius = 2, color = "blue", fill = TRUE, fillOpacity = 0.5
      ) %>%
      setView(lng = mean(data$LONGITUD), lat = mean(data$LATITUDE), zoom = 6)
  })
  
  output$timeSeriesPlot <- renderPlot({
    data <- filtered_accidents()
    
    if (input$day != "All") {
      data <- data %>%
        mutate(Time = as.POSIXct(paste(Sys.Date(), HOUR, MINUTE), format="%Y-%m-%d %H %M")) %>%
        group_by(HOUR) %>%
        summarise(accident_count = n())
      
      ggplot(data, aes(x = HOUR, y = accident_count)) +
        geom_line() +
        geom_point() +
        labs(title = paste("Accidents by Hour on", input$day),
             x = "Hour of the Day", y = "Number of Accidents") +
        theme_minimal()
    } else {
      data <- data %>%
        mutate(Time = as.POSIXct(paste(Sys.Date(), HOUR, MINUTE), format="%Y-%m-%d %H %M")) %>%
        group_by(HOUR) %>%
        summarise(accident_count = n())
      
      ggplot(data, aes(x = HOUR, y = accident_count)) +
        geom_line() +
        geom_point() +
        labs(title = "Accidents by Hour (All Days)",
             x = "Hour of the Day", y = "Number of Accidents") +
        theme_minimal()
    }
  })
  
  output$weatherBarPlot <- renderPlot({
    data <- filtered_accidents()
    
    if (input$weather != "All") {
      weather_data <- data %>%
        group_by(WEATHERNAME) %>%
        summarise(accident_count = n())
      
      ggplot(weather_data, aes(x = WEATHERNAME, y = accident_count, fill = WEATHERNAME)) +
        geom_bar(stat = "identity") +
        labs(title = paste("Accidents by Weather Condition:", input$weather),
             x = "Weather Condition", y = "Number of Accidents") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      weather_data <- data %>%
        group_by(WEATHERNAME) %>%
        summarise(accident_count = n())
      
      ggplot(weather_data, aes(x = WEATHERNAME, y = accident_count, fill = WEATHERNAME)) +
        geom_bar(stat = "identity") +
        labs(title = "Accidents by Weather Condition (All)",
             x = "Weather Condition", y = "Number of Accidents") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
}

shinyApp(ui = ui, server = server)

