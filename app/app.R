#Developed by Yi Xu
# last update 2025.6.28
# This is a shiny app to provide a tool to use Nearest Neighbor method to forecast salmon return
# 2024 Sockeye International competition
library(shiny)
library(tidyverse)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .top-right-img {
        position: fixed;
        top: 10px;
        right: 10px;
        width: 90px;  /* Adjust size as needed */
        height: auto;
      }
    "))),
  titlePanel("Sockeye Forecasting App using Nearest Neighbor Method"),
  sidebarLayout(
    sidebarPanel(
      fileInput("data_file", "Upload SR data", accept = ".csv"),
      uiOutput("stock_selector"),
      numericInput("N", "Enter N of nearest years:", value = 2, min = 1, max = 10),
      numericInput("forecast_year", "Enter Forecast Year:", value = 2024, min = 1900, max = 2100),
      br(),strong("Developed by Yi Xu"), 
      br(),strong("Last update 2025.3.9."),
      br(),strong("All rights reserved."),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Forecast", tableOutput("forecast_table")),
        tabPanel("Raw Data", tableOutput("raw_data_table"))
      )
    )
  ),
  
  # Add image to bottom-left corner
  tags$img(src = "https://raw.githubusercontent.com/yi-xu-dfw/2024_Sockeye_International/main/Salmon%20counts.jpg
", class = "top-right-img")
)

# Define Server
server <- function(input, output, session) {
  
  # Internal Age Conversion Data Frame
  age_table <- reactive({
    data.frame(
      EUAge = c(0.1, 0.2, 1.1, 0.3, 1.2, 2.1, 0.4, 1.3, 2.2, 1.4, 2.3, 3.2, 1.5),
      GilbertAge = c(21, 31, 32, 41, 42, 43, 51, 52, 53, 62, 63, 64, 72),
      TotalAge = c(2, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6, 7),
      EUAgeName = c("X0.1", "X0.2", "X1.1", "X0.3", "X1.2", "X2.1", "X0.4", "X1.3", "X2.2", "X1.4", "X2.3", "X3.2", "X1.5")
    ) %>%
      mutate(Brood.Year = input$forecast_year - TotalAge)
  })
  
  # Reactive expression to read data
  raw_data <- reactive({
    req(input$data_file)
    read_csv(input$data_file$datapath)
  })
  
  # Generate stock dropdown
  output$stock_selector <- renderUI({
    req(raw_data())
    selectInput("stock", "Select Stock:", choices = unique(raw_data()$stock))
  })
  
  # Filter raw data by stock
  output$raw_data_table <- renderTable({
    req(raw_data(), input$stock)
    raw_data() %>% filter(stock == input$stock) %>% 
      mutate(across(where(is.numeric), as.integer))
  })
  
  # Forecast calculation
  forecast <- reactive({
    req(raw_data(), age_table(), input$stock, input$N)
    
    stock_data <- raw_data() %>%
      filter(stock == input$stock & Brood.Year > 1990)
    
    target_age <- stock_data %>%
      pivot_longer(cols = starts_with("X"), values_to = "recruit", names_to = "ages") %>%
      rename(total_recruits = Recruits) %>%
      mutate(perc = recruit / total_recruits) %>%
      filter(recruit >= 1 & perc > 0.05) %>%
      count(ages) %>%
      filter(n > 5) %>%
      left_join(age_table(), by = c("ages" = "EUAgeName")) %>%
      select(Age = ages, Brood.Year) %>%
      mutate(Forecast = NA)
    
    brief <- stock_data %>%
      select(Brood.Year, target_age$Age, Escapement, Recruits)
    
    N <- input$N
    s <- 0
    forecast <- data.frame(Brood.Year = unique(target_age$Brood.Year), 
                           Age = NA,
                           Forecast = NA)
    
    for (iyear in unique(target_age$Brood.Year)) {
      target_esc <- brief$Escapement[brief$Brood.Year == iyear]
      nearest_years <- brief %>%
        filter(Brood.Year < iyear) %>%
        mutate(diff = abs(Escapement - target_esc)) %>%
        arrange(diff) %>%
        head(N)
      
      if (nrow(nearest_years) < N) next
      
      total_recruit <- sum(nearest_years[, target_age$Age[target_age$Brood.Year == iyear]], na.rm = TRUE)
      total_esc <- sum(nearest_years$Escapement, na.rm = TRUE)
      forecast_age <- ifelse(total_esc > 0, target_esc * total_recruit / total_esc, NA)
      
      forecast$Age[forecast$Brood.Year == iyear] <- paste(sub("X", "", target_age$Age[target_age$Brood.Year == iyear]), collapse = ", ")
      forecast$Forecast[forecast$Brood.Year == iyear] <- forecast_age
      s <- s + forecast_age
    }
    
    forecast$Forecast <- round(forecast$Forecast, 0)
    forecast
    })
  
  # Render forecast table
  output$forecast_table <- renderTable({
    req(forecast())
    forecast()  %>%
       mutate(
         Brood.Year = as.character(as.integer(Brood.Year)),
         Age = as.character(Age),
         Forecast = format(Forecast, big.mark = ",", scientific = FALSE)
       ) %>%
       bind_rows(data.frame(
         Brood.Year = "Total", Age = "All",
         Forecast = format(sum(forecast()$Forecast, na.rm = TRUE), big.mark = ",", scientific = FALSE)
       ))
  })
  
}

# Run App
shinyApp(ui, server)