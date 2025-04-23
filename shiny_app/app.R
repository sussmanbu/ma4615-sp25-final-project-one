library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)
library(scales)

# Load dataset
mass_data <- readRDS("../dataset/massachusetts_district_data.rds")

# UI
ui <- fluidPage(
  titlePanel("🎓 Massachusetts Graduation Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("district", "Choose a District:", choices = unique(mass_data$`District Name`)),
      sliderInput("year", "Select Year:", min = 2006, max = 2023, value = 2023, step = 1),
      br(),
      h5("Hover over the bars to see exact statistics.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Graduation Overview", plotlyOutput("gradPlot")),
        tabPanel("Grad vs Dropout", plotlyOutput("gradDropPlot")),
        tabPanel("Income vs Graduation", plotlyOutput("incomePlot")),
        tabPanel("Demographics", plotlyOutput("demographicPlot")),
        tabPanel("SAT Scores", plotlyOutput("satPlot")),
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Graduation Line Chart
  output$gradPlot <- renderPlotly({
    data <- mass_data %>%
      filter(`District Name` == input$district) %>%
      arrange(Year) %>%
      select(Year, Percent_Graduated)
    
    p <- ggplot(data, aes(x = Year, y = Percent_Graduated)) +
      geom_line(color = "#0A9396", size = 1.2) +
      geom_point(size = 2) +
      labs(title = paste("Graduation Rate Trend in", input$district),
           x = "Year", y = "Graduation Rate (%)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Grad vs Dropout
  output$gradDropPlot <- renderPlotly({
    filtered <- mass_data %>%
      filter(`District Name` == input$district, Year == input$year) %>%
      select(Percent_Graduated, Percent_Dropped_Out) %>%
      pivot_longer(cols = everything(), names_to = "Status", values_to = "Percent")
    
    p <- ggplot(filtered, aes(x = Status, y = Percent, fill = Status)) +
      geom_col(width = 0.5) +
      scale_fill_manual(values = c("Percent_Graduated" = "#0A9396", "Percent_Dropped_Out" = "#AE2012")) +
      labs(title = paste("Graduation vs Dropout Rate in", input$district, input$year),
           x = NULL, y = "Percent") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Income vs Graduation
  output$incomePlot <- renderPlotly({
    data <- mass_data %>% filter(Year == input$year)
    
    p <- ggplot(data, aes(x = Low_Income_Percent, y = Percent_Graduated,
                          text = paste("District:", `District Name`))) +
      geom_point(color = "#277DA1", alpha = 0.7) +
      labs(title = paste("Income vs Graduation Rate -", input$year),
           x = "Low Income %", y = "Graduation Rate %") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Demographics
  output$demographicPlot <- renderPlotly({
    data <- mass_data %>%
      filter(`District Name` == input$district, Year == input$year) %>%
      mutate(across(
        c(Percent_American_Indian, Percent_Asian, Percent_Black,
          Percent_Hispanic, Percent_Multi_Race, Percent_Pacific_Islander,
          Percent_White, Percent_Female, Percent_Male),
        ~ as.numeric(str_replace(.x, "%", ""))
      )) %>%
      pivot_longer(cols = starts_with("Percent_"), names_to = "Group", values_to = "Value") %>%
      filter(!is.na(Value))
    
    data$Group <- str_replace_all(data$Group, "Percent_", "")
    data$Group <- str_replace_all(data$Group, "_", " ")
    
    p <- ggplot(data, aes(x = reorder(Group, Value), y = Value, fill = Group)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(title = paste("Demographics in", input$district, input$year),
           x = NULL, y = "Percent (%)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # SAT Plot
  output$satPlot <- renderPlotly({
    filtered <- mass_data %>%
      filter(`District Name` == input$district, Year == input$year)
    
    sat_data <- tibble(
      Subject = c("Reading/Writing", "Math"),
      Score = c(as.numeric(filtered$SAT_Reading_Writing_Mean_Score),
                as.numeric(filtered$SAT_Math_Mean_Score))
    )
    
    p <- ggplot(sat_data, aes(x = Subject, y = Score, fill = Subject)) +
      geom_col(width = 0.5) +
      scale_fill_manual(values = c("Reading/Writing" = "#90BE6D", "Math" = "#F8961E")) +
      labs(title = paste("SAT Scores in", input$district, input$year),
           x = NULL, y = "Mean Score") +
      theme_minimal()
    
    ggplotly(p)
  })
}

# Run App
shinyApp(ui = ui, server = server)
