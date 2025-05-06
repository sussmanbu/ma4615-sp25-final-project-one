library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)
library(scales)
library(sf)
library(stringr)

# === Optional: Increase max file size if needed ===
options(shiny.maxRequestSize = 30 * 1024^2)  # 30 MB

# === Load Data ===
shapefile_path <- "dataset_for_shiny/schooldistricts/SCHOOLDISTRICTS_POLY.shp"
ma_district_shapes <- st_read(shapefile_path) %>%
  mutate(District_Name_Upper = toupper(DISTRICT_N))

mass_data <- readRDS("dataset/massachusetts_district_data.rds") %>%
  mutate(
    Year = as.integer(Year),
    Percent_Graduated = as.numeric(Percent_Graduated),
    Low_Income_Percent = as.numeric(Low_Income_Percent),
    SAT_Math_Mean_Score = as.numeric(SAT_Math_Mean_Score),
    SAT_Reading_Writing_Mean_Score = as.numeric(SAT_Reading_Writing_Mean_Score),
    District_Name_Upper = toupper(`District Name`)
  )


# === UI ===
ui <- fluidPage(
  titlePanel("Massachusetts Graduation Equity Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("district", "Choose a District:", choices = sort(unique(mass_data$`District Name`))),
      sliderInput("year", "Select Year:", min = min(mass_data$Year), max = max(mass_data$Year), value = 2023),
      
      conditionalPanel(
        condition = "input.tabselected == 'Graduation Heatmap'",
        checkboxGroupInput("filters", "Filter Map by:", 
                           choices = c("Race Demographics" = "race", 
                                       "Gender" = "gender", 
                                       "Show Only Districts with SAT Data" = "sat"),
                           selected = NULL)
      )
    ),
    mainPanel(
      tabsetPanel(id = "tabselected",
        tabPanel("Income vs Graduation", 
                 plotlyOutput("incomePlot", height = "700px"),
                 br(),
                 uiOutput("districtInfo")),
        tabPanel("Graduation Heatmap", 
                 plotlyOutput("heatmapPlot", height = "700px"),
                 br(),
                 uiOutput("districtInfo"))
      )
    )
  )
)

# === Server ===
server <- function(input, output) {
  
  getFilteredData <- reactive({
    df <- mass_data %>% filter(Year == input$year)
    
    if ("sat" %in% input$filters) {
      df <- df %>%
        filter(!is.na(SAT_Math_Mean_Score) & !is.na(SAT_Reading_Writing_Mean_Score))
    }
    if ("race" %in% input$filters) {
      df <- df %>%
        filter_at(vars(Percent_White, Percent_Black, Percent_Hispanic, Percent_Asian), any_vars(!is.na(.)))
    }
    if ("gender" %in% input$filters) {
      df <- df %>%
        filter(!is.na(Percent_Female) & !is.na(Percent_Male))
    }
    
    df
  })
  
  output$incomePlot <- renderPlotly({
    req(input$year, input$district)
    data <- getFilteredData()
    selected <- input$district
    
    data <- data %>%
      mutate(
        is_selected = ifelse(`District Name` == selected, TRUE, FALSE),
        hover_text = paste0(
          "District: ", `District Name`, "<br>",
          "Graduation Rate: ", Percent_Graduated, "%<br>",
          "Low Income: ", Low_Income_Percent, "%"
        )
      )
    
    p <- ggplot(data, aes(x = Low_Income_Percent, y = Percent_Graduated)) +
      geom_point(aes(text = hover_text), alpha = 0.6, color = "#219EBC", size = 2.5) +
      geom_point(data = data %>% filter(is_selected),
                 aes(x = Low_Income_Percent, y = Percent_Graduated),
                 color = "red", size = 4) +
      geom_smooth(method = "lm", se = FALSE, color = "gray30", linetype = "dashed") +
      labs(title = paste("Income vs Graduation Rate -", input$year),
           x = "Low Income %", y = "Graduation Rate %") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$heatmapPlot <- renderPlotly({
    req(input$year, input$district)
    
    edu_data <- getFilteredData()
    req(nrow(edu_data) > 0)
    
    merged_data <- left_join(ma_district_shapes, edu_data, by = "District_Name_Upper")
    selected_upper <- toupper(input$district)
    
    merged_data$highlight <- ifelse(merged_data$District_Name_Upper == selected_upper, "selected", "other")
    merged_data <- merged_data %>%
      mutate(
        SAT_Avg = round((SAT_Math_Mean_Score + SAT_Reading_Writing_Mean_Score) / 2, 1),
        hover_text = paste0(DISTRICT_N, "\n",
                            "Graduation Rate: ", Percent_Graduated, "%\n",
                            "Low Income: ", Low_Income_Percent, "%\n",
                            "SAT Avg: ", SAT_Avg)
      )
    
    p <- ggplot(merged_data) +
      geom_sf(aes(fill = Percent_Graduated, text = hover_text), color = "white", size = 0.2) +
      geom_sf(data = merged_data %>% filter(highlight == "selected"), 
              fill = NA, color = "red", size = 1.5) +
      scale_fill_viridis_c(option = "magma", na.value = "grey90", name = "Grad Rate %") +
      labs(title = paste("Graduation Heatmap -", input$year)) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$districtInfo <- renderUI({
    req(input$year, input$district)
    df <- mass_data %>% filter(Year == input$year, `District Name` == input$district)
    if (nrow(df) == 0) return(NULL)
    
    tagList(
      h4(paste("Details for", input$district, "-", input$year)),
      tags$ul(
        tags$li(strong("Graduation Rate:"), paste0(df$Percent_Graduated, "%")),
        tags$li(strong("Low Income %:"), paste0(df$Low_Income_Percent, "%")),
        tags$li(strong("SAT Reading/Writing Avg:"), df$SAT_Reading_Writing_Mean_Score),
        tags$li(strong("SAT Math Avg:"), df$SAT_Math_Mean_Score),
        tags$li(strong("Gender:"), paste0("Female: ", df$Percent_Female, "% | Male: ", df$Percent_Male, "%")),
        tags$li(strong("Race:"), paste("White:", df$Percent_White, "% |",
                                       "Black:", df$Percent_Black, "% |",
                                       "Hispanic:", df$Percent_Hispanic, "% |",
                                       "Asian:", df$Percent_Asian, "%"))
      )
    )
  })
}

# === Launch ===
shinyApp(ui = ui, server = server)
