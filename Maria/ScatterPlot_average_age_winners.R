install.packages(c("shiny", "plotly", "dplyr", "shinyWidgets"))
library(shiny)
library(plotly)
library(dplyr)
library(shinyWidgets)

olympics <- read.csv("/Users/mtue/Desktop/Data visualization/Eksamen project/archive/dataset_olympics.csv")

# ---- UI ----
ui <- fluidPage(
  titlePanel("Average Age of Medal Winners Over Time (1896–2016)"),
  
  sidebarLayout(
    sidebarPanel(
      pickerInput(
        inputId = "sport",
        label = "Choose Sport(s):",
        choices = sort(unique(olympics$Sport)),
        selected = unique(olympics$Sport),
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,   # Adds select/deselect all buttons
          `live-search` = TRUE    # Search box for long lists
        )
      ),
      
      selectInput("medalType", "Choose Medal:",
                  choices = c("All", "Gold", "Silver", "Bronze"),
                  selected = "All"),
      
      sliderInput("yearRange", "Select Year Range:",
                  min = 1896, max = 2016,
                  value = c(1896, 2016),
                  step = 4, sep = "")
    ),
    
    mainPanel(
      plotlyOutput("agePlot")
    )
  )
)

# ---- SERVER ----
server <- function(input, output) {
  
  output$agePlot <- renderPlotly({
    
    # If no sport is selected, return empty plot
    if (is.null(input$sport) || length(input$sport) == 0) {
      return(plot_ly() %>% layout(title = "No sport selected"))
    }
    
    filtered <- olympics %>%
      filter(
        Year >= input$yearRange[1],
        Year <= input$yearRange[2],
        !is.na(Age),
        !is.na(Medal),
        Sport %in% input$sport
      )
    
    if (input$medalType != "All") {
      filtered <- filtered %>% filter(Medal == input$medalType)
    }
    
    # ---- Compute average age per year per sport ----
    avg_age_year_sport <- filtered %>%
      group_by(Year, Sport) %>%
      summarise(AverageAge = mean(Age, na.rm = TRUE), .groups = "drop")
    
    # ---- Scatter plot with line per sport ----
    plot_ly(avg_age_year_sport, x = ~Year, y = ~AverageAge, color = ~Sport,
            type = "scatter", mode = "lines+markers",
            hoverinfo = "text",
            hovertext = ~paste(
              "Year:", Year,
              "<br>Sport:", Sport,
              "<br>Average Age:", round(AverageAge, 2)
            )) %>%
      layout(
        title = "Average Age of Medal Winners Over Time by Sport",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Average Age"),
        hovermode = "closest"
      )
    
  })
}

# ---- Run the app ----
shinyApp(ui = ui, server = server)

