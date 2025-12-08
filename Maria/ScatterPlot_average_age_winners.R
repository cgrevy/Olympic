install.packages(c("shiny", "plotly", "dplyr", "shinyWidgets"))
library(shiny)
library(plotly)
library(dplyr)
library(shinyWidgets)

olympics <- read.csv("/Users/mtue/Desktop/Data visualization/Eksamen project/archive/dataset_olympics.csv")

hex_to_rgba <- function(hex, alpha = 0.4) {
  rgb <- col2rgb(hex)
  sprintf("rgba(%d,%d,%d,%.2f)", rgb[1], rgb[2], rgb[3], alpha)
}

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
          `actions-box` = TRUE, 
          `live-search` = TRUE    
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
    
    sport_levels <- unique(avg_age_year_sport$Sport)
    
    # ---- DYNAMIC COLORS ----
    custom_colors <- c("#0078D0", "#FFB114", "#00A651", "#F0282D", "#000000")
    
    if (length(sport_levels) > length(custom_colors)) {
      extra_needed <- length(sport_levels) - length(custom_colors)
      set.seed(123)
      extra_colors <- grDevices::rainbow(extra_needed)
      colors_assigned <- c(custom_colors, extra_colors)
    } else {
      colors_assigned <- custom_colors[1:length(sport_levels)]
    }
    
    colors_assigned <- setNames(colors_assigned, sport_levels)
    
    # Create faded line colors (RGBA)
    line_colors_faded <- sapply(colors_assigned, hex_to_rgba, alpha = 0.35)
    
    # ---- BUILD PLOT ----
    p <- plot_ly()
    
    for (sp in sport_levels) {
      df_sp <- avg_age_year_sport %>% filter(Sport == sp)
      
      # --- Fixed: hover text generated outside plot_ly formula evaluation ---
      hover_text <- paste(
        "Year:", df_sp$Year,
        "<br>Sport:", df_sp$Sport,
        "<br>Average Age:", round(df_sp$AverageAge, 2)
      )
      
      # LINE TRACE
      p <- add_trace(
        p,
        data = df_sp,
        x = ~Year,
        y = ~AverageAge,
        type = "scatter",
        mode = "lines",
        line = list(color = unname(line_colors_faded[sp]), width = 3),
        name = sp,
        hoverinfo = "none",
        showlegend = TRUE
      )
      
      # MARKER TRACE
      p <- add_trace(
        p,
        data = df_sp,
        x = ~Year,
        y = ~AverageAge,
        type = "scatter",
        mode = "markers",
        marker = list(color = unname(colors_assigned[sp]), size = 7),
        hoverinfo = "text",
        hovertext = hover_text,
        name = sp,
        showlegend = FALSE
      )
    }
    
    p %>% layout(
      title = "Average Age of Medal Winners Over Time by Sport",
      xaxis = list(title = "Year"),
      yaxis = list(title = "Average Age"),
      hovermode = "closest"
    )
    
  })
}

# ---- Run the app ----
shinyApp(ui = ui, server = server)
