library(shiny)
library(plotly)
library(dplyr)
library(tidyr)
library(viridisLite)
library(shinyWidgets)

olympics <- read.csv("/Users/mtue/Desktop/Data visualization/Eksamen project/archive/dataset_olympics.csv")

hex_to_rgba <- function(hex, alpha = 0.35) {
  rgb <- col2rgb(hex)
  sprintf("rgba(%d,%d,%d,%.2f)", rgb[1], rgb[2], rgb[3], alpha)
}

# ---- UI ----
ui <- fluidPage(
  titlePanel("Olympic Participants by Sport Over Time (Animated Line+Bubble Chart)"),
  
  sidebarLayout(
    sidebarPanel(
      pickerInput(
        inputId = "sport",
        label = "Choose Sport(s):",
        choices = sort(unique(olympics$Sport)),
        selected = sample(unique(olympics$Sport), 2),
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE
        )
      )
    ),
    
    mainPanel(
      plotlyOutput("bubbleRace", height = "700px")
    )
  )
)

# ---- SERVER ----
server <- function(input, output) {
  
  output$bubbleRace <- renderPlotly({
    
    if (is.null(input$sport) || length(input$sport) == 0) {
      return(plot_ly() %>% layout(title = "No sport selected"))
    }
    
    filtered <- olympics %>% filter(Sport %in% input$sport)
    
    # Summaries
    sport_summary <- filtered %>%
      group_by(Year, Sport) %>%
      summarise(Participants = n_distinct(ID), .groups = "drop")
    
    all_years <- sort(unique(olympics$Year))
    
    sport_summary <- sport_summary %>%
      complete(Year = all_years, Sport = input$sport,
               fill = list(Participants = 0)) %>%
      arrange(Sport, Year)
    
    # ---- IMPROVED COLOR SECTION ----
    custom_colors <- c("#0078D0", "#FFB114", "#00A651", "#F0282D", "#000000")
    
    if (length(input$sport) > length(custom_colors)) {
      extra_needed <- length(input$sport) - length(custom_colors)
      set.seed(123)
      extra_colors <- grDevices::rainbow(extra_needed)
      sport_colors <- c(custom_colors, extra_colors)
    } else {
      sport_colors <- custom_colors[1:length(input$sport)]
    }
    
    sport_colors <- setNames(sport_colors, input$sport)
    
    # Faded color version for lines
    faded_colors <- sapply(sport_colors, hex_to_rgba)
    # --------------------------------
    
    p <- plot_ly()
    
    # ---- Animated traces ----
    for (yr in all_years) {
      for (sp in input$sport) {
        
        df_line <- sport_summary %>% filter(Sport == sp, Year <= yr)
        df_point <- df_line %>% filter(Year == yr)
        
        show_leg <- yr == min(all_years)
        
        # Faded LINE trace
        p <- add_trace(
          p,
          data = df_line,
          x = ~Year,
          y = ~Participants,
          type = "scatter",
          mode = "lines",
          line = list(color = faded_colors[sp], width = 3),
          name = sp,
          frame = as.character(yr),
          hoverinfo = "none",
          showlegend = show_leg
        )
        
        # Solid bubble marker trace
        p <- add_trace(
          p,
          data = df_point,
          x = ~Year,
          y = ~Participants,
          type = "scatter",
          mode = "markers",
          marker = list(color = sport_colors[sp], size = 14),
          name = sp,
          frame = as.character(yr),
          hoverinfo = "text",
          text = ~paste("Sport:", Sport,
                        "<br>Year:", Year,
                        "<br>Participants:", Participants),
          showlegend = FALSE
        )
      }
    }
    
    p %>%
      layout(
        title = "Olympic Participants by Sport Over Time",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Number of Participants"),
        showlegend = TRUE
      ) %>%
      animation_opts(frame = 1000, transition = 500, redraw = FALSE) %>%
      animation_slider(currentvalue = list(prefix = "Year: "))
    
  })
}

# ---- Run the app ----
shinyApp(ui = ui, server = server)
