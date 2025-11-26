library(shiny)
library(plotly)
library(dplyr)
library(tidyr)
library(viridisLite)
library(shinyWidgets)

olympics <- read.csv("/Users/mtue/Desktop/Data visualization/Eksamen project/archive/dataset_olympics.csv")

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
    
    filtered <- olympics %>%
      filter(Sport %in% input$sport)
    
    # Summarize participants per sport per year
    sport_summary <- filtered %>%
      group_by(Year, Sport) %>%
      summarise(Participants = n_distinct(ID), .groups = "drop")
    
    all_years <- sort(unique(olympics$Year))
    
    # Fill missing Year x Sport combinations
    sport_summary <- sport_summary %>%
      complete(Year = all_years, Sport = input$sport, fill = list(Participants = 0)) %>%
      arrange(Sport, Year)
    
    sport_colors <- viridis(length(input$sport))
    names(sport_colors) <- input$sport
    
    # ---- Build animated traces with trailing lines ----
    p <- plot_ly()
    
    for(yr in all_years){
      for(sp in input$sport){
        df_line <- sport_summary %>% filter(Sport == sp & Year <= yr)
        
        show_leg <- ifelse(yr == min(all_years), TRUE, FALSE)
        
        p <- add_trace(
          p,
          data = df_line,
          x = ~Year,
          y = ~Participants,
          type = "scatter",
          mode = "lines+markers",
          line = list(color = sport_colors[sp], width = 2),
          marker = list(color = sport_colors[sp], size = 10),
          name = sp,
          frame = as.character(yr),
          hoverinfo = "text",
          text = ~paste("Sport:", Sport,
                        "<br>Year:", Year,
                        "<br>Participants:", Participants),
          showlegend = show_leg
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
