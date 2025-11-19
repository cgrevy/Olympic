library(shiny)
library(plotly)
library(dplyr)
library(tidyr)
library(viridisLite)
library(shinyWidgets)

olympics <- read.csv("/Users/mtue/Desktop/Data visualization/Eksamen project/archive/dataset_olympics.csv")

# ---- UI ----
ui <- fluidPage(
  titlePanel("Olympic Participants by Sport Over Time (Bubble Race)"),
  
  sidebarLayout(
    sidebarPanel(
      # Sport pickerInput with 2 random sports selected by default
      pickerInput(
        inputId = "sport",
        label = "Choose Sport(s):",
        choices = sort(unique(olympics$Sport)),
        selected = sample(unique(olympics$Sport), 2),
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,   # Adds select/deselect all buttons
          `live-search` = TRUE    # Search box for long lists
        )
      )
    ),
    
    mainPanel(
      plotlyOutput("bubbleParticipants", height = "700px")
    )
  )
)

# ---- SERVER ----
server <- function(input, output) {
  
  output$bubbleParticipants <- renderPlotly({
    
    # If no sport is selected, return empty plot
    if (is.null(input$sport) || length(input$sport) == 0) {
      return(plot_ly() %>% layout(title = "No sport selected"))
    }
    
    filtered <- olympics %>%
      filter(Sport %in% input$sport)
    
    # Summarize by Year + Sport
    sport_summary <- filtered %>%
      group_by(Year, Sport) %>%
      summarise(
        Participants = n_distinct(ID),
        .groups = "drop"
      )
    
    # Fill missing Year x Sport combinations with zeros
    all_years <- sort(unique(olympics$Year))
    sport_summary <- sport_summary %>%
      complete(
        Year = all_years,
        Sport = input$sport,
        fill = list(Participants = 0)
      )
    
    # Generate colors dynamically for selected sports
    sport_colors <- viridisLite::viridis(n = length(input$sport))
    
    # Create animated bubble chart
    plot_ly(
      data = sport_summary,
      x = ~Year,
      y = ~Participants,
      color = ~Sport,
      colors = sport_colors,
      frame = ~Year,
      text = ~paste(
        "Sport:", Sport,
        "<br>Year:", Year,
        "<br>Participants:", Participants
      ),
      hoverinfo = "text",
      type = 'scatter',
      mode = 'markers',
      marker = list(size = 20)  # fixed bubble size
    ) %>%
      layout(
        title = "Olympic Participants by Sport Over Time",
        xaxis = list(title = "Year", range = c(min(all_years), max(all_years))),
        yaxis = list(title = "Number of Participants"),
        showlegend = TRUE
      ) %>%
      animation_opts(frame = 1000, transition = 500, redraw = FALSE) %>%
      animation_slider(currentvalue = list(prefix = "Year: "))
    
  })
}

# ---- Run the app ----
shinyApp(ui = ui, server = server)
