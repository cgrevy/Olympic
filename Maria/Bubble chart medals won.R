library(shiny)
library(plotly)
library(dplyr)
library(tidyr)
library(viridisLite)

olympics <- read.csv("/Users/mtue/Desktop/Data visualization/Eksamen project/archive/dataset_olympics.csv")

# ---- UI ----
ui <- fluidPage(
  titlePanel("Olympic Medals by Team Over Time (Top 10 Teams with Animation)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("season", "Choose Games:", c("All","Summer","Winter")),
      selectInput("medalType", "Choose Medal:", c("All","Gold","Silver","Bronze"))
    ),
    mainPanel(plotlyOutput("bubbleRace", height="700px"))
  )
)

# ---- SERVER ----
server <- function(input, output) {
  
  output$bubbleRace <- renderPlotly({
    
    filtered <- olympics
    if(input$season != "All") filtered <- filtered %>% filter(Season == input$season)
    if(input$medalType != "All") filtered <- filtered %>% filter(Medal == input$medalType)
    
    # Top 10 teams
    top_teams <- filtered %>%
      count(Team, name="TotalMedals") %>%
      arrange(desc(TotalMedals)) %>%
      slice_head(n = 5) %>%
      pull(Team)
    
    filtered <- filtered %>% filter(Team %in% top_teams)
    
    # Summarize medals and participants per team per year
    country_summary <- filtered %>%
      group_by(Year, Team) %>%
      summarise(Medals = n(),
                Participants = n_distinct(ID),
                .groups = "drop")
    
    olympic_years <- sort(unique(country_summary$Year))
    
    # Fill missing year-team combinations
    country_summary <- country_summary %>%
      complete(Year = olympic_years, Team = top_teams,
               fill = list(Medals = 0, Participants = 0)) %>%
      arrange(Team, Year)
    
    colors <- viridis(length(top_teams))
    names(colors) <- top_teams
    
    # ---- Build animated traces with trailing lines ----
    p <- plot_ly()
    
    for(yr in olympic_years){
      for(team in top_teams){
        df_line <- country_summary %>% filter(Team == team & Year <= yr)
        
        # Only show legend for the first frame
        show_leg <- ifelse(yr == min(olympic_years), TRUE, FALSE)
        
        p <- add_trace(
          p,
          data = df_line,
          x = ~Year,
          y = ~Medals,
          type = "scatter",
          mode = "lines+markers",
          line = list(color = colors[team], width = 2),
          marker = list(color = colors[team], size = 10),
          name = team,
          frame = as.character(yr),
          hoverinfo = "text",
          text = ~paste("Team:", Team,
                        "<br>Year:", Year,
                        "<br>Medals:", Medals,
                        "<br>Participants:", Participants),
          showlegend = show_leg
        )
      }
    }
    
    # ---- Layout & Animation ----
    p %>%
      layout(
        title = "Olympic Medals by Team Over Time (Top 10 Teams)",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Medals"),
        showlegend = TRUE
      ) %>%
      animation_opts(frame = 1000, transition = 500, redraw = FALSE) %>%
      animation_slider(currentvalue = list(prefix = "Year: "))
  })
}

# ---- Run the app ----
shinyApp(ui, server)
