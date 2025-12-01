library(shiny)
library(plotly)
library(dplyr)
library(tidyr)
library(viridisLite)

olympics <- read.csv("/Users/mtue/Desktop/Data visualization/Eksamen project/archive/dataset_olympics.csv")

hex_to_rgba <- function(hex, alpha = 0.35) {
  rgb <- col2rgb(hex)
  sprintf("rgba(%d,%d,%d,%.2f)", rgb[1], rgb[2], rgb[3], alpha)
}

# ---- UI ----
ui <- fluidPage(
  titlePanel("Olympic Medals by Team Over Time (Top 5 Teams with Animation)"),
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
    
    # Top 5 teams
    top_teams <- filtered %>%
      count(Team, name="TotalMedals") %>%
      arrange(desc(TotalMedals)) %>%
      slice_head(n = 5) %>%
      pull(Team)
    
    filtered <- filtered %>% filter(Team %in% top_teams)
    
    # Summaries
    country_summary <- filtered %>%
      group_by(Year, Team) %>%
      summarise(Medals = n(),
                Participants = n_distinct(ID),
                .groups = "drop")
    
    olympic_years <- sort(unique(country_summary$Year))
    
    # Fill missing combinations
    country_summary <- country_summary %>%
      complete(Year = olympic_years, Team = top_teams,
               fill = list(Medals = 0, Participants = 0)) %>%
      arrange(Team, Year)
    
    # Colors
    custom_colors <- c("#0078D0", "#FFB114", "#00A651", "#F0282D", "#000000")
    colors <- setNames(custom_colors[1:length(top_teams)], top_teams)
    faded_colors <- sapply(colors, hex_to_rgba)
    
    # ---- Build animated traces ----
    p <- plot_ly()
    
    for (yr in olympic_years) {
      for (team in top_teams) {
        
        df_line <- country_summary %>% filter(Team == team, Year <= yr)
        df_point <- df_line %>% filter(Year == yr)
        
        show_leg <- yr == min(olympic_years)
        
        # Faded line
        p <- add_trace(
          p,
          data = df_line,
          x = ~Year,
          y = ~Medals,
          type = "scatter",
          mode = "lines",
          line = list(color = faded_colors[team], width = 3),
          name = team,
          frame = as.character(yr),
          hoverinfo = "none",
          showlegend = show_leg
        )
        
        # Solid marker
        p <- add_trace(
          p,
          data = df_point,
          x = ~Year,
          y = ~Medals,
          type = "scatter",
          mode = "markers",
          marker = list(color = colors[team], size = 12),
          name = team,
          frame = as.character(yr),
          hoverinfo = "text",
          text = ~paste("Team:", Team,
                        "<br>Year:", Year,
                        "<br>Medals:", Medals,
                        "<br>Participants:", Participants),
          showlegend = FALSE
        )
      }
    }
    
    # ---- Layout & Animation ----
    p %>%
      layout(
        title = "Olympic Medals by Team Over Time (Top 5 Teams)",
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
