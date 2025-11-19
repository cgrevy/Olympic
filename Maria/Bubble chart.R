library(shiny)
library(plotly)
library(dplyr)
library(tidyr)
library(viridisLite)

olympics <- read.csv("/Users/mtue/Desktop/Data visualization/Eksamen project/archive/dataset_olympics.csv")

# ---- UI ----
ui <- fluidPage(
  titlePanel("Olympic Medals by Team Over Time (Bubble Race)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("season", "Choose Games:",
                  choices = c("All", "Summer", "Winter"),
                  selected = "All"),
      
      selectInput("medalType", "Choose Medal:",
                  choices = c("All", "Gold", "Silver", "Bronze"),
                  selected = "All"),
      
      selectInput("topTeams", "Number of Top Teams to Display:",
                  choices = c(10, 20, 30, 40, 50),
                  selected = 10),
      
      # Info box about Olympic year structure
      helpText(HTML(
        "<b>Olympic Year Info:</b><br/>
        - 1924–1992: Summer and Winter Olympics occurred in the same year.<br/>
        - 1994 onwards: Winter Olympics shifted to occur 2 years after Summer Olympics."
      ))
    ),
    
    mainPanel(
      plotlyOutput("bubbleRace", height = "700px")
    )
  )
)

# ---- SERVER ----
server <- function(input, output) {
  
  output$bubbleRace <- renderPlotly({
    
    filtered <- olympics
    
    # Filter by season if needed
    if (input$season != "All") {
      filtered <- filtered %>% filter(Season == input$season)
    }
    
    # Filter by medal type if needed
    if (input$medalType != "All") {
      filtered <- filtered %>% filter(Medal == input$medalType)
    }
    
    # Determine top N teams based on total medals
    top_teams <- filtered %>%
      group_by(Team) %>%
      summarise(TotalMedals = n(), .groups = "drop") %>%
      arrange(desc(TotalMedals)) %>%
      slice_head(n = as.numeric(input$topTeams)) %>%
      pull(Team)
    
    # Filter to only top N teams
    filtered <- filtered %>% filter(Team %in% top_teams)
    
    # Summarize by Year + Team
    country_summary <- filtered %>%
      group_by(Year, Team) %>%
      summarise(
        Medals = n(),
        Participants = n_distinct(ID),
        .groups = "drop"
      )
    
    # Handle Olympic years correctly
    if (input$season == "All") {
      # Take all Olympic years from the original dataset (both Summer and Winter)
      olympic_years <- sort(unique(olympics$Year))
    } else {
      # Only years for the selected season
      olympic_years <- sort(unique(filtered$Year))
    }
    
    # Fill missing Year x Team combinations with zeros
    country_summary <- country_summary %>%
      complete(
        Year = olympic_years,
        Team = top_teams,
        fill = list(Medals = 0, Participants = 0)
      )
    
    # Adjust bubble sizes
    max_participants <- max(country_summary$Participants, na.rm = TRUE)
    desired_max_size <- 60
    sizeref_val <- 2.0 * max_participants / (desired_max_size^2)
    
    # Generate colors dynamically for top N teams
    team_colors <- viridisLite::viridis(n = length(top_teams))
    
    # Create animated bubble chart
    plot_ly(
      data = country_summary,
      x = ~Year,
      y = ~Medals,
      size = ~Participants,
      color = ~Team,
      colors = team_colors,
      frame = ~Year,
      text = ~paste(
        "Team:", Team,
        "<br>Year:", Year,
        "<br>Medals:", Medals,
        "<br>Participants:", Participants
      ),
      hoverinfo = "text",
      type = 'scatter',
      mode = 'markers',
      marker = list(sizemode = 'area', sizeref = sizeref_val)
    ) %>%
      layout(
        title = paste("Olympic Medals by Team Over Time (Top", input$topTeams, "Teams)"),
        xaxis = list(title = "Year", range = c(min(olympic_years), max(olympic_years))),
        yaxis = list(title = "Number of Medals"),
        showlegend = TRUE
      ) %>%
      animation_opts(frame = 1000, transition = 500, redraw = FALSE) %>%
      animation_slider(currentvalue = list(prefix = "Year: "))
    
  })
}

# ---- Run the app ----
shinyApp(ui = ui, server = server)
