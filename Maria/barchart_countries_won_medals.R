install.packages(c("shiny", "plotly", "dplyr"))
library(shiny)
library(plotly)
library(dplyr)

olympics <- read.csv("/Users/mtue/Desktop/Data visualization/Eksamen project/archive/dataset_olympics.csv")


# ---- UI ----
ui <- fluidPage(
  titlePanel("Olympic Medals by Team (1896–2016)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("season", "Choose Games:",
                  choices = c("All", "Summer", "Winter"),
                  selected = "All"),
      
      sliderInput("yearRange", "Select Year Range:",
                  min = 1896, max = 2016,
                  value = c(1896, 2016),
                  step = 4, sep = ""),
      
      selectInput("topN", "Number of Teams to Display:",
                  choices = c(10, 15, 25, 50, 100, "All"),
                  selected = 15)
    ),
    
    mainPanel(
      plotlyOutput("medalPlot")
    )
  )
)

# ---- SERVER ----
server <- function(input, output) {
  
  output$medalPlot <- renderPlotly({
    
    # Filter by year range
    filtered <- olympics %>%
      filter(Year >= input$yearRange[1],
             Year <= input$yearRange[2])
    
    # Filter by season if not "All"
    if (input$season != "All") {
      filtered <- filtered %>% filter(Season == input$season)
    }
    
    # Get all teams (even those with no medals)
    all_teams <- filtered %>%
      distinct(Team)
    
    # Count medals per team
    medal_counts <- filtered %>%
      filter(!is.na(Medal)) %>%
      group_by(Team) %>%
      summarise(TotalMedals = n(), .groups = "drop")
    
    # Join to include teams with 0 medals
    all_teams_medals <- all_teams %>%
      left_join(medal_counts, by = "Team") %>%
      mutate(TotalMedals = ifelse(is.na(TotalMedals), 0, TotalMedals)) %>%
      arrange(desc(TotalMedals))
    
    # ✅ Limit to top N unless "All" is selected
    if (input$topN != "All") {
      all_teams_medals <- all_teams_medals %>%
        slice_head(n = as.numeric(input$topN))
    }
    
    # Create interactive bar chart
    plot_ly(
      data = all_teams_medals,
      x = ~reorder(Team, TotalMedals),
      y = ~TotalMedals,
      type = "bar",
      text = ~paste(
        "Team: ", Team,
        "<br>Total Medals: ", TotalMedals
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        title = paste0(
          if (input$topN == "All") "All Teams" else paste("Top", input$topN, "Teams"),
          " - ", input$season, " Olympic Games"
        ),
        xaxis = list(title = "Team", categoryorder = "total descending"),
        yaxis = list(title = "Total Medals")
      )
  })
}


# ---- Run the app ----
shinyApp(ui = ui, server = server)

