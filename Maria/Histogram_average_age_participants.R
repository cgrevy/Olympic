install.packages(c("shiny", "plotly", "dplyr"))
library(shiny)
library(plotly)
library(dplyr)

olympics <- read.csv("/Users/mtue/Desktop/Data visualization/Eksamen project/archive/dataset_olympics.csv")

# ---- UI ----
ui <- fluidPage(
  titlePanel("Average Age of Participants per Olympic Game (1896–2016)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("season", "Choose Games:",
                  choices = c("All", "Summer", "Winter"),
                  selected = "All"),
      
      sliderInput("yearRange", "Select Year Range:",
                  min = 1896, max = 2016,
                  value = c(1896, 2016),
                  step = 4, sep = "")
    ),
    
    mainPanel(
      plotlyOutput("ageGameHistogram")
    )
  )
)

# ---- SERVER ----
server <- function(input, output) {
  
  output$ageGameHistogram <- renderPlotly({
    
    filtered <- olympics %>%
      filter(Year >= input$yearRange[1],
             Year <= input$yearRange[2],
             !is.na(Age))
    
    if (input$season != "All") {
      filtered <- filtered %>% filter(Season == input$season)
    }
    
    # Calculate average age and number of participants per game (Year + Season)
    avg_age_game <- filtered %>%
      group_by(Year, Season) %>%
      summarise(
        AverageAge = mean(Age, na.rm = TRUE),
        NumParticipants = n(),
        .groups = "drop"
      )
    
    # Define bins for histogram
    bin_width <- 1
    bins <- seq(floor(min(avg_age_game$AverageAge)), ceiling(max(avg_age_game$AverageAge)), by = bin_width)
    
    # Assign each game to a bin
    avg_age_game <- avg_age_game %>%
      mutate(bin = cut(AverageAge, breaks = bins, include.lowest = TRUE, right = FALSE))
    
    # Summarize per bin
    bin_summary <- avg_age_game %>%
      group_by(bin) %>%
      summarise(
        Count = n(),  # number of games in bin
        AvgAgeBin = round(mean(AverageAge), 2),  # average age across games in bin
        TotalParticipants = sum(NumParticipants),
        Games = paste(Year, Season, collapse = ", "),
        .groups = "drop"
      )
    
    # Wrap games for hover
    wrap_games <- function(games_string, n = 4) {
      games_vec <- strsplit(games_string, ", ")[[1]]
      chunks <- split(games_vec, ceiling(seq_along(games_vec)/n))
      paste(sapply(chunks, paste, collapse = ", "), collapse = "<br>")
    }
    bin_summary$Games_br <- sapply(bin_summary$Games, wrap_games)
    
    # Plot histogram
    plot_ly(
      data = bin_summary,
      x = ~bin,
      y = ~Count,
      type = "bar",
      text = ~Count,
      textposition = 'outside',
      hoverinfo = "text",
      hovertext = ~paste(
        "Age Range:", bin,
        "<br>Number of Games:", Count,
        "<br>Avg Age:", AvgAgeBin,
        "<br>Total Participants:", TotalParticipants,
        "<br>Games:<br>", Games_br
      )
    ) %>%
      layout(
        title = paste("Distribution of Average Age per Olympic Game -", input$season, "Games"),
        xaxis = list(title = "Average Age Bin"),
        yaxis = list(title = "Number of Games"),
        bargap = 0.1
      )
    
  })
}


# ---- Run the app ----
shinyApp(ui = ui, server = server)
