install.packages(c("shiny", "plotly", "dplyr"))
library(shiny)
library(plotly)
library(dplyr)

olympics <- read.csv("/Users/mtue/Desktop/Data visualization/Eksamen project/archive/dataset_olympics.csv")

# ---- UI ----
ui <- fluidPage(
  titlePanel("Average Age of Medal Winners per Sport (1896–2016)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("season", "Choose Games:",
                  choices = c("All", "Summer", "Winter"),
                  selected = "All"),
      
      selectInput("medalType", "Choose Medal:",
                  choices = c("All", "Gold", "Silver", "Bronze"),
                  selected = "All"),
      
      sliderInput("yearRange", "Select Year Range:",
                  min = 1896, max = 2016,
                  value = c(1896, 2016),
                  step = 4, sep = "")
    ),
    
    mainPanel(
      plotlyOutput("ageHistogram")
    )
  )
)

# ---- SERVER ----
server <- function(input, output) {
  
  output$ageHistogram <- renderPlotly({
    
    filtered <- olympics %>%
      filter(Year >= input$yearRange[1],
             Year <= input$yearRange[2],
             !is.na(Age),
             !is.na(Medal))
    
    if (input$season != "All") {
      filtered <- filtered %>% filter(Season == input$season)
    }
    
    if (input$medalType != "All") {
      filtered <- filtered %>% filter(Medal == input$medalType)
    }
    
    avg_age_sport <- filtered %>%
      group_by(Sport) %>%
      summarise(AverageAge = mean(Age, na.rm = TRUE), .groups = "drop")
    
    bin_width <- 2
    bins <- seq(floor(min(avg_age_sport$AverageAge)), ceiling(max(avg_age_sport$AverageAge)), by = bin_width)
    
    avg_age_sport <- avg_age_sport %>%
      mutate(bin = cut(AverageAge, breaks = bins, include.lowest = TRUE, right = FALSE))
    
    bin_summary <- avg_age_sport %>%
      group_by(bin) %>%
      summarise(
        Count = n(),
        Sports = paste(Sport, collapse = ", "),
        .groups = "drop"
      )
    
    # Wrap sport names for hover (line breaks every 4 sports)
    wrap_sports <- function(sports_string, n = 4) {
      sports_vec <- strsplit(sports_string, ", ")[[1]]
      chunks <- split(sports_vec, ceiling(seq_along(sports_vec)/n))
      paste(sapply(chunks, paste, collapse = ", "), collapse = "<br>")
    }
    bin_summary$Sports_br <- sapply(bin_summary$Sports, wrap_sports)
    
    # Label on bars is always the count
    bin_summary$label <- bin_summary$Count
    
    plot_ly(
      data = bin_summary,
      x = ~bin,
      y = ~Count,
      type = "bar",
      text = ~label,
      textposition = 'outside',
      hoverinfo = "text",
      hovertext = ~paste(
        "Age Range:", bin,
        "<br>Number of Sports:", Count,
        "<br>Sports:<br>", Sports_br
      )
    ) %>%
      layout(
        title = paste("Distribution of Average Ages by Sport -", input$season, "Games"),
        xaxis = list(title = "Average Age Bin"),
        yaxis = list(title = "Number of Sports"),
        bargap = 0.1
      )
    
  })
}

# ---- Run the app ----
shinyApp(ui = ui, server = server)

