# Install these if you haven't already
# install.packages(c("shiny", "tidyverse", "plotly"))

library(shiny)
library(tidyverse)
library(plotly)

# Read the dataset
olympics <- read.csv("/Users/mtue/Desktop/Data visualization/Eksamen project/archive/dataset_olympics.csv")

# Keep only rows where Medal is not NA
medalists <- olympics %>%
  filter(!is.na(Medal))

# Aggregate medals per athlete
athlete_medal_counts <- medalists %>%
  group_by(Name) %>%
  summarise(
    total_medals = n(),
    gold = sum(Medal == "Gold"),
    silver = sum(Medal == "Silver"),
    bronze = sum(Medal == "Bronze"),
    .groups = "drop"
  ) %>%
  arrange(desc(total_medals))


# Define UI
ui <- fluidPage(
  titlePanel("Most Winning Olympic Athletes"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("top_n", "Select top N athletes:", 
                  min = 5, max = 50, value = 10),
      checkboxGroupInput("medal_type", "Select medal types to display:",
                         choices = c("Gold", "Silver", "Bronze"),
                         selected = c("Gold", "Silver", "Bronze"))
    ),
    mainPanel(
      plotlyOutput("medalPlot"),
      tableOutput("athleteTable")
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    athlete_medal_counts %>%
      slice_max(order_by = total_medals, n = input$top_n)
  })
  
  output$medalPlot <- renderPlotly({
    data <- filtered_data()
    
    # Prepare for stacked bar by medal type
    plot_data <- data %>%
      select(Name, all_of(tolower(input$medal_type))) %>%
      pivot_longer(-Name, names_to = "Medal", values_to = "Count") %>%
      mutate(Medal = str_to_title(Medal))
    
    p <- ggplot(plot_data, aes(x = fct_reorder(Name, Count), y = Count, fill = Medal)) +
      geom_col() +
      coord_flip() +
      labs(
        x = "Athlete",
        y = "Number of Medals",
        title = paste("Top", input$top_n, "Olympic Athletes by Medals")
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$athleteTable <- renderTable({
    filtered_data()
  })
}

# Run the app
shinyApp(ui, server)
