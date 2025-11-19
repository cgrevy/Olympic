install.packages(c("shiny", "plotly", "dplyr"))
library(shiny)
library(plotly)
library(dplyr)

olympics <- read.csv("/Users/mtue/Desktop/Data visualization/Eksamen project/archive/dataset_olympics.csv")

# ---- UI ----
ui <- fluidPage(
  titlePanel("Age Distribution of Olympic Athletes by Sport (1896–2016)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("sport", "Choose Sport:",
                  choices = c("All", sort(unique(olympics$Sport)))),
      
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
    
    # Filter data
    filtered <- olympics %>%
      filter(Year >= input$yearRange[1],
             Year <= input$yearRange[2],
             !is.na(Age))
    
    if (input$sport != "All") {
      filtered <- filtered %>% filter(Sport == input$sport)
    }
    
    # Summarize ages (1-year bins)
    age_summary <- filtered %>%
      mutate(AgeBin = Age) %>%
      group_by(AgeBin) %>%
      summarise(
        Participants = n(),
        .groups = "drop"
      )
    
    # Plot
    plot_ly(
      data = age_summary,
      x = ~AgeBin,
      y = ~Participants,
      type = "bar",
      hoverinfo = "text",
      hovertext = ~paste(
        "Age:", AgeBin,
        "<br>Participants:", Participants
      )
    ) %>%
      layout(
        title = paste("Age Distribution of Athletes - Sport:", input$sport),
        xaxis = list(
          title = "Age (years)",
          tickmode = "linear",
          dtick = 1,
          rangeslider = list(visible = TRUE),
          range = c(
            min(age_summary$AgeBin),
            min(age_summary$AgeBin) + (max(age_summary$AgeBin) - min(age_summary$AgeBin)) / 3
          )   # <--- Default zoom: 1/3 of the range
        ),
        yaxis = list(title = "Number of Athletes"),
        bargap = 0.05
      )
  })
}


# ---- Run the app ----
shinyApp(ui = ui, server = server)
