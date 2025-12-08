library(shiny)
library(plotly)
library(dplyr)

olympics <- dataset_olympics


render_hist_age <- function(input, output) {
  
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
      marker = list(color = "#0078D0"),
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

