library(tidyverse)
library(shiny)
library(ggplot2)
library(plotly)
source("prep_map.R", local = FALSE)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
    /* Base box styling */
    .timeline-details-box {
      border-radius: 10px;
      padding: 15px;
      box-shadow: 0 0 8px rgba(0, 0, 0, 0.08);
      margin-top: 10px;
    }

    /* Summer (yellow) */
    .summer-box {
      border: 2px solid #FFCE19;
      background-color: #FFF7CC;
    }

    /* Winter (blue) */
    .winter-box {
      border: 2px solid #00B4EB;
      background-color: #E7F7FF;
    }

    .timeline-details-box h4 {
      margin-top: 0;
      margin-bottom: 10px;
    }
  "))
  ),
  titlePanel("Olympic data"),
  tabsetPanel(
    tabPanel("Olympic timeline",
               column(
                 6,
                 plotlyOutput("ol_timeline", height = "1000px", width="400px")
               ),
               column(
                 6,
                 plotlyOutput("NOC_per_game", height="500px", width="600px"),
                 h3("Details"),
                 htmlOutput("timeline_details")
               )
             ),
    tabPanel("Avg age per sport",
             fluidRow(
               plotlyOutput("avg_age_per_sport_plot")
               )
             )
    )
  )
2
server <- function(input, output) {
  
    # ---- Vertikal tidslinje ----
    output$ol_timeline <- renderPlotly({
      # Vi laver en vertikal akse (Year) og x = 0 (ren vertikal linje)
      # Farve efter Season, bubble-size fx efter antal deltagere
      p <- plot_ly(
        data   = games_timeline,
        source = "ol_timeline",
        x      = ~ifelse(Season == "Summer", -1, 1),
        y      = ~Year,
        type   = "scatter",
        mode   = "markers+text",
        text   = ~paste0(City, " ", Year),
        textposition = ~ifelse(
          Season == "Summer",
          "middle right",
          "middle left"
        ),
        marker = list(
          size  = 15,  # bubble size
          line  = list(width = 1, color = "black")
        ),
        color = ~Season,
        colors = c("Summer" = "#FFCE19", "Winter" = "#00B4EB"),
        hoverinfo = "text",
        hovertext = ~paste0(
          "<b>", City, " ", Year, "</b><br>",
          "Season: ", Season, "<br>",
          "Country: ", Country, "<br>",
          "Participants: ", participants
        ),
        customdata = ~Games
      ) %>%
        layout(
          showlegend = FALSE,
          title = "Olympic Games Timeline",
          xaxis = list(
            title = "",
            showgrid = FALSE,
            showticklabels = FALSE,
            zeroline = FALSE
          ),
          yaxis = list(
            title = "",
            showgrid = FALSE,
            tickmode = "array",
            tickvals = sort(unique(games_timeline$Year))
          ),
          annotations = list(
            list(
              x = -1,
              y = 1.0,
              xref = "x",
              yref = "paper",
              text = "Summer",
              showarrow = FALSE,
              font = list(size = 16, color = "#FFCE19")
            ),
            list(
              x = 1,
              y = 1.0,
              xref = "x",
              yref = "paper",
              text = "Winter",
              showarrow = FALSE,
              font = list(size = 16, color = "#00B4EB")
            )
          )
        )
    })
    
    # ---- Click shows details for chosen Game ----
  observeEvent(plotly::event_data("plotly_click", source = "ol_timeline"), {
    click <- plotly::event_data("plotly_click", source = "ol_timeline")
    req(click)
    
    clicked_games <- click$customdata[1]
    
    info <- games_timeline %>%
      filter(Games == clicked_games) %>%
      slice(1)
    
    output$timeline_details <- renderUI({
      season_class <- ifelse(info$Season == "Summer", "summer-box", "winter-box")
      
      div(
        class = paste("timeline-details-box", season_class),
        
        tags$h4(paste0(info$City, " ", info$Year, " (", info$Season, ")")),
        tags$p(tags$b("Country: "), info$Country),
        tags$p(tags$b("Participants: "), format(info$participants, big.mark = ",")),
        tags$p(tags$b("Number of sports: "), info$n_sports),
        tags$p(tags$b("Countries represented: "), info$n_countries_rep),
        tags$hr(),
        tags$p(tags$b("Unique events (only at this Games):")),
        if (info$unique_events_count == 0) {
          tags$p("None")
        } else {
          tags$p(info$unique_events)
        }
      )
    })
    
  })

    
    output$timeline_details <- renderUI({
      tags$p("Click a bubble on the timeline to see details here.")
    })
    
    output$NOC_per_game <- renderPlotly({
      NOC_per_game
    })
    
    output$avg_age_per_sport_plot <- renderPlotly({
      avg_age_per_sport_plotly
    })
}
  

shinyApp(ui, server)


