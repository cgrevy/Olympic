library(tidyverse)
library(shiny)
library(ggplot2)
source("prep_map.R", local = FALSE)

ui <- fluidPage(
  titlePanel("Olympic data"),
  tabsetPanel(
    tabPanel("Hosts of Olympic Games",
             plotlyOutput("host_map")),
    tabPanel("OL tidslinje",
             fluidRow(
               column(
                 7,
                 plotlyOutput("ol_timeline", height = "1000px", width="400px")
               ),
               column(
                 5,
                 h3("Details"),
                 htmlOutput("timeline_details")
               ))
             )
    )
  )

server <- function(input, output) {
  world <- map_data("world")
  output$host_map <- renderPlotly({
    host_map
  })
  
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
        colors = c("Summer" = "#E69F00", "Winter" = "#56B4E9"),
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
            showgrid = FALSE,
            showticklabels = FALSE,
            zeroline = FALSE
          ),
          yaxis = list(
            title = "Year",
            showgrid = FALSE,
            tickmode = "array",
            tickvals = sort(unique(games_timeline$Year))
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
        tagList(
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
}
  

shinyApp(ui, server)


