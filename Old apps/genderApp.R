library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(shinyWidgets)
library(plotly)

summer <- dataset_olympics_male_female %>% filter(Season=="Summer")
winter <- dataset_olympics_male_female %>% filter(Season=="Winter")

valid_years_summer <- sort(unique(summer$Year))
valid_years_winter <- sort(unique(winter$Year))

min_year <- min(dataset_olympics_male_female$Year, na.rm = TRUE)
max_year <- max(dataset_olympics_male_female$Year, na.rm = TRUE)

# Define UI
ui <- fluidPage(
  titlePanel("Olympic data"),
  tabsetPanel(
    tabPanel("Gender distribution for Winter Games",
             sliderTextInput(
               inputId = "year_winter",
               label   = "Choose Year",
               choices = valid_years_winter,
               selected = min(valid_years_winter),
               grid = TRUE,
               animate = TRUE
             ),
             plotlyOutput("area_winter")),
    tabPanel("Gender Distribution for Summer Games",
             sliderTextInput(
               inputId = "year_summer",
               label   = "Choose Year",
               choices = valid_years_summer,
               selected = min(valid_years_summer),
               grid = TRUE,
               animate = TRUE
             ),
             #plotOutput("pie_summer"),
             plotlyOutput("area_summer"))
    )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  output$area_winter <- renderPlotly({
    selected_year <- as.numeric(input$year_winter)
    
    gender_year <- dataset_olympics_male_female %>%
      filter(Season == "Winter",
             Year <= selected_year) %>%
      group_by(Sex, Year) %>%
      summarise(Count = n(), .groups = "drop") %>%
      group_by(Year) %>%
      mutate(Percent = Count / sum(Count) * 100) %>%
      ungroup() %>%
      mutate(
        Sex = factor(Sex, levels = c("Female", "Male")),
        hover_text = sprintf(
          "Year: %d<br>Gender: %s<br>Percent: %.1f%%",
          Year, Sex, Percent
        ))
    
    a_plot <- plotly::plot_ly() %>%
      add_trace(
        data = gender_year %>% filter(Sex == "Female"),
        x = ~Year,
        y = ~Percent,
        text = ~hover_text,
        hoverinfo = "text",
        type = "scatter",
        mode = "none",
        stackgroup = "one",
        name = "Female",
        fillcolor = "rgba(240,40,45,1)",   # <-- olympic red
        line = list(color = "rgba(240,40,45,1)")
      ) %>%
      add_trace(
        data = gender_year %>% filter(Sex == "Male"),
        x = ~Year,
        y = ~Percent,
        text = ~hover_text,
        hoverinfo = "text",
        type = "scatter",
        mode = "none",
        stackgroup = "one",
        name = "Male",
        fillcolor = "rgba(0,120,208,1)",   # <-- olympic blue
        line = list(color = "rgba(0,120,208,1)")
      ) %>%
      
      layout(
        title = paste("Gender distribution –", input$year_winter),
        xaxis = list(range = c(min(valid_years_winter), max(valid_years_winter))),
        yaxis = list(range = c(0, 100), title = "Percent"),
        shapes = list(
          list(
            type = "line",
            x0 = min(valid_years_winter),
            x1 = max(valid_years_winter),
            y0 = 50,
            y1 = 50,
            line = list(dash = "dot")
          )
        )
      ) %>%
      animation_opts(1000, easing = "elastic", redraw = FALSE)
  })
  
  
  output$area_summer <- renderPlotly({
    selected_year <- as.numeric(input$year_summer)
    
    gender_year <- dataset_olympics_male_female %>%
      filter(Season == "Summer",
             Year <= selected_year) %>%
      group_by(Sex, Year) %>%
      summarise(Count = n(), .groups = "drop") %>%
      group_by(Year) %>%
      mutate(Percent = Count / sum(Count) * 100) %>%
      ungroup() %>%
      mutate(
        Sex = factor(Sex, levels = c("Female", "Male")),
        hover_text = sprintf(
          "Year: %d<br>Gender: %s<br>Percent: %.1f%%",
          Year, Sex, Percent
        ))
    
    a_plot <- plotly::plot_ly() %>%
      add_trace(
        data = gender_year %>% filter(Sex == "Female"),
        x = ~Year,
        y = ~Percent,
        text = ~hover_text,
        hoverinfo = "text",
        type = "scatter",
        mode = "none",
        stackgroup = "one",
        name = "Female",
        fillcolor = "rgba(240,40,45,1)",   # <-- olympic red
        line = list(color = "rgba(240,40,45,1)")
      ) %>%
      add_trace(
        data = gender_year %>% filter(Sex == "Male"),
        x = ~Year,
        y = ~Percent,
        text = ~hover_text,
        hoverinfo = "text",
        type = "scatter",
        mode = "none",
        stackgroup = "one",
        name = "Male",
        fillcolor = "rgba(0,120,208,1)",   # <-- olympic blue
        line = list(color = "rgba(0,120,208,1)")
      ) %>%

      layout(
        title = paste("Gender distribution –", input$year_summer),
        xaxis = list(range = c(min(valid_years_summer), max(valid_years_summer))),
        yaxis = list(range = c(0, 100), title = "Percent"),
        shapes = list(
          list(
            type = "line",
            x0 = min(valid_years_summer),
            x1 = max(valid_years_summer),
            y0 = 50,
            y1 = 50,
            line = list(dash = "dot")
          )
        )
      ) %>%
      animation_opts(1000, easing = "elastic", redraw = FALSE)
  })
}

shinyApp(ui, server)

