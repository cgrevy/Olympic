library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(shinyWidgets)

summer <- dataset_olympics %>% filter(Season=="Summer")
winter <- dataset_olympics %>% filter(Season=="Winter")

valid_years_summer <- sort(unique(summer$Year))
valid_years_winter <- sort(unique(winter$Year))

min_year <- min(dataset_olympics$Year, na.rm = TRUE)
max_year <- max(dataset_olympics$Year, na.rm = TRUE)

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
             plotOutput("pie_winter")),
    tabPanel("Gender Distribution for Summer Games",
             sliderTextInput(
               inputId = "year_summer",
               label   = "Choose Year",
               choices = valid_years_summer,
               selected = min(valid_years_summer),
               grid = TRUE,
               animate = TRUE
             ),
             plotOutput("pie_summer"))
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  output$pie_winter <- renderPlot({
    # Data: Count og procent for valgt år
    gender_year <- dataset_olympics %>%
      filter(Year == input$year_winter) %>%
      group_by(Sex) %>%
      summarise(Count = n(), .groups = "drop") %>%
      mutate(Percent = Count / sum(Count) * 100)
    
    validate(
      need(nrow(gender_year) > 0, "No data for chosen year.")
    )
    
    ggplot(gender_year, aes(x = "", y = Percent, fill = Sex)) +
      geom_col(width = 1, color = "white") +
      coord_polar(theta = "y") +
      geom_text(aes(label = paste0(round(Percent, 1), "%")),
                position = position_stack(vjust = 0.5), color = "white", size = 5) +
      labs(title = paste("Gender distribution –", input$year_winter), fill = "Gender") +
      theme_void(base_size = 14) +
      scale_fill_manual(values = c("M" = "#1f77b4", "F" = "#ff7f0e"))
  })
  
  
  
  output$pie_summer <- renderPlot({
    # Data: Count og procent for valgt år
    gender_year <- dataset_olympics %>%
      filter(Year == input$year_summer) %>%
      group_by(Sex) %>%
      summarise(Count = n(), .groups = "drop") %>%
      mutate(Percent = Count / sum(Count) * 100)
    
    validate(
      need(nrow(gender_year) > 0, "No data for chosen year.")
    )
    
    ggplot(gender_year, aes(x = "", y = Percent, fill = Sex)) +
      geom_col(width = 1, color = "white") +
      coord_polar(theta = "y") +
      geom_text(aes(label = paste0(round(Percent, 1), "%")),
                position = position_stack(vjust = 0.5), color = "white", size = 5) +
      labs(title = paste("Gender distribution –", input$year_summer), fill = "Gender") +
      theme_void(base_size = 14) +
      scale_fill_manual(values = c("M" = "#1f77b4", "F" = "#ff7f0e")) 
  })
}

shinyApp(ui, server)

