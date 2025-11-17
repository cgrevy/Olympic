library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(shinyWidgets)
library(plotly)


valid_sports<- dataset_olympics %>%
  filter(
    !is.na(Height),
    !is.na(Weight)
  ) %>%
  pull(Sport) %>%
  unique() %>%
  sort()

# Define UI
ui <- fluidPage(
  tags$head(tags$style(HTML("
    .multicol .shiny-options-group {
      display: grid;
      grid-template-columns: repeat(4, minmax(150px, 1fr)); /* 4 kolonner */
      grid-column-gap: 8px;
      grid-row-gap: 2px;
    }
  "))),
  
  titlePanel("Olympic data"),
  tabsetPanel(
    tabPanel("Height's influence on medals won",
             selectInput("selected_sport", "Select Sport", choices = sort(unique(dataset_olympics$Sport))),
             plotOutput("height_medal")),
    tabPanel("Height/Weight ratio for athletes",
             selectInput("selected_gender", "Select Sex", choices=unique(dataset_olympics$Sex)),
             checkboxGroupInput("sports_selected", "Select sports", 
                                choices = valid_sports) %>% 
               tagAppendAttributes(class = "multicol"),
             plotlyOutput("height_weight")
             )
      )
  )


# Define server logic required to draw a histogram ----
# SERVER
server <- function(input, output, session) {
  
  # ---- Scatter: Height vs Medal count (by sport) ----
  output$height_medal <- renderPlot({
    req(input$selected_sport)
    
    # Base: one row per athlete (keep height and sex)
    athletes_base <- dataset_olympics %>%
      filter(Sport == input$selected_sport, !is.na(Height)) %>%
      group_by(Name, Sex, Height) %>%
      summarise(.groups = "drop")  # distinct athletes with known height
    
    # Medal counts per athlete (within sport), including 0
    medals_per_athlete <- dataset_olympics %>%
      filter(Sport == input$selected_sport) %>%
      group_by(Name) %>%
      summarise(Medals = sum(!is.na(Medal)), .groups = "drop")
    
    df <- athletes_base %>%
      left_join(medals_per_athlete, by = "Name") %>%
      mutate(Medals = tidyr::replace_na(Medals, 0L))
    
    validate(
      need(nrow(df) > 0, paste0("No data for sport: ", input$selected_sport))
    )
    
    # Optional: Spearman correlation (robust for counts)
    cor_val <- suppressWarnings(cor(df$Height, df$Medals, method = "spearman"))
    subtitle_txt <- paste0("Spearman ρ = ", round(cor_val, 3),
                           "  (n = ", nrow(df), ")")
    
    ggplot(df, aes(x = Height, y = Medals, color = Sex)) +
      geom_jitter(width = 0, height = 0.1, alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(
        title = paste("Height vs. medals –", input$selected_sport),
        subtitle = subtitle_txt,
        x = "Height (cm)",
        y = "Medal count"
      ) +
      theme_minimal(base_size = 14) + 
      facet_wrap(~ Sex, ncol = 1, nrow = 2)
  })
  
  # ---- Height/Weight plot (by selected sex) ----
output$height_weight <- plotly::renderPlotly({
  req(input$selected_gender, input$sports_selected)

  df_hw <- dataset_olympics %>%
    filter(
      Sex == input$selected_gender,
      Sport %in% input$sports_selected,
      !is.na(Height),
      !is.na(Weight)
    ) %>%
    mutate(
      Height_m = Height / 100,
      HW_ratio = Height / Weight
    )

  validate(
    need(nrow(df_hw) > 0, "No data for that combination.")
  )

  gg <- ggplot(df_hw, aes(
    x = Height,
    y = Weight,
    color = Sport,
    text = paste(
      "Name: ", Name,
      "<br>Sport: ", Sport,
      "<br>Height: ", Height, " cm",
      "<br>Weight: ", Weight, " kg"
    )
  )) +
    geom_point(alpha = 0.7, size = 3) +
    labs(
      title = paste("Height vs Weight –", input$selected_gender),
      x = "Height (cm)",
      y = "Weight (kg)"
    ) +
    theme_minimal()

  plotly::ggplotly(gg, tooltip = "text")
  })
}



shinyApp(ui, server)

