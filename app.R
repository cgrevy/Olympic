library(tidyverse)
library(shiny)
library(ggplot2)
library(plotly)
library(viridisLite)
library(shinyWidgets)
library(tidyr)

source("prep_map.R", local = FALSE)
source("AI.R", local=FALSE)
source("Hist_avg_age.R", local=FALSE)

olympics <- dataset_olympics
valid_years_summer <- sort(unique(summer$Year))
valid_years_winter <- sort(unique(winter$Year))

valid_sports_pa<- dataset_olympics_male_female %>%
  filter(
    !is.na(Height),
    !is.na(Weight)
  ) %>%
  pull(Sport) %>%
  unique() %>%
  sort()


ui <- fluidPage(

  # For styling:
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ), 
  titlePanel(
    div(
      style = "text-align: center;",
      img(src = "rings.svg", height = "40px", style = "vertical-align: middle; margin-right: 10px;"),
      span("Olympic Data Visualization", style = "font-size: 32px; font-weight: bold;")
    )
  ),
  
  navlistPanel(
    widths = c(2, 10),
    "Menu",
    
    tabPanel("Introduction ", h3("Introduction"),
             img(src="cover.jpg", height="300px", align="center"),
             p(introduction_text)),
    
    tabPanel("Size of the Games",
             titlePanel("Size of the Games"),
             fluidRow(
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
             fluidRow(
               h3("Participants over time"),
               sidebarLayout(
                 sidebarPanel(
                   radioButtons(
                     inputId = "season",
                     label   = "Choose season:",
                     choices = c("Summer", "Winter"),
                     selected = "Summer",
                     inline = TRUE
                   ),
                   
                   pickerInput(
                     inputId = "sport",
                     label   = "Choose Sport(s):",
                     choices = sort(unique(olympics$Sport[olympics$Season == "Summer"])),
                     multiple = TRUE,
                     options = list(
                       `actions-box` = TRUE,
                       `live-search` = TRUE
                     )
                   )
                   ),
                   mainPanel(
                     plotlyOutput("bubbleRace", height = "700px")
                   )
               )
            ),
            fluidRow(
              h3("Gender Distribution for Summer Games"),
              sliderTextInput(
                inputId = "year_summer",
                label   = "Choose Year",
                choices = valid_years_summer,
                selected = min(valid_years_summer),
                grid = TRUE,
                animate = TRUE
              ),
              plotlyOutput("area_summer")
              ),
            fluidRow(
              h3("Gender Distribution for Winter Games"),
              sliderTextInput(
                inputId = "year_winter",
                label   = "Choose Year",
                choices = valid_years_winter,
                selected = min(valid_years_winter),
                grid = TRUE,
                animate = TRUE
              ),
              plotlyOutput("area_winter")
            )
            ),
    
    tabPanel("Top Winning Nations",
             titlePanel("Top Winning Nations"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("season", "Choose Games:",
                             choices = c("All", "Summer", "Winter"),
                             selected = "All"),
                 
                 sliderInput("yearRange", "Select Year Range:",
                             min = 1896, max = 2016,
                             value = c(1896, 2016),
                             step = 4, sep = ""),
                 
                 selectInput("topN", "Number of Teams to Display:",
                             choices = c(10, 15, 25, 50, 100, "All"),
                             selected = 15)
               ),
               mainPanel(
                 plotlyOutput("top_team_bar")
               )
             ),
             h3("Olympic Medals by Team over Time"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("season", "Choose Games:", c("All","Summer","Winter")),
                 selectInput("medalType", "Choose Medal:", c("All","Gold","Silver","Bronze"))
               ),
               mainPanel(plotlyOutput("bubble_medals_won", height="700px"))
             )
             ),
    
    tabPanel("The Physics of an Olympian",
             titlePanel("The Physics of an Olympian"),
             h3("Height's influence on medals won"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("selected_gender_height_tab", "Select Sex", choices=unique(dataset_olympics_male_female$Sex)),
                 selectInput("selected_sport", "Select Sport", choices = sort(unique(dataset_olympics_male_female$Sport))),
               ),
               mainPanel(plotOutput("height_medal"))),
               h3("Height/Weight ratio for athletes"),
               selectInput("selected_gender", "Select Sex", choices=unique(dataset_olympics_male_female$Sex)), 
               checkboxGroupInput("sports_selected", "Select sports", 
                                  choices = valid_sports_pa, 
                                  selected=c("Basketball", "Gymnastics", "Nordic Combined", "Weightlifting")) %>% 
                 tagAppendAttributes(class = "multicol"),
               plotlyOutput("height_weight")
            ),
    tabPanel("Age in the Olympics",
             titlePanel("Age in the Olympics"),
               h3("Participant's Average Age"),
               plotlyOutput("avg_age_per_sport_plot")
               ,
             h3("Distribution of Age per Sport"),
               sidebarLayout(
                 sidebarPanel(
                   selectInput("sportHist", "Choose Sport:",
                               choices = c("All", sort(unique(olympics$Sport)))
                               ),

                   sliderInput("yearRange", "Select Year Range:",
                               min = 1896, max = 2016,
                               value = c(1896, 2016),
                               step = 4, sep = "")
                 ),
                 mainPanel(
                   plotlyOutput("ageHistogram")
                 )
               ),
             h3("Medal Winner's Average Age"),
             fluidRow(
               sidebarLayout(
                 sidebarPanel(
                   pickerInput(
                     inputId = "sportAge",
                     label = "Choose Sport(s):",
                     choices = sort(unique(olympics$Sport)),
                     selected = c("Gymnastics", "Athletics", "Equestrianism", "Shooting", "Swimming"),
                     multiple = TRUE,
                     options = list(
                       `actions-box` = TRUE, 
                       `live-search` = TRUE    
                     )
                   ),
                   
                   selectInput("medalType", "Choose Medal:",
                               choices = c("All", "Gold", "Silver", "Bronze"),
                               selected = "All"),
                   
                   sliderInput("yearRange", "Select Year Range:",
                               min = 1896, max = 2016,
                               value = c(1896, 2016),
                               step = 4, sep = "")
                 ),
                 
                 mainPanel(
                   plotlyOutput("agePlot")
                 )
               )
             )
             ),
    tabPanel("Most Winning Athletes (AI)",
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
             ),
    tabPanel("Report",
             )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Size of the games tab
  
  render_ol_timeline(output, games_timeline)
  render_timeline_click(output, games_timeline)
  
  render_participants_over_time(input, output, olympics, session) 
  

  output$timeline_details <- renderUI({
    tags$p("Click a bubble on the timeline to see details here.")
  })
  
  output$NOC_per_game <- renderPlotly({
    NOC_per_game
  })
  
  render_gender_winter(input, output, valid_years_winter)
  render_gender_summer(input, output, valid_years_summer)
  
  # Most winning countries tab
  render_top_countries_bar(input, output)
  render_top_countries_bubble(input, output)
  
  # Physical attr tab
  render_height_medal(input, output)
  render_height_weight(input, output)
  
  # Age tab
  output$avg_age_per_sport_plot <- renderPlotly({
    avg_age_per_sport_plotly
  })
  render_hist_age(input, output)
  render_avg_age_winners(input, output)
  
  # AI Tab
  render_ai_plot(input, output)
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
