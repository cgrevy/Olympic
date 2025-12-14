library(tibble)
library(plotly)

dataset_olympics_male_female <- dataset_olympics
dataset_olympics_male_female$Sex <- ifelse(dataset_olympics$Sex == "M", "Male", "Female")

dataset_olympics_male_female


introduction_text <- HTML("The Olympic Games creates a worldwide gathering point for the entire population. <br><br>
It is an event where people can lay their differences aside and come together in celebrating sports and achievements. But how have the Olympics evolved over time? 
And what can we learn from the data of its participants? <br><br>

Coming from sports backgrounds we have a personal interest in human performance and competition. It is interesting to see which metrics play a role in winning medals and producing top athletes, 
and how these differ between sports. <br><br>
Is an athlete just an athlete? Different sports favor different attributes, and how does this look? <br><br>

<b> The data </b> <br>
The data visualized in this project has been obtained from the website Kaggle. <br><br>

The dataset covers the name, gender, height, weight and age of the athlete, 
the year and sport they participated in, the team they represented, what committee they represented, 
whether they participated in summer or winter olympics and what medal they won, if any. 
It also contains the season, the city and the event. <br><br>

Explore the data by switching between the tabs on the left!")

hosts <- dataset_olympics %>%
  distinct(City) %>%
  arrange(City)

city_country <- tribble(
  ~City,               ~Country,
  "Albertville",       "France",
  "Amsterdam",         "Netherlands",
  "Antwerpen",         "Belgium",
  "Athina",            "Greece",
  "Atlanta",           "United States",
  "Barcelona",         "Spain",
  "Beijing",           "China",
  "Berlin",            "Germany",
  "Calgary",           "Canada",
  "Chamonix",          "France",
  "Cortina d'Ampezzo", "Italy",
  "Garmisch-Partenkirchen", "Germany",
  "Grenoble",          "France",
  "Helsinki",          "Finland",
  "Innsbruck",         "Austria",
  "Lake Placid",       "United States",
  "Lillehammer",       "Norway",
  "London",            "United Kingdom",
  "Los Angeles",       "United States",
  "Melbourne",         "Australia",
  "Mexico City",       "Mexico",
  "Montreal",          "Canada",
  "Moskva",            "Russian Federation",
  "Munich",            "Germany",
  "Nagano",            "Japan",
  "Oslo",              "Norway",
  "Paris",             "France",
  "Rio de Janeiro",    "Brazil",
  "Roma",              "Italy",
  "Salt Lake City",    "United States",
  "Sankt Moritz",      "Switzerland",
  "Sapporo",           "Japan",
  "Sarajevo",          "Bosnia and Herzegovina",
  "Seoul",             "Republic of Korea",
  "Sochi",             "Russian Federation",
  "Squaw Valley",      "United States",
  "St. Louis",         "United States",
  "Stockholm",         "Sweden",
  "Sydney",            "Australia",
  "Tokyo",             "Japan",
  "Torino",            "Italy",
  "Vancouver",         "Canada"
)

hex_to_rgba <- function(hex, alpha = 0.35) {
  rgb <- col2rgb(hex)
  sprintf("rgba(%d,%d,%d,%.2f)", rgb[1], rgb[2], rgb[3], alpha)
}


unique_events_by_games <- dataset_olympics %>%
  group_by(Event) %>%
  mutate(n_games = n_distinct(Games)) %>%
  ungroup() %>%
  filter(n_games == 1) %>% 
  distinct(Games, Event) %>%
  group_by(Games) %>%
  summarise(
    unique_events       = paste(sort(Event), collapse = ", "),
    unique_events_count = n(),
    .groups = "drop"
  )

games_summary <- dataset_olympics %>%
  left_join(city_country, by = "City") %>%
  group_by(Games, Year, City, Season, Country) %>%
  summarise(
    participants    = n_distinct(Name),
    n_sports        = n_distinct(Sport),
    n_countries_rep = n_distinct(NOC),
    .groups = "drop"
  )

games_summary <- games_summary %>%
  filter(!(City == "Stockholm" & Year == 1956))

games_timeline <- games_summary %>%
  left_join(unique_events_by_games, by = "Games") %>%
  mutate(
    unique_events       = replace_na(unique_events, "None"),
    unique_events_count = replace_na(unique_events_count, 0L)
  )

NOC_per_game <- plot_ly(
  data = games_summary,
  x = ~Games,
  y = ~n_countries_rep,
  color = ~Season,
  colors = c(
    "Summer" = "#FFCE19",
    "Winter" = "#00B4EB"
  ),
  type = 'scatter',
  mode = 'lines+markers'
) %>%
  layout(
    title = "Number of Countries represented at the Games",
    xaxis = list(title = "", tickangle = 45),
    yaxis = list(title = "Number of Countries")
  )

summer <- dataset_olympics_male_female %>% filter(Season=="Summer")
winter <- dataset_olympics_male_female %>% filter(Season=="Winter")

render_gender_summer <- function(input, output, valid_years_summer){
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

render_gender_winter <- function(input, output, valid_years_winter) {
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
}

render_ol_timeline <- function(output, games_timeline) {
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
}

render_participants_over_time <- function(input, output, olympics, session) {
  
  # Opdater sport-valg afhængigt af sæson
  observeEvent(input$season, {
    sports_available <- sort(unique(olympics$Sport[olympics$Season == input$season]))
    
    default_sel <-c("Equestrianism", "Gymnastics", "Swimming")
    
    updatePickerInput(
      session,
      inputId = "sport",
      choices = sports_available,
      selected = default_sel
    )
  })
  
  # Selve bubbleRace-plottet
  output$bubbleRace <- renderPlotly({
    
    # Hvis ingen sports er valgt
    if (is.null(input$sport) || length(input$sport) == 0) {
      return(
        plot_ly() %>% 
          layout(title = "No sport selected")
      )
    }
    
    # Filter data
    filtered <- olympics %>%
      dplyr::filter(
        Season == input$season,
        Sport  %in% input$sport
      )
    
    # Summaries
    sport_summary <- filtered %>%
      dplyr::group_by(Year, Sport) %>%
      dplyr::summarise(Participants = dplyr::n_distinct(ID), .groups = "drop")
    
    all_years <- sort(unique(filtered$Year))
    
    sport_summary <- sport_summary %>%
      tidyr::complete(
        Year = all_years,
        Sport = input$sport,
        fill = list(Participants = 0)
      ) %>%
      dplyr::arrange(Sport, Year)
    
    # ---- COLORS ----
    custom_colors <- c("#0078D0", "#FFB114", "#00A651", "#F0282D", "#000000")
    
    if (length(input$sport) > length(custom_colors)) {
      extra_needed <- length(input$sport) - length(custom_colors)
      set.seed(123)
      extra_colors <- grDevices::rainbow(extra_needed)
      sport_colors <- c(custom_colors, extra_colors)
    } else {
      sport_colors <- custom_colors[1:length(input$sport)]
    }
    
    sport_colors <- stats::setNames(sport_colors, input$sport)
    faded_colors <- sapply(sport_colors, hex_to_rgba)  # forudsætter at hex_to_rgba er defineret et sted
    
    p <- plot_ly()
    
    # ---- Animated traces ----
    for (yr in all_years) {
      for (sp in input$sport) {
        
        df_line  <- sport_summary %>% dplyr::filter(Sport == sp, Year <= yr)
        df_point <- df_line %>% dplyr::filter(Year == yr)
        
        show_leg <- (yr == min(all_years))
        
        # LINE trace
        p <- add_trace(
          p,
          data = df_line,
          x = ~Year,
          y = ~Participants,
          type = "scatter",
          mode = "lines",
          line = list(color = faded_colors[sp], width = 3),
          name = sp,
          frame = as.character(yr),
          hoverinfo = "none",
          showlegend = show_leg
        )
        
        # Bubble marker trace
        p <- add_trace(
          p,
          data = df_point,
          x = ~Year,
          y = ~Participants,
          type = "scatter",
          mode = "markers",
          marker = list(color = sport_colors[sp], size = 14),
          name = sp,
          frame = as.character(yr),
          hoverinfo = "text",
          text = ~paste(
            "Sport:", Sport,
            "<br>Year:", Year,
            "<br>Participants:", Participants
          ),
          showlegend = FALSE
        )
      }
    }
    
    p %>%
      layout(
        title = paste("Olympic Participants by Sport Over Time (", input$season, ")", sep = ""),
        xaxis = list(title = "Year"),
        yaxis = list(title = "Number of Participants"),
        showlegend = TRUE
      ) %>%
      animation_opts(frame = 1000, transition = 500, redraw = FALSE) %>%
      animation_slider(currentvalue = list(prefix = "Year: "))
  })
}


render_timeline_click <- function(output, games_timeline) {
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
}

render_height_medal <- function(input, output){
  # ---- Scatter: Height vs Medal count (by sport) ----
  output$height_medal <- renderPlot({
    req(input$selected_sport)
    
    # Base: one row per athlete (keep height and sex)
    athletes_base <- dataset_olympics_male_female %>%
      filter(Sport == input$selected_sport, Sex==input$selected_gender_height_tab, !is.na(Height)) %>%
      group_by(Name, Sex, Height) %>%
      summarise(.groups = "drop")  # distinct athletes with known height
    
    # Medal counts per athlete (within sport), including 0
    medals_per_athlete <- dataset_olympics_male_female %>%
      filter(Sport == input$selected_sport) %>%
      group_by(Name) %>%
      summarise(Medals = sum(!is.na(Medal)), .groups = "drop")
    
    df <- athletes_base %>%
      left_join(medals_per_athlete, by = "Name") %>%
      mutate(Medals = tidyr::replace_na(Medals, 0L))
    
    validate(
      need(nrow(df) > 0, paste0("No data for sport: ", input$selected_sport))
    )
    
    ggplot(df, aes(x = Height, y = Medals, color = Sex)) +
      geom_jitter(width = 0, height = 0.1, alpha = 0.6) +
      labs(
        title = paste("Height vs. medals –", input$selected_sport),
        x = "Height (cm)",
        y = "Medal count"
      ) +
      theme_minimal(base_size = 14) 
  })
}

render_height_weight <- function(input, output){
  
  output$height_weight <- plotly::renderPlotly({
    req(input$selected_gender, input$sports_selected)
    
    df_hw <- dataset_olympics_male_female %>%
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
    
    # ---- CUSTOM COLORS ----
    custom_colors <- c("#0078D0", "#FFB114", "#00A651", "#F0282D", "#000000")
    sports <- input$sports_selected
    
    extra_needed <- max(0, length(sports) - length(custom_colors))
    
    set.seed(123)  # gør random farver reproducerbare
    extra_colors <- if (extra_needed > 0) {
      grDevices::rainbow(extra_needed)  # RAINBOW tilfældige farver
    } else {
      character(0)
    }
    
    color_values <- setNames(
      c(
        custom_colors[seq_len(min(5, length(sports)))],
        extra_colors
      ),
      sports
    )
    
    # ---- GGPlot ----
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
      scale_color_manual(values = color_values) +
      labs(
        title = paste("Height vs Weight –", input$selected_gender),
        x = "Height (cm)",
        y = "Weight (kg)"
      ) +
      theme_minimal()
    
    plotly::ggplotly(gg, tooltip = "text")
  })
}

avg_age_per_sport <- dataset_olympics %>%
  filter(!is.na(Age)) %>%
  group_by(Sport) %>%
  summarise(
    mean_age = mean(Age)       
  ) %>%
  arrange(desc(mean_age)) %>%
  arrange(mean_age) %>%
  mutate(Sport = factor(Sport, levels = Sport))

gen_avg_age <- mean(avg_age_per_sport$mean_age)

avg_age_per_sport <- avg_age_per_sport %>%
  mutate(diff_from_avg = avg_age_per_sport$mean_age - gen_avg_age) # for tooltip

avg_age_per_sport_plot <- avg_age_per_sport %>%
  ggplot(aes(x = reorder(Sport, mean_age), y = mean_age, fill= mean_age)) +
  geom_col() +
  geom_hline(yintercept = gen_avg_age, linetype=3) +
  annotate(
    "text", label = "Avg age across sports",
    x = gen_avg_age, y = 30, size = 3, colour = "black"
  ) +
  scale_fill_gradient(
    low = "#AAE8B8",
    high = "#005A46",
    guide="none"
  ) +
  labs(
    title = "Average age per sport",
    x = "Sport",
    y = "Average age"
  ) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 75, vjust = 1.0, hjust=1))

avg_age_per_sport_plotly <- plot_ly(
  data = avg_age_per_sport,
  x = ~Sport,
  y = ~mean_age,
  type = "bar",
  # gradient based on mean_age
  marker = list(
    color = ~mean_age,
    colorscale = list(
      c(0, 1),
      c("#AAE8B8", "#005A46")
    ),
    showscale = FALSE
  ),
  # custom tooltip
  hovertemplate = paste(
    "<b>%{x}</b><br>",
    "Average age: %{y:.1f}<br>",
    "Difference from overall avg: %{customdata:.1f}<extra></extra>"
  ),
  customdata = ~diff_from_avg
  
) %>%
  # horizontal reference line (avg across sports)
  add_trace(
    x = ~Sport,
    y = rep(gen_avg_age, nrow(avg_age_per_sport)),
    type = "scatter",
    mode = "lines",
    line = list(dash = "dot", width = 1),
    hoverinfo = "none",
    showlegend = FALSE,
    inherit = FALSE
  ) %>%
  # annotation for the line
  layout(
    title = "Average age per sport",
    xaxis = list(title = "Sport", tickangle = 75),
    yaxis = list(title = "Average age"),
    annotations = list(
      list(
        x = 0.5,
        y = gen_avg_age,
        xref = "paper",
        yref = "y",
        text = "Avg age across sports",
        showarrow = FALSE,
        ax = 0,
        ay = -40
      )
    )
  )

render_top_countries_bar <- function(input, output) {
  
  output$top_team_bar <- renderPlotly({
    
    # Filter by year range
    filtered <- dataset_olympics %>%
      filter(Year >= input$yearRange[1],
             Year <= input$yearRange[2])
    
    # Filter by season if not "All"
    if (input$season != "All") {
      filtered <- filtered %>% filter(Season == input$season)
    }
    
    # Get all teams (even those with no medals)
    all_teams <- filtered %>%
      distinct(Team)
    
    # Count medals per team
    medal_counts <- filtered %>%
      filter(!is.na(Medal)) %>%
      group_by(Team) %>%
      summarise(TotalMedals = n(), .groups = "drop")
    
    # Join to include teams with 0 medals
    all_teams_medals <- all_teams %>%
      left_join(medal_counts, by = "Team") %>%
      mutate(TotalMedals = ifelse(is.na(TotalMedals), 0, TotalMedals)) %>%
      arrange(desc(TotalMedals))
    
    # Limit to top N unless "All" is selected
    if (input$topN != "All") {
      all_teams_medals <- all_teams_medals %>%
        slice_head(n = as.numeric(input$topN))
    }
    
    # Create interactive bar chart
    plot_ly(
      data = all_teams_medals,
      x = ~reorder(Team, TotalMedals),
      y = ~TotalMedals,
      type = "bar",
      hovertext = ~paste(
        "Team: ", Team,
        "<br>Total Medals: ", TotalMedals
      ),
      hoverinfo = "text",
      textposition = "none",
      marker = list(color = "#FFB114")
    ) %>%
      layout(
        title = paste0(
          if (input$topN == "All") "All Teams" else paste("Top", input$topN, "Teams"),
          " - ", input$season, " Olympic Games"
        ),
        xaxis = list(title = "Team", categoryorder = "total descending"),
        yaxis = list(title = "Total Medals")
      )
  })
}

render_avg_age_winners <- function(input, output) {
  
  output$agePlot <- renderPlotly({
    
    if (is.null(input$sportAge) || length(input$sportAge) == 0) {
      return(plot_ly() %>% layout(title = "No sport selected"))
    }
    
    filtered <- olympics %>%
      filter(
        Year >= input$yearRange[1],
        Year <= input$yearRange[2],
        !is.na(Age),
        !is.na(Medal),
        Sport %in% input$sportAge
      )
    
    if (input$medalType != "All") {
      filtered <- filtered %>% filter(Medal == input$medalType)
    }
    
    # ---- Compute average age per year per sport ----
    avg_age_year_sport <- filtered %>%
      group_by(Year, Sport) %>%
      summarise(AverageAge = mean(Age, na.rm = TRUE), .groups = "drop")
    
    sport_levels <- unique(avg_age_year_sport$Sport)
    
    # ---- DYNAMIC COLORS ----
    custom_colors <- c("#0078D0", "#FFB114", "#00A651", "#F0282D", "#000000")
    
    if (length(sport_levels) > length(custom_colors)) {
      extra_needed <- length(sport_levels) - length(custom_colors)
      set.seed(123)
      extra_colors <- grDevices::rainbow(extra_needed)
      colors_assigned <- c(custom_colors, extra_colors)
    } else {
      colors_assigned <- custom_colors[1:length(sport_levels)]
    }
    
    colors_assigned <- setNames(colors_assigned, sport_levels)
    
    # Create faded line colors (RGBA)
    line_colors_faded <- sapply(colors_assigned, hex_to_rgba, alpha = 0.35)
    
    # ---- BUILD PLOT ----
    p <- plot_ly()
    
    for (sp in sport_levels) {
      df_sp <- avg_age_year_sport %>% filter(Sport == sp)
      
      # --- Fixed: hover text generated outside plot_ly formula evaluation ---
      hover_text <- paste(
        "Year:", df_sp$Year,
        "<br>Sport:", df_sp$Sport,
        "<br>Average Age:", round(df_sp$AverageAge, 2)
      )
      
      # LINE TRACE
      p <- add_trace(
        p,
        data = df_sp,
        x = ~Year,
        y = ~AverageAge,
        type = "scatter",
        mode = "lines",
        line = list(color = unname(line_colors_faded[sp]), width = 3),
        name = sp,
        hoverinfo = "none",
        showlegend = TRUE
      )
      
      # MARKER TRACE
      p <- add_trace(
        p,
        data = df_sp,
        x = ~Year,
        y = ~AverageAge,
        type = "scatter",
        mode = "markers",
        marker = list(color = unname(colors_assigned[sp]), size = 7),
        hoverinfo = "text",
        hovertext = hover_text,
        name = sp,
        showlegend = FALSE
      )
    }
    
    p %>% layout(
      title = "Average Age of Medal Winners Over Time by Sport",
      xaxis = list(title = "Year"),
      yaxis = list(title = "Average Age"),
      hovermode = "closest"
    )
    
  })
}


render_top_countries_bubble <- function(input, output) {
  
  output$bubble_medals_won <- renderPlotly({
    
    filtered <- olympics
    if(input$season != "All") filtered <- filtered %>% filter(Season == input$season)
    if(input$medalType != "All") filtered <- filtered %>% filter(Medal == input$medalType)
    
    # Top 5 teams
    top_teams <- filtered %>%
      count(Team, name="TotalMedals") %>%
      arrange(desc(TotalMedals)) %>%
      slice_head(n = 5) %>%
      pull(Team)
    
    filtered <- filtered %>% filter(Team %in% top_teams)
    
    # Summaries
    country_summary <- filtered %>%
      group_by(Year, Team) %>%
      summarise(Medals = n(),
                Participants = n_distinct(ID),
                .groups = "drop")
    
    olympic_years <- sort(unique(country_summary$Year))
    
    # Fill missing combinations
    country_summary <- country_summary %>%
      complete(Year = olympic_years, Team = top_teams,
               fill = list(Medals = 0, Participants = 0)) %>%
      arrange(Team, Year)
    
    # Colors
    custom_colors <- c("#0078D0", "#FFB114", "#00A651", "#F0282D", "#000000")
    colors <- setNames(custom_colors[1:length(top_teams)], top_teams)
    faded_colors <- sapply(colors, hex_to_rgba)
    
    # ---- Build animated traces ----
    p <- plot_ly()
    
    for (yr in olympic_years) {
      for (team in top_teams) {
        
        df_line <- country_summary %>% filter(Team == team, Year <= yr)
        df_point <- df_line %>% filter(Year == yr)
        
        show_leg <- yr == min(olympic_years)
        
        # Faded line
        p <- add_trace(
          p,
          data = df_line,
          x = ~Year,
          y = ~Medals,
          type = "scatter",
          mode = "lines",
          line = list(color = faded_colors[team], width = 3),
          name = team,
          frame = as.character(yr),
          hoverinfo = "none",
          showlegend = show_leg
        )
        
        # Solid marker
        p <- add_trace(
          p,
          data = df_point,
          x = ~Year,
          y = ~Medals,
          type = "scatter",
          mode = "markers",
          marker = list(color = colors[team], size = 12),
          name = team,
          frame = as.character(yr),
          hoverinfo = "text",
          text = ~paste("Team:", Team,
                        "<br>Year:", Year,
                        "<br>Medals:", Medals,
                        "<br>Participants:", Participants),
          showlegend = FALSE
        )
      }
    }
    
    # ---- Layout & Animation ----
    p %>%
      layout(
        title = "Olympic Medals by Team Over Time (Top 5 Teams)",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Medals"),
        showlegend = TRUE
      ) %>%
      animation_opts(frame = 1000, transition = 500, redraw = FALSE) %>%
      animation_slider(currentvalue = list(prefix = "Year: "))
  })
}

