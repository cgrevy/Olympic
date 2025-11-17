library(tibble)
library(sf)
library(rnaturalearth)
library(plotly)

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

hosts_raw <- dataset_olympics %>%
  distinct(Games, Year, City, Name) %>%   # én række per atlet per Games
  left_join(city_country, by = "City") %>%
  group_by(Games, Year, Country, City) %>%
  summarise(
    Participants = n_distinct(Name),
    .groups = "drop"
  )

hosts_city_summary <- dataset_olympics %>%
  left_join(city_country, by = "City") %>%
  filter(!is.na(Country)) %>%
  group_by(Country, Games, Year, City) %>%
  summarise(
    participants        = n_distinct(Name),
    n_sports            = n_distinct(Sport),
    n_countries_rep     = n_distinct(NOC),
    .groups = "drop"
  )

host_country_summary <- hosts_city_summary %>%
  group_by(Country) %>%
  summarise(
    times_hosted = n(),
    games_block = paste0(
      "<b>", City, " ", Year, ":</b> ",
      "Participants: ", participants,
      ", Sports: ", n_sports,
      ", Countries: ", n_countries_rep,
      collapse = "<br>"
    ),
    .groups = "drop"
  ) %>%
  mutate(
    hover_text = paste0(
      "<b>Country: ", Country, "</b>",
      "<br>Games hosted: ", times_hosted, "<br>",
      games_block
    )
  )


world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(name_long, iso_a3, geometry)

world_hosts <- world %>%
  left_join(host_country_summary,
            by = c("name_long" = "Country"))


# static map
gg_host <- ggplot(world_hosts) +
  geom_sf(aes(fill = times_hosted, text = hover_text),
          color = NA) +
  scale_fill_gradient(
    name = "Times hosted",
    na.value = "grey90",
    low = "#deebf7", high = "#08519c"
  ) +
  labs(
    title = "Countries that have hosted the Olympic Games",
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text  = element_blank(),
    panel.grid = element_blank()
  )

#making it interactive
host_map <- ggplotly(gg_host, tooltip = "text") %>%
  layout(hovermode = "closest")

host_map

unique_events_by_games <- dataset_olympics %>%
  group_by(Event) %>%
  mutate(n_games = n_distinct(Games)) %>%
  ungroup() %>%
  filter(n_games == 1) %>%              # events kun med ved ét Games
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
    n_countries_rep = n_distinct(NOC),  # byt NOC ud hvis din kolonne hedder noget andet
    .groups = "drop"
  )

games_timeline <- games_summary %>%
  left_join(unique_events_by_games, by = "Games") %>%
  mutate(
    unique_events       = replace_na(unique_events, "None"),
    unique_events_count = replace_na(unique_events_count, 0L)
  )


