# Barchart for showing countries with most medals won

library(tidyverse)
library(dplyr)

medals_only <- dataset_olympics %>%
  filter(!is.na(Medal))

summer_medals <- medals_only %>% filter(Season == "Summer")

summer_medals_count <- summer_medals %>% count(NOC, sort=TRUE)

winter_medals <- medals_only %>% filter(Season == "Winter")

winter_medals_count <- winter_medals %>% count(NOC, sort= TRUE)

medal_count_country <- medals_only %>% count(NOC, sort = TRUE)

top_5_overall <- slice_max(medal_count_country, order_by = n, n=5)
top_5_winter <- slice_max(winter_medals_count, order_by = n, n=5)
top_5_summer <- slice_max(summer_medals_count, order_by = n, n=5)

ggplot(top_5_overall, aes(NOC, n)) + geom_col()
