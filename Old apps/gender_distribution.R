# Gender dist
library(tidyverse)
library(dplyr)
library(ggplot2)
library(gganimate)

gender_count <- dataset_olympics %>% group_by(Games) %>% count(Sex)
gender_distribution <- dataset_olympics %>%
  group_by(Games, Sex) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = Sex, values_from = Count, values_fill = 0)

gender_count_summer <- dataset_olympics %>% filter(Season=="Summer") %>% 
                                                   group_by(Games) %>% count(Sex)

gender_percent_summer <- gender_count_summer %>% mutate(Percent = n /sum(n)*100)

# animated pie chart

plot <- ggplot(gender_count, aes(x = Games, y = n, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Gender Distribution in the Olympic Games",
       x = "Games",
       y = "Participants",
       fill = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p <- ggplot(gender_count_summer, aes(x = Sex, y = n, fill = Sex)) +
  geom_col() +
  labs(title = "Kønsfordeling under {closest_state}",
       x = "Køn",
       y = "Antal deltagere") +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("M" = "#1f77b4", "F" = "#ff7f0e")) +
  transition_states(Games, transition_length = 2, state_length = 3) +
  ease_aes('cubic-in-out')

pie_plot <- ggplot(gender_percent_summer, aes(x = "", y = Percent, fill = Sex)) +
  geom_col() +
  coord_polar(theta="y") +
  labs(title = "Kønsfordeling under {closest_state}",
       fill = "Gender") +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("M" = "#1f77b4", "F" = "#ff7f0e")) +
  transition_states(Games, transition_length = 2, state_length = 3) +
  ease_aes('cubic-in-out')

# Animate
p_a <-animate(p, nframes = 100, fps = 10, width = 800, height = 500, renderer = gifski_renderer())
anim_save("gender_1.gif", animation = p_a)

pie_an <-animate(pie_plot, nframes = 100, fps = 10, width = 800, height = 500, renderer = gifski_renderer())
anim_save("gender_pie.gif", animation = pie_an)

