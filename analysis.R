library(ggplot2)
library(dplyr)
library(leaflet)
library(knitr)

shootings <- read.csv("data/shootings-2018.csv", stringsAsFactors = FALSE)

# summary information
num_shootings <- nrow(shootings)
lives_lost <- sum(shootings$num_killed)
city_most_impacted <- shootings %>%
  group_by(city) %>%
  mutate(casualties = num_killed + num_injured) %>%
  summarize(
    sum_casualties = sum(casualties)
  ) %>%
  filter(
    sum_casualties == max(sum_casualties)
  ) %>%
  select(city) %>%
  unlist(use.names = FALSE)
state_most_impacted <- shootings %>%
  group_by(state) %>%
  mutate(casualties = num_killed + num_injured) %>%
  summarize(sum_casualties = sum(casualties)) %>%
  filter(sum_casualties == max(sum_casualties)) %>%
  select(state) %>%
  unlist(use.names = FALSE)
state_least_impacted <- shootings %>%
  group_by(state) %>%
  mutate(
    casualties = num_killed + num_injured
  ) %>%
  summarize(
    sum_casualties = sum(casualties)
  ) %>%
  filter(sum_casualties == min(sum_casualties)) %>%
  select(state) %>%
  unlist(use.names = FALSE)

# summary table
summary_table <-shootings %>%
  group_by(state) %>%
  summarize(
    Deaths = sum(num_killed),
    Injured = sum(num_injured)
  ) %>%
  arrange(-Deaths, -Injured)

# Description of a particular incident
california_shooting <- shootings %>%
  filter(address == "99 Rolling Oaks Dr")
cal_date <- california_shooting %>%
  select(date) %>%
  unlist(use.names=FALSE)
cal_killed <- california_shooting %>%
  select(num_killed) %>%
  unlist(use.names=FALSE)
cal_injured <- california_shooting %>%
  select(num_injured) %>%
  unlist(use.names=FALSE)
cal_address <- california_shooting %>%
  select(address) %>%
  unlist(use.names=FALSE)

# An interactive map
map_plot <- leaflet(data = shootings) %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(lng = -96.196, lat = 38.703, zoom = 4) %>%
  addCircles(
    lat = ~lat,
    lng = ~long,
    stroke = FALSE,
    popup = paste(shootings$date, "<br>", shootings$num_killed, "killed", 
                  "<br>", shootings$num_injured, "injured"),
    radius = ~num_killed
  )

# Plot of choice
grouped_states <- shootings %>%
  mutate(
    month = format(as.Date(date, format="%B %d, %Y"), "%m"),
    casualties = num_killed + num_injured
  ) %>%
  group_by(month) %>%
  summarize(
    casualties = sum(casualties)
  ) 

casualties_per_month <- ggplot(data = grouped_states) +
  geom_col(mapping = aes(x = month, y = casualties)) +
  labs(
    title = "Number of Casualties (injuries and deaths) by Month in 2018",
    x = "Month",
    y = "Casualties"
  )
