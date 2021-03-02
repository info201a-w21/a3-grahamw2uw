library(maps)
library(ggplot2)
library(mapproj)
library(dplyr)
library(tidyverse)
library(stringr)
library(patchwork)
library(styler)
library(lintr)
incarceration_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

prop_black_inmates_2018 <- incarceration_data %>%
  filter(year == "2018") %>%
  mutate(percent_black = black_pop_15to64 / total_pop_15to64) %>%
  drop_na(percent_black) %>%
  summarize(percent_black = sum(percent_black) / length(percent_black)) %>%
  pull(percent_black)

highest_black_prop_overall <- incarceration_data %>%
  mutate(percent_black = black_pop_15to64 / total_pop_15to64) %>%
  drop_na(percent_black) %>%
  filter(percent_black == max(percent_black)) %>%
  pull(percent_black)

highest_black_prop_overall_year <- incarceration_data %>%
  mutate(percent_black = black_pop_15to64 / total_pop_15to64) %>%
  drop_na(percent_black) %>%
  filter(percent_black == max(percent_black)) %>%
  pull(year)

state_highest_black_prop_2018 <- incarceration_data %>%
  filter(year == "2018") %>%
  mutate(percent_black = black_pop_15to64 / total_pop_15to64) %>%
  drop_na(percent_black) %>%
  filter(percent_black == max(percent_black)) %>%
  pull(state)

highest_number_of_black_inmates_2018 <- incarceration_data %>%
  filter(year == "2018") %>%
  filter(black_pop_15to64 == max(black_pop_15to64)) %>%
  pull(black_pop_15to64)

percent_black_over_time <- incarceration_data %>%
  mutate(percent_black = black_pop_15to64 / total_pop_15to64) %>%
  drop_na(percent_black) %>%
  group_by(year) %>%
  summarize(percent_black = sum(percent_black) / length(percent_black)) %>%
  mutate(change_in_prop = (percent_black - lag(percent_black, n = 1)) * 100) %>%
  drop_na(change_in_prop)

percent_white_over_time <- incarceration_data %>%
  mutate(percent_white = white_pop_15to64 / total_pop_15to64) %>%
  drop_na(percent_white) %>%
  group_by(year) %>%
  summarize(percent_white = sum(percent_white) / length(percent_white)) %>%
  mutate(change_in_prop = (percent_white - lag(percent_white, n = 1)) * 100) %>%
  drop_na(change_in_prop)

change_in_percentage_by_year <- ggplot() +
  geom_line(
    data = percent_black_over_time, aes(x = year, y = change_in_prop),
    color = "blue"
  ) +
  geom_line(
    data = percent_white_over_time, aes(x = year, y = change_in_prop),
    color = "red"
  ) +
  labs(
    x = "Year", y = "Percent Change", color = "Race",
    title = "Change in Racial Proportion by Year"
  ) +
  scale_colour_manual("",
    breaks = c("black", "white"),
    values = c("white" = "red", "black" = "blue")
  )

private_vs_public <- incarceration_data %>%
  mutate(percent_black = black_pop_15to64 / total_pop_15to64) %>%
  drop_na(private_jail_flag) %>%
  group_by(private_jail_flag, year) %>%
  summarize(percent_black = sum(percent_black) / length(percent_black)) %>%
  filter(year <= "2009" & year >= "2000")

private_vs_public_bar_chart <- ggplot(
  private_vs_public,
  aes(
    x = year, y = percent_black,
    fill = factor(private_jail_flag)
  )
) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(
    name = "Prison Type",
    breaks = c(0, 1),
    labels = c("Public", "Private")
  ) +
  xlab("Year") +
  ylab("Proportion of Black Inmates") +
  ggtitle("Proportion of Black Inmates for Public and Private Prisons by Year")

map_data_2018 <- incarceration_data %>%
  filter(state == "IL") %>%
  filter(year == "2018") %>%
  mutate(proportion_of_black_inmates = black_pop_15to64 / total_pop_15to64)

county_shapes <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")

map_data <- county_shapes %>%
  left_join(map_data_2018, by = "fips") %>%
  filter(state == "IL" & county_name != "unknown")

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

percentage_black_illinois_map <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(
      x = long, y = lat, group = group,
      fill = proportion_of_black_inmates
    ),
    color = "gray", size = 0.3
  ) +
  coord_map() +
  scale_fill_continuous(
    limits = c(
      0,
      max(map_data$proportion_of_black_inmates)
    ),
    na.value = "white", low = "light blue", high = "purple"
  ) +
  blank_theme +
  ggtitle("Proportion of Black Inmates in Illinois in 2018")
