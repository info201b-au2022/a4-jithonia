library(tidyverse)
library(dplyr)
library(plotly)
library(leaflet)

# The functions might be useful for A4
source("../source/a4-helpers.R")

# Load dataset
prison <- read_delim("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

## Section 2  ----

## proportion of black people in jail and the total jail population in the
## most recent year.
# Total number of black people in jail
black_jail_total <- prison %>%
  filter(year == max(year)) %>%
  filter(
    !is.na(black_jail_pop),
    !is.na(total_jail_pop)
  ) %>%
  summarize(
    black_total = sum(black_jail_pop)
  ) %>%
  pull(black_total)

# total number of individuals in jail
jail_total <- prison %>%
  filter(year == max(year)) %>%
  filter(year == max(year)) %>%
  filter(
    !is.na(black_jail_pop),
    !is.na(total_jail_pop)
  ) %>%
  summarize(
    jail_total = sum(total_jail_pop)
  ) %>%
  pull(jail_total)

# proportion of black and total individuals in jail
black_jail_prop <- round((black_jail_total / jail_total), 2) * 100

## year with the most black individuals in jail
total_black_pop_year <- prison %>%
  group_by(year) %>%
  summarize(
    black_total = sum(black_jail_pop, na.rm = TRUE)
  )

highest_black_pop_year <- total_black_pop_year %>%
  filter(black_total == max(black_total, na.rm = TRUE)) %>%
  pull(year)

## County with highest latinx jail population
county_highest_latinx_pop <- prison %>%
  filter(year == "2018") %>%
  filter(latinx_jail_pop == max(latinx_jail_pop, na.rm = TRUE)) %>%
  pull(county_name)

## County with highest black jail population
county_highest_black_pop <- prison %>%
  filter(year == "2018") %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = TRUE)) %>%
  pull(county_name)

## Section 3  ----
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
get_year_jail_pop <- function() {
  jail_pop_total <- prison %>%
    filter(!is.na(total_jail_pop)) %>%
    select(total_jail_pop, year)
  return(jail_pop_total)
}

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function() {
  plot <- ggplot(get_year_jail_pop(), aes(x = year, y = total_jail_pop)) +
    geom_col() +
    labs(
      x = "Year",
      y = "Total Jail Population",
      title = "Increase of Jail Population in U.S. (1970-2018)",
      caption = "---"
    ) +
    scale_y_continuous(labels = scales::comma)
  return(plot)
}

## Section 4  ----
#----------------------------------------------------------------------------#
# Growth of Prison Population by State

get_jail_pop_by_states <- function(states) {
  jail_pop_state <- prison %>%
    filter(state %in% states) %>%
    filter(!is.na(total_prison_pop)) %>%
    group_by(state, year) %>%
    summarize(
      total_prison_pop = sum(total_prison_pop)
    )
}

plot_jail_pop_by_states <- function(states) {
  plot <- ggplot(
    get_jail_pop_by_states(states),
    aes(x = year, y = total_prison_pop, col = state)
  ) +
    geom_line() +
    labs(
      x = "year",
      y = "Total Prison Population",
      title = "-",
      caption = "--"
    )
}

states_pop_plot <- plot_jail_pop_by_states(c("CA", "FL", "OH", "TX"))

#----------------------------------------------------------------------------#

## Section 5  ----
#----------------------------------------------------------------------------#
black_latinx_jail_perc <- function() {
  black_and_latinx_jail_pop <- prison %>%
    filter(
      !is.na(latinx_jail_pop),
      !is.na(black_jail_pop),
      !is.na(white_jail_pop)
    ) %>%
    group_by(region) %>%
    summarize(
      black_latinx_total = round(((sum(black_jail_pop) + sum(latinx_jail_pop)) / sum(total_jail_pop, na.rm = TRUE)), 3)
    )
}

white_jail_perc <- function() {
  pop_white_jail <- prison %>%
    filter(
      !is.na(latinx_jail_pop),
      !is.na(black_jail_pop),
      !is.na(white_jail_pop)
    ) %>%
    group_by(region) %>%
    summarize(
      white_total <- round((sum(white_jail_pop) / (sum(total_jail_pop, na.rm = TRUE))), 3)
    )
}

combine <- data.frame(white_jail_perc(), black_latinx_jail_perc())


section5plot <- function() {
  plot <- ggplot(
    combine,
    aes(
      x = black_latinx_total,
      y = white_total.......,
      col = region
    )
  ) +
    geom_point(
      size = 6,
    ) +
    labs(
      x = "Percentage of White population in Jail",
      y = "Percentage of Black and Latinx population in Jail",
      title = "Percentage of Black/Latinx and White Population in Jail in each Region of the U.S."
    )
  plot <- ggplotly(plot)
  return(plot)
}

#----------------------------------------------------------------------------#

## Section 6  ----
#----------------------------------------------------------------------------#
section6 <- function() {

}

section6plot <- function() {

}
#----------------------------------------------------------------------------#


## Load data frame ----
