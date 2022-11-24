library(tidyverse)
library(dplyr)
library(plotly)
library(leaflet)
library(maps)

# The functions might be useful for A4
source("../source/a4-helpers.R")

# Load dataset
prison <- read_delim("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

## Section 2  ----

## Q1: What is the percentage of black individuals in jail compared to total jail population in the year 2018?
#----------------------#

# Total number of black people in jail 
black_jail_total <- prison %>%
  # filter to the most recent year (2018)
  filter(year == max(year)) %>%
  filter(
    # remove unknowns
    !is.na(black_jail_pop),
    !is.na(total_jail_pop)
  ) %>%
  # add up population
  summarize(
    black_total = sum(black_jail_pop)
  ) %>%
  pull(black_total)

# total number of individuals in jail
jail_total <- prison %>%
  # filter to most recent year (2018)
  filter(year == max(year)) %>%
  filter(
    # remove unknowns
    !is.na(black_jail_pop),
    !is.na(total_jail_pop)
  ) %>%
  # add up population
  summarize(
    jail_total = sum(total_jail_pop)
  ) %>%
  pull(jail_total)

# percentage of black and total individuals in jail
black_jail_prop <- round((black_jail_total / jail_total), 2) * 100
#--------------------------------#

## Q2:What year has the highest black population in jail?
#---------------------------#

# black population grouped by year
total_black_pop_year <- prison %>%
  group_by(year) %>%
  summarize(
    # add up population
    black_total = sum(black_jail_pop, na.rm = TRUE)
  )

# year with highest black population in jail
highest_black_pop_year <- total_black_pop_year %>%
  filter(black_total == max(black_total, na.rm = TRUE)) %>%
  pull(year)
#-------------------------------#

## Q3: What county has the highest population of black and latinx individuals in jail in 2018?
#-----------------------------#

## County with highest latinx jail population
county_highest_latinx_pop <- prison %>%
  # filter by most recent year (2018)
  filter(year == "2018") %>%
  # filter to the highest population
  filter(latinx_jail_pop == max(latinx_jail_pop, na.rm = TRUE)) %>%
  pull(county_name)

## County with highest black jail population
county_highest_black_pop <- prison %>%
  # filter by most recent year (2018)
  filter(year == "2018") %>%
  # filter to the highest population
  filter(black_jail_pop == max(black_jail_pop, na.rm = TRUE)) %>%
  pull(county_name)

#----------------------------------#

## Section 3  ----
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
#----------------------------------------------------------------------------#

## This function will be utilized in plot_jail_pop_for_us
get_year_jail_pop <- function() {
  jail_pop_total <- prison %>%
    # filter out unknown variables
    filter(!is.na(total_jail_pop)) %>%
    select(total_jail_pop, year)
  return(jail_pop_total)
}

# This function will use get_year_jail_pop(), using 
# year as the x variable and total_jail_pop as the
# y variable to create a graph using ggplot.
plot_jail_pop_for_us <- function() {
  plot <- ggplot(get_year_jail_pop(),
                 aes(x = year,
                     y = total_jail_pop)) +
    # Used to make a bar graph
    geom_col() +
    # Label the graph; x axis, y axis, title
    labs(
      x = "Year",
      y = "Total Jail Population",
      title = "Increase of Jail Population in U.S. (1970-2018)"
    ) +
    # To make the Y variables change to comma notation
    scale_y_continuous(labels = scales::comma)
  return(plot)
}

## Section 4  ----
#----------------------------------------------------------------------------#
# Growth of Prison Population by State

# Function that takes in states and find the total prison
# population by year for input states
get_jail_pop_by_states <- function(states) {
  jail_pop_state <- prison %>%
    # filter inputed states
    filter(state %in% states) %>%
    # get rid of unknowns
    filter(!is.na(total_prison_pop)) %>%
    group_by(state, year) %>%
    # sums up the population, keep all inputed states for plot
    summarize(
      total_prison_pop = sum(total_prison_pop), .groups = "keep"
    )
}

# function that uses get_jail_pop_by_states to plot a line graph
plot_jail_pop_by_states <- function(states) {
  plot <- ggplot(
    get_jail_pop_by_states(states),
    aes(x = year, 
        y = total_prison_pop, 
        # every state has a different color
        col = state)
  ) +
    # creates line graph
    geom_line() +
    # labels; x axis, y axis, title
    labs(
      x = "Year",
      y = "Total Prison Population",
      title = "Prison Population of Selected U.S. States (1970 - 2018)"
    )
  return(plot)
}

#----------------------------------------------------------------------------#

## Section 5  ----
## How does the Black and Latinx population in jail compare to the
## white population in each U.S. region?
#----------------------------------------------------------------------------#

# function that finds the combined percentage of Latinx and Black
# jail population compared to total jail population
black_latinx_jail_perc <- function() {
  black_and_latinx_jail_pop <- prison %>%
    # filter out unknowns
    filter(
      !is.na(latinx_jail_pop),
      !is.na(black_jail_pop),
      !is.na(white_jail_pop)
    ) %>%
    group_by(region) %>%
    # sums up the black and latinx jail population, divides it by the total
    # jail population, rounds it to 3 places, and turns it into a percentage, 
    summarize(
      black_latinx_total = round(((sum(black_jail_pop) + sum(latinx_jail_pop)) / sum(total_jail_pop, na.rm = TRUE)), 3) * 100
    )
}

# function that finds the percentage of white 
# jail population compared to total jail population
white_jail_perc <- function() {
  pop_white_jail <- prison %>%
    # filter out unknowns
    filter(
      !is.na(latinx_jail_pop),
      !is.na(black_jail_pop),
      !is.na(white_jail_pop)
    ) %>%
    group_by(region) %>%
    # sums up the white jail population, divides it by the total
    # jail population, rounds it to 3 places, and turns it into a percentage, 
    summarize(
      white_total <- round((sum(white_jail_pop) / (sum(total_jail_pop, na.rm = TRUE))), 3) * 100
    )
}

# combines these two functions into a single dataframe that
# will be used for the plot (x for black/latinx and y for white)
combine <- data.frame(white_jail_perc(), black_latinx_jail_perc()) 

# function that returns plot that compares percentages of jail
# population of white and latinx/black
black_latinx_white_perc_plot <- function() {
  plot <- ggplot(
    combine,
    aes(
      x = black_latinx_total,
      y = white_total.......,
      # different color for each region
      col = region,
      # labels that are shown when hovered over using ggplotly
      text = paste("Black/Latinx Percentage:", black_latinx_total, "%",
                   "\nWhite Percentage:", white_total......., "%",
                   "\nRegion:", region
      )
    )
  ) +
    geom_point(
      # increase size of point
      size = 6,
    ) +
    # labels; x axis, y axis, and title
    labs(
      x = "Combined Percentage of Black and Latinx population in Jail",
      y = "Percentage of White Population in Jail",
      title = "Combined Percentage of Black/Latinx and White Population in Jail in each Region of the U.S."
    )
  # makes plot interactive
  plot <- ggplotly(plot, tooltip = c("text"))
  return(plot)
}

#----------------------------------------------------------------------------#

## Section 6  
## How does the Latinx and Black jail population differ 
## from state to state in the U.S.? Are there any trends?
#----------------------------------------------------------------------------#

## gets the total black and latinx population by state
black_latinx_jail_map_data <- function() {
  black_latinx_jail_pop <- prison %>%
    group_by(state) %>% 
    ## get rid of unknowns
    filter(!is.na(black_jail_pop),
           !is.na(latinx_jail_pop),
           ## sort by most recent year
           year == max(year)) %>% 
    summarize(
      ## adds up total black and latinx population
      black_latinx_total = sum(black_jail_pop) + sum(latinx_jail_pop)
    )
}

## uses state.fips to combine to black_latinx_jail_map_data()
## to use for the section 6 map
states <- state.fips
## gets rid of anything after the : in polyname 
states$polyname <- gsub(":[[:alnum:][:space:][:punct:]]+", "", states$polyname)
## keeps distinct states, abb, and polyname
states <- distinct(states, abb, polyname)
## combines the two dataframes by the state abbreviation
mapdata <- inner_join(states, black_latinx_jail_map_data(), by = c("abb" = "state"))

## map that shows the combined black/latinx population differences
## by state
section6plot <- function() {
  # outlines the states on graph
  shape <- map_data("state") %>% 
    inner_join(mapdata, by = c("region" = "polyname"))
  plot6 <- ggplot(shape) +
    ## represents each state of US
    geom_polygon(
      aes(x = long,
          y = lat,
          group = group, fill = black_latinx_total),
      color = "black"
    ) +
    ## rid of excess materials; grid lines 
    theme_void() + 
    labs(
      title = "Combines Black and Latinx Jail Population in the U.S. (2018)",
      fill = "Black/Latinx Population"
    )
  return(plot6)
}

#----------------------------------------------------------------------------#
