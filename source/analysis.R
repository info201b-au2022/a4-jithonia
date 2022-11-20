library(tidyverse)

# The functions might be useful for A4
source("../source/a4-helpers.R")

# Load dataset
prison <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

## Section 2  ---- 

## proportion of black people in jail and the total jail population in the
## most recent year. 
# Total number of black people in jail
black_jail_total <- prison %>% 
  filter(year == max(year)) %>% 
  filter(!is.na(black_jail_pop),
         !is.na(total_jail_pop)) %>% 
  summarize(
    black_total = sum(black_jail_pop)
  ) %>% 
  pull(black_total)

# total number of individuals in jail
jail_total <- prison %>% 
  filter(year == max(year)) %>% 
  filter(year == max(year)) %>% 
  filter(!is.na(black_jail_pop),
         !is.na(total_jail_pop)) %>% 
  summarize(
    jail_total = sum(total_jail_pop)
  ) %>% 
  pull(jail_total)

# proportion of black and total individuals in jail
black_jail_prop <- black_jail_total / jail_total

## 

total_female_jail_pop <- prison %>% 
  filter(year == max(year),
         !is.na(female_jail_pop),
         !is.na(male_jail_pop)) %>% 
  summarize(
    female_total = round(sum(female_jail_pop))
  ) %>% 
  pull(female_total)

total_male_jail_pop <- prison %>% 
  filter(year == max(year),
         !is.na(female_jail_pop),
         !is.na(male_jail_pop)) %>% 
  summarize(
    male_total = sum(male_jail_pop) 
  ) %>% 
  pull(male_total)

## County with highest prison population
year_county_highest_prison_pop <- prison %>% 
  filter(total_prison_pop == max(total_prison_pop, na.rm = TRUE)) %>%
  pull(year, county_name)

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
plot_jail_pop_for_us <- function()  {
  plot <- ggplot(get_year_jail_pop(), aes(x = year, y = total_jail_pop)) +
                   geom_col() +
                   labs(x = "Year", 
                        y = "Total Jail Population",
                        title = "Increase of Jail Population in U.S. (1970-2018)"
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
  plot <- ggplot(get_jail_pop_by_states(states),
                 aes(x = year, y = total_prison_pop, col = state)) +
    geom_line() +
    labs(x = "year", 
         y = "Total Prison Population",
         title = "-")
}

states_pop_plot <- plot_jail_pop_by_states(c("CA", "FL", "OH", "IL"))

#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#


#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


