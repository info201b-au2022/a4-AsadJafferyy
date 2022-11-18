library(tidyverse)

# The functions might be useful for A4
source("~/info201/assignments/a4-AsadJafferyy/source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

test_query1()

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

test_query2()

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>

incarceration_trends <- read.csv('https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv')
View(incarceration_trends)

# number of counties in the data set
num_counties <- length(unique(incarceration_trends$county_name))

# the county with the most amount of incarcerations
most_people_in_jail <- incarceration_trends %>% 
  filter(total_jail_pop == max(total_jail_pop, na.rm = TRUE))

# The ratio of black people in jail to white people in jail 
la_black_white_ratio <- incarceration_trends %>% 
  filter(year == max(year)) %>%
  filter(state == "CA") %>%
  filter (county_name == "Los Angeles County") %>%
  mutate(ratio = black_jail_pop / white_jail_pop) %>%
  pull(ratio)

la_black_white_ratio

#average amount of black people in jail(all counties, and years)
average_black_jail_pop <- incarceration_trends %>%
  summarize(avg_pop = mean(black_jail_pop, na.rm = TRUE))%>%
  pull(avg_pop)

average_black_jail_pop

#average amount of white people in jail (all counties, and years)
average_white_jail_pop <- incarceration_trends %>%
  summarize(avg_pop = mean(white_jail_pop, na.rm = TRUE))%>%
  pull(avg_pop)

average_white_jail_pop


#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
get_year_jail_pop <- function() {
  # TODO: Implement this function 
return()   
}

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  # TODO: Implement this function 
  return()   
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


