library(tidyverse)
library(ggplot2)
install.packages("sf")
library(sf)

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

# The ratio of black people in jail to white people in jail in the LA county
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

# This function creates a data frame that contains the total jail 
# population for every year that data set contains

get_year_jail_pop <- function() {
  
  year_jail_pop <- incarceration_trends %>%
    group_by(year) %>%
    summarize(total_year_jail_pop = sum(total_jail_pop, na.rm = TRUE))
  

return(year_jail_pop)   
}

View(get_year_jail_pop())



# This function creates a bar chart that represents the total U.S population in 
# jail from 1970 - 2018
plot_jail_pop_for_us <- function()  {
  
  plot <- ggplot(get_year_jail_pop(), aes(x=year, y = total_year_jail_pop)) +
    geom_bar(stat = "identity", alpha = 100) +
    theme_bw()+
    labs(title = "Increase of Jail Population in U.S (1970 - 2018)",
         x = "Year",
         y = "Total Jail Population",
         caption = "Figure 1: The U.S Prision Population over the years.") +
    ylim(0, 800000)
  
  return(plot)   
} 

plot_jail_pop_for_us()

#----------------------------------------------------------------------------#

## Section 4


#This function takes in a vector of states and creates a data set that contains
# the population in jail for every year of each state that was specified in the argument 
get_jail_pop_by_states <- function(states) {
  
  jail_pop_by_state <- incarceration_trends %>%
    group_by(state, year) %>%
    filter(state %in% (states)) %>%
    summarize(total_state_year_jail_pop = sum(total_jail_pop, na.rm = TRUE))
  
  return(jail_pop_by_state)
  
}


# This function takes in a vector of states, and creates a line chart that 
# tracks the population in jail of each  state from 1970 - 2018

plot_jail_pop_by_states <- function(states){

plot <- ggplot(get_jail_pop_by_states(states), aes(x = year, y = total_state_year_jail_pop, color = state)) +
                 geom_line() +
                 labs(title = "Increase of Jail Population in States (1970 - 2018)",
                      x = "Year",
                      y = "Jail Population",
                      caption = "Figure 2: The jail population for states in the U.S.")
return(plot)
}


#----------------------------------------------------------------------------#

## Section 5  


# This function takes in a state as an argument, and creates a data set that has 
# the white and black prison population for that state over the years 1970-2018
get_black_vs_white_jail_pop <- function(states) {
  
 black_vs_white <- incarceration_trends %>%
    group_by(state, year) %>%
    filter(state == states) %>%
    summarize(total_jail_black_pop = sum(black_jail_pop, na.rm = TRUE), total_jail_white_pop = sum(white_jail_pop, na.rm = TRUE))
  
  return(black_vs_white)
  
}

# This function takes in a state creates a line chart that shows the change in 
# black prison population vs white prison population over the years 1970-2018

plot_black_vs_white_jail_pop <- function(states){
  
  plot <- ggplot(get_black_vs_white_jail_pop(states), aes(x = year,)) +
    geom_line(aes(y = total_jail_white_pop,  color = "hp")) +
    geom_line(aes(y = total_jail_black_pop)) +
    labs(title = "Black vs White population in Jail",
         x = "Year",
         y = "Jail Population",
         caption = "The jail population for black vs. white people.")
  return(plot)
  
  
}


  


#----------------------------------------------------------------------------#

## Section 6  ---- 

get_black_to_white_ratio <- function(){

# create simplified data frame with just black and white prison population for each state
df <- incarceration_trends %>%
  group_by(state) %>%
  summarise(state_black_jail_pop = sum(black_jail_pop, na.rm = TRUE), state_white_jail_pop = sum(white_jail_pop, na.rm = TRUE))



#Create data frame that contains ratio of black people to white people in jail for each state
black_to_white_ratio <- mutate(df, ratio = state_black_jail_pop / state_white_jail_pop)

#Replace NA values with 0
black_to_white_ratio[is.na(black_to_white_ratio)] = 0


# add a column with the full name of each state
df2 <- black_to_white_ratio %>%
  mutate(region = tolower(state.name[match(black_to_white_ratio$state, state.abb)]))
  
# delete data from Washington DC
df3 <- df2[-8,]

# get longitude and latitude data 
location_data <- map_data("state")

#combine location data with the ratio data
final_data <- left_join(df3, location_data, by = "region")

#remove Alaska data
final_data1 <- final_data[-1,]


return(final_data1)
}


#Takes the data that in the previous function and creates a map chart out of it
plot_black_to_white_ratio <- function(){

plot1 <- ggplot(get_black_to_white_ratio()) +
  geom_polygon(aes(x = long, y = lat, fill = ratio), color = "black") +
  coord_map()+
  labs(title = "Ratio of Black to White incarcerated in the U.S",
       caption = "figure 3: The ratio of total black to white incarcerations throughout the U.S")


plot2 <- plot1 + scale_fill_gradient(name = "ratio", low = "white", high = "red")

plot2
   return(plot2)

}
plot_black_to_white_ratio()

