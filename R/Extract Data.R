library(readr)
library(tidyverse)
library(plyr)

# Loading tennis data
# match17 <- read_csv("data/2017-ausopen-matches.csv")
# points17<- read_csv("data/2017-ausopen-points.csv")
# dt <- left_join(points17,match17)
# dt <- dt %>% mutate(Gender = as.factor(ifelse(test = match_num <2000,yes = 'Male',no = 'Female')))

# Function for extracting the top_player


# Function for extracting records for an individual player
extract_records <- function(dt1, dt2, name){
  # read in the data 
  match <- read_csv(dt1)
  point <- read_csv(dt2)
  data <- left_join(match, point) %>% 
    mutate(Gender = as.factor(ifelse(test = match_num <2000,yes = 'Male',no = 'Female')))
  dt1 <- data[data$player1 == name,]
  dt2 <- data[data$player2 == name,]
  dt <- rbind(dt1,dt2)
  
  ## tidying data
  dt<- dt[!is.na(dt$match_id),]
  # Re-arrange dt based on match_num
  
  dt <- dt %>% arrange(match_num) %>% 
    # Create a column Position
    mutate(Position = as.integer(ifelse(player1 == name, 1,2)))
    
  # Filter for the rows that are served by the chosen player 
  dt <- dt %>% filter(dt$ServeIndicator == dt$Position) 
  
  # Filter out the rows with ServeNumber == 0 (DoubleFault)
  dt <- dt %>% filter(as.factor(dt$ServeNumber) != 0)
    
  # Create new variables
  dt <- dt %>% 
    mutate(Count = seq(1,length(dt$SetNo),1)) %>% 
    mutate(Round = as.factor((dt$match_num - 1000) %/% 100)) %>% 
    mutate(match_num = as.factor(dt$match_num))
  
  return(dt)
}

# Extract records for individual players
nadal <-  extract_records("data/2017-ausopen-matches.csv", "data/2017-ausopen-points.csv", "Rafael Nadal")
Federer <- extract_records("data/2017-ausopen-matches.csv", "data/2017-ausopen-points.csv", "Roger Federer")


