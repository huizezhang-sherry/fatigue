library(readr)
library(tidyverse)
library(plyr)

# Loading data
match17 <- read_csv("data/2017-ausopen-matches.csv")
points17<- read_csv("data/2017-ausopen-points.csv")
dt <- left_join(points17,match17)
dt <- dt %>% mutate(Gender = as.factor(ifelse(test = match_num <2000,yes = 'Male',no = 'Female')))

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


# Comments: create an object for records: record$dt, record$year, record$event, record$df, record$top_player
# df
df<-  %>% filter(df$ServeNumber == 0)
df <- df %>% 
  mutate(Count = seq(1,length(df$SetNo),1)) %>% 
  mutate(Round = (df$match_num -1000) %/% 100)

# top_player
top_player <- function(gender,n=10){
  p1 <- dt %>% 
    dplyr::select(player1,Gender) %>% 
    filter(Gender == gender) %>% 
    mutate(player1 = as.factor(player1)) %>% 
    # consider each player
    group_by(player1) %>% 
    # count how many rows
    tally() %>%
    arrange(desc(n))
  
  
  p2 <- dt %>% 
    dplyr::select(player2,Gender) %>% 
    filter(Gender == gender) %>% 
    mutate(player2 = as.factor(player2)) %>% 
    # consider each player
    group_by(player2) %>% 
    # count how many rows
    tally() %>%
    arrange(desc(n))
  
  
  colnames(p1) <- c("player", "count1")
  colnames(p2) <- c("player", "count2")
  
  player<- full_join(p1,p2, by="player")
  player$count2[is.na(player$count2)] <- 0
  player$count1[is.na(player$count1)] <- 0
  
  player <- player %>% 
    mutate(count = count1 + count2) %>% 
    #select only the columns needed: player & count
    dplyr::select(player,count) %>% 
    #arrange by count
    arrange(desc(count)) %>% 
    filter(!is.na(player)) %>%   
    # select the top n as per argument 
    top_n(n)
  
  return(player)
}

# Extracting top_player
male_player <- top_player("Male",20)
female_player <- top_player("Female",10)



# Double Fault
ggplot(DoubleFault,aes(x = SetNo, fill = as.factor(SetNo))) +
  geom_bar(aes(group = SetNo)) +
  facet_wrap(~Round, nrow = 2)

