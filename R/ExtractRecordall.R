
extract_records_all <- function(dt1, dt2, name){
  require(readr)
  require(tidyverse)
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
  
  dt <- dt %>% arrange(match_num) 
  
  # Create new variables
  dt <- dt %>%
    #mutate(index_point = seq(1,length(dt$SetNo),1)) %>%
    mutate(game_ = as.integer(ifelse(dt$GameWinner == 0,0,1))) %>%
    mutate(round = as.factor((dt$match_num - 1000) %/% 100)) %>%
    mutate(match_num = as.factor(dt$match_num)) %>%
    mutate(round = as.numeric(substr(match_id, 15, 15))) %>%
    mutate(set_ = as.integer(ifelse(dt$SetWinner ==0, 0, 1))) %>%
    mutate(index_set = as.numeric(paste(match_num,SetNo, sep = ""))) %>%
    mutate(dist= ifelse(player1 == name, P1DistanceRun, P2DistanceRun)) %>%
    mutate(index_game = (paste(match_num,SetNo,GameNo, sep = ""))) %>%
    mutate(MatchNo = as.factor(substr(dt$match_num,2,2))) %>%
    mutate(time = c(0,as.vector(diff(dt$ElapsedTime)))) %>%
    mutate(cum_time = cumsum(time)/60)

  dt <- dt %>% filter(PointNumber != 0)
  return(dt)
}

