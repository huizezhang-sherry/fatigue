library(tidyverse)
Federer = extract_records_all("data/2017-ausopen-matches.csv", "data/2017-ausopen-points.csv", "Roger Federer")

break_point <- Federer %>% 
  # filter the break game
  filter((GameWinner != PointServer) & (GameWinner != 0)) %>% 
  select( player1, player2, SetNo, match_num,GameWinner,PointServer, point) %>% 
  mutate(win = PointServer - GameWinner) 

break1 <- break_point %>% 
  # if player 2 isFederer, win< 0 mean PointServer = 1GameWinner = 2, thus Federer breaks the game
  filter(player2 == 'Roger Federer') %>% filter(win<0)%>% 
  # chain the match_num and SetNo to form single index
  mutate(index = as.numeric(paste(match_num,SetNo, sep = "")))

break2 <- break_point %>% 
  # if player 1 isFederer, win> 0 mean PointServer = 2,GameWinner = 1, thus Federer breaks the game
  filter(player1 == 'Roger Federer') %>% filter(win>0)%>% 
  # chain the match_num and SetNo to form single index
  mutate(index = as.numeric(paste(match_num,SetNo, sep = "")))

# filter the row with double break
break1_sub <- break1 %>% group_by(index) %>% count() %>% filter(n>=2)
break2_sub <- break2 %>% group_by(index) %>% count() %>% filter(n>=2)


db_point1 <- break1 %>% filter(index %in% break1_sub$index) %>% 
  group_by(index) %>% filter(row_number() ==2)
db_point2 <- break2 %>% filter(index %in% break2_sub$index) %>% 
  group_by(index) %>% filter(row_number() ==2)

db_point = rbind(db_point1,db_point2) %>% arrange(match_num) 

d <- db_point %>% select(match_num, SetNo, point,index)

# select the columns from origin Federer for left_join
f <- Federer %>% select(match_num, SetNo, point,index)

# filter the point that are greater than the benchmark point 
point_impt <- f %>% 
  left_join(d,by =c("match_num", "SetNo","index"),suffix = c('_point', '_benchmark')) %>%
  filter(point_point > point_benchmark) %>% 
  mutate(point_impt = 1)

# create a dummy variable, which is equal to 1 if 
fd <- Federer%>%  mutate(point_impt = ifelse(point %in% point_impt$point_point,1,0))
fd$point_impt
