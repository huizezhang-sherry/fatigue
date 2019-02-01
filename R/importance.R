point_impt <- function(dt){
  temp <- dt
  
  # encode score from 1-15-30-40 to 0-1-2-3
  temp$P1Score <- ifelse(temp$P1Score ==0,0,
                         ifelse(temp$P1Score == 15,1,
                                ifelse(temp$P1Score==30,2,
                                       ifelse(temp$P1Score == 40,3,3))))
  temp$P2Score <- ifelse(temp$P2Score ==0,0,
                         ifelse(temp$P2Score == 15,1,
                                ifelse(temp$P2Score==30,2,
                                       ifelse(temp$P2Score == 40,3,3))))
  
  
  add_set_index <- function(dt){
    dt <- dt %>%
      mutate(p1_set = cumsum(as.numeric(ifelse(SetWinner ==1, 1, 0)))) %>% 
      mutate(p2_set = cumsum(as.numeric(ifelse(SetWinner ==2, 1, 0))))
  }
  
  data <- split(temp, as.numeric(temp$match_num))
  temp2 <- map_df(data, add_set_index)
  
  # serve_point/return_point: the current point score of the server/returner in 0-3 scale
  # serve_game/return_game: the current match score of the server/returner 
    #i.e. if the current match score is 4-6 for server, serve_game = 4, return_game = 6
  # serve_set/return_set: the current set score of the server/returner 
    # i.e. if the currnet set score is 0-2 for server, serve_set = 0, return_set = 2
  temp2 <- temp2 %>% group_by(match_num) %>% 
    mutate(P1GamesWon = ifelse(duplicated(SetNo, fromLast = TRUE),P1GamesWon,0))%>% 
    mutate(P2GamesWon = ifelse(duplicated(SetNo, fromLast = TRUE),P2GamesWon,0))
  
  temp3 <- temp2 %>% 
    mutate(serve_point = as.numeric(ifelse(ServeIndicator == 1, P1Score, P2Score))) %>% 
    mutate(return_point = as.numeric(ifelse(ServeIndicator == 1, P2Score, P1Score))) %>% 
    mutate(serve_game = as.numeric(ifelse(ServeIndicator == 1, P1GamesWon,P2GamesWon))) %>% 
    mutate(return_game = as.numeric(ifelse(ServeIndicator == 1, P2GamesWon, P1GamesWon))) %>% 
    mutate(serve_set = as.numeric(ifelse(ServeIndicator == 1, p1_set,p2_set))) %>% 
    mutate(return_set = as.numeric(ifelse(ServeIndicator == 1, p2_set,p1_set))) %>% 
    dplyr::select(serve_point, return_point, serve_game, return_game, serve_set, return_set)
  
  temp3 <- temp3 %>% 
    mutate(id = paste(serve_point, return_point, serve_game, return_game, serve_set, return_set, sep = ""))
  
  
  importance <- ifelse(dt$Gender == "Male", 
    atp_importance$importance[match(temp3$id, atp_importance$id)],
    wta_importance$importance[match(temp3$id, atp_importance$id)])
  
}

library(deuce)
library(purrr)
data(atp_importance)
data("wta_importance")


# To cite this article: Peter G. O'donoghue (2001) The Most Important Points in Grand Slam Singles Tennis, Research Quarterly for Exercise and Sport, 72:2, 125-131, DOI:10.1080/02701367.2001.10608942
