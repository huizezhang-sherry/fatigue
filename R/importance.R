point_impt <- function(dt){
  temp <- dt
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

