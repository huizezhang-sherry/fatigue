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
