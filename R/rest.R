rest <- function(dt){
  # extract the odd number of game 
  i <- as.numeric(ifelse(((as.numeric(dt$GameNo) + dt$game_) %%2 ==0)&(dt$game_ !=0)==TRUE,1,0))
  
  # extract the game that is odd and ends the set
  end <- as.integer(ifelse(dt$game_ + dt$set_ ==2, 1,0))
  
  # for the odd game that end the set, players get 120 second of break
  long = as.numeric(ifelse(end ==1, 2,0))
  
  # for the odd game that doesn't end the sent, players get 90 second of break
  short = as.numeric(ifelse(i == 1 & dt$set_ !=1,1.5,0))
  
  # incorporate short and long break into a vector
  dose = long + short
  dose <- c(0,dose)
  dose <- head(dose, -1)
  return(dose)
}
