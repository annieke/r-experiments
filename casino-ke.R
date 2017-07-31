play_game = function() {
  n = 0
  w = 0
  while (n < 1) {
    w = w + 10
    n = n + runif(1)^2
  }
  return(w)
}

play_new_game = function() {
  n = runif(1)
  w = -5
  while(n < 1) {
    w = w + 10
    n = n + runif(1)
  }
  return(w)
}

play_and_plot = function(n) {
  all_games = replicate(n, play_new_game())
  
  all_games_table = table(all_games)
  
  barplot(all_games_table)
  return(mean(all_games))
}
