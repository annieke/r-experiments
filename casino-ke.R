# This function takes no inputs and plays the game specified in the casino problem
play_game = function() {
  # n is our collective sum, which begins at 0
  n = 0
  # w is our winnings, which also begins at 0
  w = 0
  
  # we keep playing while the sum is less than 1
  while (n < 1) {
    # we win $10 every time it's less than 1
    w = w + 10
    # and we add the square of a random number from part (b)
    n = n + runif(1)^2
  }
  
  # we return the amount won
  return(w)
}

# This function also takes no inputs and plays the game I described in part (c)
play_new_game = function() {
  # we generate a random number before even playing
  n = runif(1)
  # the player pays $5 to play
  w = -5
  # then same rules apply
  while(n < 1) {
    w = w + 10
    n = n + runif(1)
  }
  return(w)
}

# This function takes an input n and plays the game n times and plots all the results
play_and_plot = function(n) {
  # we play the game n times
  all_games = replicate(n, play_new_game())
  # we count the frequency of each payoff
  all_games_table = table(all_games)
  
  # we plot the frequencies and return the avearge payoff
  barplot(all_games_table)
  return(mean(all_games))
}
