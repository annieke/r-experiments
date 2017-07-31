# This function takes an input n, and simulates the first attempt to blow out n candles
blow_n_candles_once = function(n) {
  # returns a random number between 1 and n inclusive
  return(sample(1:n, 1))
}

# This function takes an input n and returns the number of attempts to blow out n candles
# assuming that each blow extinguishes between 1 to all the remaining candles
blow_out_candles = function(n) {
  # a is the number of attempts, and we start with 0 attempts
  a = 0
  
  # while we still have candles left, we keep on blowing out candles
  # each time we blow out a number of candles, we subtract them from the count
  while (n > 0) {
    n = n - blow_n_candles_once(n)
    # each time we blow, the number of attempt increases by 1
    a = a + 1
  }
  
  # we return the number of attempts
  return(a)
}

# This function takes inputs num_candles and num_repetitions 
# it runs the simulations of blowing out num_candles candles num_repetitions times
blow_and_plot = function(num_candles, num_repetitions) {
  # we repeat blowing out candles the specified number of times and note down attempt counts
  all_experiments = replicate(num_repetitions, blow_out_candles(num_candles))
  
  # we count the frequency of each number of attempts
  all_experiments_table = table(all_experiments)
  
  # we plot the frequency count and return it
  barplot(all_experiments_table)
  return(all_experiments_table)
}

# This function takes no inputs and plots the average number of attempts needed to 
# blow out from 1 to 100 candles, averaging over 1,000 simulations 
blow_1_to_100_candles = function() {
  # n is the number of candles, and we start with 1
  n = 1
  # we keep our averages in a numeric list with 100 values
  avgs = numeric(100)
  
  # we simulate all candle counts from 1 to 100
  while (n < 101) {
    # we set each slot in the list to the average number of attempts needed to blow out
    # its index-number of candles over 1,000 simulations
    avgs[n] = mean(replicate(1000, blow_out_candles(n)))
    # we move on to the next candles count 
    n = n + 1
  }
  
  # we plot the averages and return them 
  barplot(avgs)
  return(avgs)
}
