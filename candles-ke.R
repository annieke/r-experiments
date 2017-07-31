# This function takes an input n, and simulates the first attempt to blow out n candles
blow_n_candles_once = function(n) {
  return(sample(1:n, 1))
}

# This function takes no inputs and returns the number of attempts to blow out 35 candles
# assuming that each blow extinguishes between 1 to all the remaining candles
blow_out_candles = function(n) {
  a = 0
  while (n > 0) {
    n = n - blow_n_candles_once(n)
    a = a + 1
  }
  return(a)
}

blow_and_plot = function(num_candles, num_repetitions) {
  all_experiments = replicate(num_repetitions, blow_out_candles(num_candles))
  
  all_experiments_table = table(all_experiments)
  
  barplot(all_experiments_table)
  
  return(all_experiments_table)
}

blow_1_to_100_candles = function() {
  n = 1
  avgs = numeric(100)
  while (n < 101) {
    avgs[n] = mean(replicate(1000, blow_out_candles(n)))
    n = n + 1
  }
  barplot(avgs)
  return(avgs)
}
