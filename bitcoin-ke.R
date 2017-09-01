# this function was given in the assignment 
prices_over_time = function(number_of_time_increments, initial_price) {
  return(cumprod(c(initial_price,exp(0.02*rnorm(number_of_time_increments)))));
}

# this function performs Strategy A given in the problem
invest_strat_a = function() {
  # load prices
  prices = prices_over_time(10000, 100)
  # make an array to keep net worth over time 
  net_worth = numeric(10000)
  # we start with 100 dollars 
  net_worth[1] = 1000
  # we first buy all our coins at 100 dollars
  bought_price = 100 
  # which gives us 10 coins
  num_coins = 10
  # we haven't sold any coins yet
  sell_price = 0
  # our last action was to buy coins 
  just_bought = TRUE
  # start on day 2 since we just traded for day 1
  n = 2
  # before we hit the last day
  while (n <= 10000) {
    # if our last action was buying, and the prices are now more than 20% higher
    if (prices[n] > bought_price * 1.2 && just_bought) {
      # we sell all our bitcoins
      sell_price = prices[n]
      net_worth[n] = num_coins * sell_price
      just_bought = FALSE 
    } else {
      # if our last action was selling, and the current price is more than 10% lower
      if (prices[n] < 0.9 * sell_price && !just_bought) {
        # we buy bitcoins with all our cash 
        bought_price = prices[n]
        num_coins = net_worth[n - 1] / bought_price
        net_worth[n] = num_coins * bought_price
        just_bought = TRUE
      } else { 
        if (just_bought) {
          net_worth[n] = num_coins * prices[n]
        } else {
          net_worth[n] = net_worth[n - 1]
        }
      }
    }
    n = n + 1
  }
  #plot(net_worth, type="l")
  
  # return the difference between net worth and the worth of 10 bitcoins at the end
  return(net_worth[10000] - 10*prices[10000])
}

# this function performs Strategy B given in the problem 
invest_strat_b = function() {
  # load prices
  prices = prices_over_time(10000, 100)
  # we first use $200 to buy 2 coins and keep the rest in cash
  num_coins = 2
  cash = 800
  # keep net worths as an array and set day 1 to 1000 
  net_worth = numeric(10000)
  net_worth[1] = 1000
  # start on day 2
  n = 2
  while (n <= 10000) {
    # if the price is higher than yesterday
    if (prices[n] > prices[n-1]) {
      # sell 20% of bitcoins 
      cash = 0.2 * num_coins * prices[n] + cash
      num_coins = 0.8 * num_coins 
      net_worth[n] = cash + num_coins * prices[n]
    } else {
      # if the price is lower than yesterday 
      if (prices[n] < prices[n-1]) {
        # use 20% of cash to buy bitcoins 
        num_coins = 0.2 * cash / prices[n] + num_coins
        cash = 0.8 * cash
        net_worth[n] = cash + num_coins * prices[n]
      } else {
        net_worth[n] = cash + num_coins * prices[n]
      }
    }
    n = n + 1
  }
  #plot(net_worth, type="l")
  
  # return the difference between net worth and the worth of 10 bitcoins at the end
  return(net_worth[10000] - 10*prices[10000])
}

# a function that takes n as its input and repeats investment strategy A n times
repeat_strat_a = function(n) {
  # store the final outcomes of investment (networth vs price of bitcoin)
  all_invests = replicate(n, invest_strat_a())
  
  all_invests_table = table(all_invests)
  
  barplot(all_invests_table)
  
  # returns the median outcome 
  return(median(all_invests))
}

# a function that takes n as its input and repeats investment strategy B n times
repeat_strat_b = function(n) {
  # store the final outcomes of investment (networth vs price of bitcoin)
  all_invests = replicate(n, invest_strat_b())
  
  all_invests_table = table(all_invests)
  
  barplot(all_invests_table)
  
  # returns the median outcome 
  return(median(all_invests))
}

# this function performs my proposed strategy 
invest_strat_c = function() {
  # load prices
  prices = prices_over_time(10000, 100)
  # we first use $500 to buy 5 coins and keep the rest in cash
  num_coins = 5
  cash = 500
  # keep net worths as an array and set day 1 to 1000 
  net_worth = numeric(10000)
  net_worth[1] = 1000
  # start on day 2
  n = 2
  while (n <= 10000) {
    # if the price is higher than yesterday
    if (prices[n] > prices[n-1]) {
      # sell 50% of bitcoins 
      cash = 0.5 * num_coins * prices[n] + cash
      num_coins = 0.5 * num_coins 
      net_worth[n] = cash + num_coins * prices[n]
    } else {
      # if the price is lower than yesterday 
      if (prices[n] < prices[n-1]) {
        # use 50% of cash to buy bitcoins 
        num_coins = 0.5 * cash / prices[n] + num_coins
        cash = 0.5 * cash
        net_worth[n] = cash + num_coins * prices[n]
      } else {
        net_worth[n] = cash + num_coins * prices[n]
      }
    }
    n = n + 1
  }
  #plot(net_worth, type="l")
  
  # return the difference between net worth and the worth of 10 bitcoins at the end
  return(net_worth[10000] - 10*prices[10000])
}

# a function that takes n as its input and repeats my investment strategy C n times
repeat_strat_c = function(n) {
  # store the final outcomes of investment (networth vs price of bitcoin)
  all_invests = replicate(n, invest_strat_c())
  
  all_invests_table = table(all_invests)
  
  barplot(all_invests_table)
  
  # returns the median outcome 
  return(median(all_invests))
}
