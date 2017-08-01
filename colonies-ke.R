# This function takes inputs n for remaining number in population and
# p for rate of survival, returning the change in population after 1 minute
pop_change_for_n_orgs = function(n, p) {
  # we return -1 for the organisms that die, and 1 for the organisms that
  # split off, returning 1 with p probability
  return(sum(sample(c(1, -1), size=n, replace=TRUE, prob=c(p, 1-p))))
}

# This function takes input p for rate of survival and returns 1 for a surviving
# population and 0 for one that went to 0
pop_over_time = function(p) {
  # we start with one organism
  pop = 1
  # and at time 0
  t = 0
  
  # keep testing how population changes until everything is dead or 20 minutes
  # has passed (to prevent infinite loops)
  while (pop > 0 & t < 20) {
    # time increases by 1 min
    t = t + 1
    # population changes based on current population and p
    pop = pop + pop_change_for_n_orgs(pop, p)
  }
  
  # return 0 for a dead population and 1 for a surviving one
  if (pop == 0) {
    return(0)
  } else {
    return(1)
  }
}

# This function takes an input N for the number of simulations to run for each 
# value of p 
sim_pop_over_p = function(N) {
  # we start at a survival rate of 0
  p = 0
  # and keep a list of rates
  rates = numeric(100)
  
  # while p isn't 1 yet
  while (p < 1) {
    # we find the rate of survival by summing over the results of the last 
    # function and dividing it by the number of simulations 
    rates[p*100+1] = sum(replicate(N, pop_over_time(p))) / N
    # go on to next probability
    p = p + 0.01
  }
  
  # plot and return the rates
  barplot(rates)
  return(rates)
}
