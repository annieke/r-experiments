# this function takes an integer input n for which dimension we want to test
find_in_n_dimension = function(n) {
  # we generate n different values between -1 and 1
  values = runif(n, min = -1, max = 1)
  
  # we test if the sum of their squares is within our boundaries
  if (sum(values^2) <= 1) {
    # return 1 if they are
    return(1)
  }
  else {
    # return 0 if not 
    return(0)
  }
}

# this function takes a dimention n & a number reps, and calculates the ratio
# of points in our boundary in this nth dimension
est_area_for_n = function(n, reps) {
  # we start on the first rep, without any points selected
  r = 1
  result = 0
  # we run through each rep and update the ratio accordingly
  while (r <= reps) {
    # add the newest result to result
    result = result + find_in_n_dimension(n)
    # print the current ratio every 1 million times
    if (r %% 1000000 == 0) {
      print(result * 2^n /r)
    }
    # update trial count 
    r = r + 1
  }
  # return the final ratio after the specified number of reps 
  return(result * 2^n /r)
}

# a function that simplifies finding the volumes for dimensions 4 through 10
est_for_4_to_10 = function() {
  results = numeric(7)
  # i for storing results in index i, n for actual dimension 
  i = 1
  n = 4 
  while (n <= 10) {
    # calculate for n and store 
    results[i] = est_area_for_n(n, 10000000)
    print(results[i])
    i = i + 1
    n = n + 1
  }
  return(results)
}
