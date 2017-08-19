prices_over_time = function(number_of_time_increments, initial_price) {
  return(cumprod(c(initial_price,exp(0.02*rnorm(number_of_time_increments)))));
}