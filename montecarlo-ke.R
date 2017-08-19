find_in_n_dimension = function(n) {
  values = runif(n, min = -1, max = 1)
  if (sum(values^2) <= 1) {
    return(1)
  }
  else {
    return(0)
  }
}

est_area_for_n = function(n, reps) {
  ratio = sum(replicate(reps, find_in_n_dimension(n))) / reps
  bound_area = 2^n
  return(ratio * bound_area)
}

