lagrange_method <- function(y, x, extrapolate){
  res <- 0
  n <- length(x)
  for (i in seq(1, n)){
    prod <- 1
    for (j in seq(1, n)){
      if (i != j) {
        num <- (extrapolate - x[j])
        denom <- (x[i] - x[j])
        term <- num/denom
        prod <- prod*term
      }
    }
    res <- res + y[i]*prod
  }
  return(res)
}


x <- c(2, 3, 4)
y <- c(1, 2, 3)
lagrange_method(y, x, 10)
