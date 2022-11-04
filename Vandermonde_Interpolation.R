vandermonde_method <- function(y, x, extrapolate){
  n <- length(x)
  V <- matrix(rep(0, n*n), nrow = n)
  res <- rep(0, n)
  for (d in seq(0, n-1)){
    for (i in seq(1, n)){
      V[i, d + 1] = x[i]^d
    }
    res[d + 1] <- extrapolate^d 
  }
  coeffs <- solve(V, y)
  return(sum(coeffs*res))
}

x <- c(2, 3, 4)
y <- c(1, 2, 3)
vandermonde_method(y, x, 10)
