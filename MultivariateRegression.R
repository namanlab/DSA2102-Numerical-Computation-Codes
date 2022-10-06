mult_reg <- function(y, ...){
  args <- list(...)
  n <- length(y)
  d <- length(args) + 1
  A <- matrix(rep(1, n), nrow = n, ncol = 1)
  for (j in args){
    A <- cbind(A, j)
  }
  print(A)
  optval = leastSq(A, y)
  return(optval)
}
x1 <- c(0,0,1,1,1)
y1 <- c(0,1,0,1,2)
z1 <- c(3,2,3,5,6)
mult_reg(z1, x1, y1)