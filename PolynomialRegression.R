poly_reg <- function(xa, y, d){
  n <- length(xa)
  A <- matrix(rep(1, n), nrow = n, ncol = 1)
  for (j in seq(1, d)){
    A <- cbind(A, xa^j)
  }
  print(A)
  optval = leastSq(A, y)
  return(optval)
}
x1 <- c(1,2,3,4)
y1 <- c(235, 265, 385, 485)
poly_reg(x1, y1, 2)
x2 <- c(1,2,3,4)
y2 <- c(235, 265, 385, 485) + (4.905*x1^2)
poly_reg(x2, y2, 1)