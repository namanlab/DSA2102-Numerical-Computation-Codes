leastSq <- function(A, b){
  xopt = solve(t(A) %*% A, t(A) %*% b)
  er = norm(A %*% xopt - b)
  return(xopt)
}
A1 = matrix(c(1,0,2,2,1,1), nrow = 3)
b1 = c(3, 1, 1)
A2 = matrix(c(1,1,1,2,0,0,1,1,1,2,1,1), nrow = 4)
b2 = c(2,3,1,2)
A3 = matrix(c(1,1,1), nrow = 3)
b3 = c(1,5,6)
leastSq(A1, b1)
leastSq(A2, b2)
leastSq(A3, b3)