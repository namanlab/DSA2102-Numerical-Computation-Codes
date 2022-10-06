jacobi <- function(A, b, epsilon){
  n <- ncol(A)
  L <- A
  L[upper.tri(L)] <- 0
  U <- A
  U[lower.tri(U)] <- 0
  Dinv <- diag(1/diag(A))
  
  #Test for Strict Diagonal Dominance:
  for (i in 1:n){
    if (2 * abs((A[i, i])) <= rowSums(abs(A))[i]){
      print("Convergence not guaranteed")
    }
  }
  
  x0 <- matrix(rep(0, n))
  x1 <- Dinv %*% (b - ((L + U) %*% x0))
  while (sum(abs(x1) - abs(x0))  < epsilon){
    x0 = x1
    x1 = Dinv %*% (b - ((L + U) %*% x0))
  }
  return(x1)
}
A = matrix(rnorm(9), 3)
A[1,1] = 100
A[2,2] = 300
A[3,3] = 400
b = matrix(rnorm(3))
jacobi(A, b, 0.000001)
solve(A, b)


#Sparse Matrix:
library(Matrix)

i <- c(1,5,2,4,2,2,8,2)
j <- c(2,5,3,2,4,2,8,4)
x <- rpois(9,2) #sample random integers from poisson
A <- sparseMatrix(i,j,x=x)
A
b = matrix(rnorm(8))
jacobi(A, b, 0.000001)
solve(A, b)