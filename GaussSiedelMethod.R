gauss.siedel <- function(A, b, epsilon){
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
  x1 <- matrix(rep(0, n))
  for (i in 1:n){
    x1[i] = Dinv[i, ] %*% (b - (U %*% x0) - (L %*% x1))
  }
  while (sum(abs(x1) - abs(x0))  < epsilon){
    x0 = x1
    for (i in 1:n){
      x1[i] = Dinv[i, ] %*% (b - (U %*% x0) - (L %*% x1))
    }
  }
  return(x1)
}
A = matrix(rnorm(9), 3)
A[1,1] = 100
A[2,2] = 300
A[3,3] = 400
b = matrix(rnorm(3))
gauss.siedel(A, b, 0.000000001)
solve(A, b)