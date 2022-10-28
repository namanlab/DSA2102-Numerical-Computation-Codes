unshiftedQR <- function(A, num_iter){
  n <- nrow(A)
  Q <- diag(n)
  for (i in seq(1, num_iter)){
    res <- mod.partial.QR(A)
    Q <- res[[1]]
    print(Q)
    R <- res[[2]]
    print(R)
    A <- R %*% Q
  }
  print("Eigenvectors:")
  print(Q)
  print("Eigenvalues:")
  print(R)
}

A <- matrix(rnorm(9), nrow = 3)
A <- A + t(A) 
#Ensure non complex eignevectors before proceeding
eigen(A)
unshiftedQR(A, 10)

#CONVERTING TO UPPER HESSENBERG FORM TO IMPROVE CONVERGENCE RATE:

#To eliminate rows after ith entry of the jth column
hhfunc <- function(A, i, j){
  n <- ncol(A)
  e <- rep(0, n)
  e[i + 1] <- 1
  a <- c(rep(0, i), A[(i+1):n,j])
  print(a)
  v <- a + (norm(a, type = "2")*e)
  print(v)
  v <- v / norm(v, type = "2")
  H <- diag(n) - 2*(v %*% t(v))
  return(H)
}
A <- matrix(rnorm(25), nrow = 5)
round(hhfunc(A, 2, 2) %*% A  %*% hhfunc(A, 2, 2), 10)
round(hhfunc(A, 1, 1) %*% A  %*% hhfunc(A, 1, 1), 10)

#While preserving eigenvalues
reducetoUHF <- function(A){
  n <- ncol(A)
  A_ = A
  for (i in seq(1, n-2)){
    H <- hhfunc(A, i, i)
    A_ <- H %*% A %*% H
  }
  return(A_)
}
A <- matrix(rnorm(25), nrow = 5)
A_ <- round(reducetoUHF(A), 10)
eigen(A)$values
eigen(A_)$values


UHFunshiftedQR <- function(A, num_iter){
  n <- nrow(A)
  Q <- diag(n)
  A <- reducetoUHF(A)
  for (i in seq(1, num_iter)){
    res <- mod.partial.QR(A)
    Q <- res[[1]]
    print(Q)
    R <- res[[2]]
    print(R)
    A <- R %*% Q
  }
  print("Eigenvectors:")
  print(Q)
  print("Eigenvalues:")
  print(R)
}

A <- matrix(rnorm(9), nrow = 3)
A <- A + t(A) 
#Ensure non complex eignevectors before proceeding
eigen(A)
UHFunshiftedQR(A, 4) # Just about 4 iterations!