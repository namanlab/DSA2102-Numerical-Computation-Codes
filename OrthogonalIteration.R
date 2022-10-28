# Normalized Simulateneous Iteration/Orthogonal Iteration

#Using built in QR
NSI <- function(A, num_iter){
  n <- nrow(A)
  X <- diag(n)
  for (i in seq(1, num_iter)){
    X <- A %*% (qr.Q(qr(X)))
  }
  print("Eigenvectors:")
  print((qr.Q(qr(X))))
  print("Eigenvalues:")
  print((qr.R(qr(X))))
}

A <- matrix(rnorm(9), nrow = 3)
#Ensure non complex eignevectors before proceeding
A <- A + t(A)
eigen(A)
NSI(A, 10)

# QR Factorization via Gram Schmidt
#PARTIAL QR:
mod.partial.QR <- function(A){
  m = nrow(A)
  n = ncol(A)
  if (m < n){
    return("Need to have independent vectors")
  }
  Q <- matrix(rep(0, m*n), nrow = m)
  R <- matrix(rep(0, n*n), nrow = n)
  R[1,1] <- norm(A[,1], type = "2")
  Q[,1] <- A[,1]/R[1,1]
  for (i in 2:n){
    res <- A[,i]
    for (j in 1:(i-1)){
      mult <- (res %*% Q[,j]) #Only replaced A[,i] with res
      res <- res - (mult*Q[,j])
    }
    Q[,i] <- res/norm(res, type = "2")
  }
  #Calculate R:
  for (i in 1:n){ #Through Columns
    for (j in 1:i){
      R[j, i] <- A[,i] %*% Q[,j]
    }
  }
  return(list(Q, R))
}

NSI <- function(A, num_iter){
  n <- nrow(A)
  Q <- diag(n)
  for (i in seq(1, num_iter)){
    X <- A %*% Q
    res <- mod.partial.QR(X)
    Q <- res[[1]]
    R <- res[[2]]
  }
  print("Eigenvectors:")
  print(Q)
  print("Eigenvalues:")
  print(R)
}


A <- matrix(rnorm(9), nrow = 3)
#Ensure non complex eignevectors before proceeding
A <- A + t(A)
eigen(A)
NSI(A, 10)
