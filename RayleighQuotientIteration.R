#Rayleigh Quotient Iteration (RQI)
RQI <- function(A, guess, ni){
  n <- ncol(A)
  lambda <- matrix(0)
  for (i in seq(1, ni)){
    guess <- solve(A - (lambda[1,1]*diag(n)), guess)
    guess <- guess/norm(guess, type = "2")
    lambda <- t(guess) %*% A %*% guess
  }
  print("Eigenvector corresponding to smallest eigneval:")
  print(guess)
  print("Smallest Eigenvalue:")
  print(lambda)
}

A <- matrix(rnorm(9), nrow = 3)
#Ensure non complex eignevectors before proceeding
A <- A + t(A)
eigen(A)
RQI(A, c(1, 2, 3), 4)


#Rayleigh Quotient Iteration (RQI) Shifted
shifted_RQI <- function(A, guess, ni, s){
  n <- ncol(A)
  A_ <- A - (s*diag(n))
  lambda <- matrix(0)
  for (i in seq(1, ni)){
    guess <- solve(A_ - (lambda[1,1]*diag(n)), guess)
    guess <- guess/norm(guess, type = "2")
    lambda <- t(guess) %*% A %*% guess
  }
  print("Eigenvector corresponding to smallest eigneval:")
  print(guess)
  print("Smallest Eigenvalue:")
  print(lambda)
}

A <- matrix(rnorm(9), nrow = 3)
#Ensure non complex eignevectors before proceeding
A <- A + t(A)
eigen(A)
shifted_RQI(A, c(1, 2, 3), 10, 1.45)