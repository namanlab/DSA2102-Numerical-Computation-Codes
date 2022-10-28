power_iter <- function(A, guess, n){
  for (i in seq(1, n)){
    guess <- A %*% guess
    lambda <- norm(guess, type = "2")
    guess <- guess/lambda
  }
  print("Dominant Eigenvector:")
  print(guess)
  print("Dominant Eigenvalue:")
  print(lambda)
}

A <- matrix(rnorm(9), nrow = 3)
#Ensure non complex eignevectors before proceeding
A <- A + t(A)
eigen(A)
power_iter(A, c(1, 2, 3), 100)


# With Rayleigh Coeff
power_iter <- function(A, guess, n){
  for (i in seq(1, n)){
    guess <- A %*% guess
    guess <- guess/norm(guess, type = "2")
  }
  #Rayleigh Coeff:
  lambda <- t(guess) %*% A %*% guess
  print("Dominant Eigenvector:")
  print(guess)
  print("Dominant Eigenvalue:")
  print(lambda)
}

A <- matrix(rnorm(9), nrow = 3)
#Ensure non complex eignevectors before proceeding
A <- A + t(A)
eigen(A)
power_iter(A, c(1, 2, 3), 100)