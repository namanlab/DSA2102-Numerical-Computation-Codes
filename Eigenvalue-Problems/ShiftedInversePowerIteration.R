shifted_inverse_power_iter <- function(A, guess, ni, s){
  n <- ncol(A)
  Ainv <- solve(A - s*diag(n))
  for (i in seq(1, ni)){
    guess <- Ainv %*% guess
    guess <- guess/norm(guess, type = "2")
  }
  #Rayleigh Coeff:
  lambda <- t(guess) %*% A %*% guess
  lambda <- lambda
  print("Eigenvector:")
  print(guess)
  print("Eigenvalue:")
  print(lambda)
}

A <- matrix(rnorm(9), nrow = 3)
#Ensure non complex eignevectors before proceeding
A <- A + t(A)
eigen(A)
shifted_inverse_power_iter(A, c(1, 2, 3), 100, -0.12)