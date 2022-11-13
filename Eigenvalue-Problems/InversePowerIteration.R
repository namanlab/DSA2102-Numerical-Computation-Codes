inverse_power_iter <- function(A, guess, n){
  for (i in seq(1, n)){
    guess <- solve(A, guess)
    guess <- guess/norm(guess, type = "2")
  }
  #Rayleigh Coeff:
  lambda <- t(guess) %*% A %*% guess
  print("Eigenvector corresponding to smallest eigneval:")
  print(guess)
  print("Smallest Eigenvalue:")
  print(lambda)
}

A <- matrix(rnorm(9), nrow = 3)
#Ensure non complex eignevectors before proceeding
A <- A + T(A)
eigen(A)
inverse_power_iter(A, c(1, 2, 3), 100)