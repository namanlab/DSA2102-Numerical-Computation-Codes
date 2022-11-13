#Simultaneous Rayleigh Iteration:
SRQI <- function(A, ni){
  n <- ncol(A)
  eigenvals <- rep(0, n)
  I <- diag(n)
  lambda <- matrix(0)
  for (i in seq(1, n)){
    guess <- I[i,]
    for (j in 1:ni){
      lambda <- t(guess) %*% A %*% guess
      guess <- solve(A - (lambda[1,1] * diag(n)), guess)
      guess <- guess/norm(guess, type = "2")
    }
    eigenvals[i] <- lambda[1,1]
  }
  print(eigenvals)
}

A <- matrix(rnorm(9), nrow = 3)
#Ensure non complex eignevectors before proceeding
A <- A + t(A)
eigen(A)
SRQI(A, 3)