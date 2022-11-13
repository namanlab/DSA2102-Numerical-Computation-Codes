# Function for backward substituition
back.sub <- function(U,b){
  n = ncol(U)
  for (j in n:2){
    if (U[j,j] == 0) {return("Error: singular")}
    for (i in 1:(j-1)){
      mult <- U[i,j]/U[j,j]
      b[i] = b[i] - mult * b[j]
    }
  }
  for (i in 1:n){
    b[i] = b[i]/U[i, i]
  }
  return(b)
}


#Gaussian Elimination
my.solve <- function(A,b){
  #First we perform Gaussian Elimination as earlier to generate an 
  #upper triangular matrix.
  n <- nrow(A)
  M <- cbind(A,b)
  for(j in 1:(n-1)){
    for(i in (j+1):n){
      mult <- M[i,j]/M[j,j]
      M[i,] = M[i,] - mult * M[j,]
    }
  }
  A1 <- M[,1:n]
  b1 <- M[,n+1]
  #Next we apply back substitution on REF (the upper triangular form)
  return(back.sub(A1,b1))
}

C <- matrix(rnorm(16),nrow = 4)
b <- rnorm(4)
solve(C,b)
my.solve(C, b)
