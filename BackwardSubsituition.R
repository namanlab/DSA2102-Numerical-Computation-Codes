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
B <- matrix(rnorm(16),nrow = 4)
B[lower.tri(B)] <- 0
b <- rnorm(4)
solve(B,b)
back.sub(B, b)