#REDUCE Ax = b TO REF(A) (UPPER TRIANGULATR MAT) 
#SAME AS REDUCING A LOWER TRIANGULAR MAT TO DIAGONAL MAT AND FINDING SOLUTIONS

#Forward Substitution: To solve Lx = b, L is a lower triangular matrix
#Or to reduce Ax = b to upper triangular A (any general matrix A 
#being reduced to upper triangular matrix is same as a lower triangular 
#matrix being reduced to a diagonal matrix)
for.sub <- function(L, b){
  if (nrow(L) != ncol(L)) {return("Error: not square")}
  if (length(b) != ncol(L)) {return("Error: RHS wrong dim")}
  n = ncol(L)
  M <- cbind(L, b)
  for (j in 1:(n-1)){
    for (i in (j+1):n){
      mult <- M[i,j]/M[j,j]
      M[i,] = M[i,] - mult * M[j,]
    }
  }
  for (i in 1:n){
    M[i, n+1] = M[i, n+1]/M[i, i]
  }
  return(M[, n+1])
}
A = matrix(rnorm(16), 4)
A[upper.tri(A)] <- 0
b = rnorm(4)
solve(A, b) #Using in built R
for.sub(A, b) #Using our own function

# Avoiding swapping entries in L because the diagonal entries remain 
# same even after swapping: This one would only be relevant for lower
# triangular matrices
for.sub.cheaper <- function(L, b){
  n = ncol(L)
  for (j in 1:(n-1)){
    if (L[j,j] == 0) {return("Error: singular")}
    for (i in (j+1):n){
      mult <- L[i,j]/L[j,j]
      b[i] = b[i] - mult * b[j]
    }
  }
  for (i in 1:n){
    b[i] = b[i]/L[i, i]
  }
  return(b)
}

A = matrix(rnorm(16), 4)
A[upper.tri(A)] <- 0
b = rnorm(4)
solve(A, b)
for.sub.cheaper(A, b)