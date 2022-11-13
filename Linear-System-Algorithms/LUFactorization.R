LU_factor <- function(A){
  n = ncol(A)
  if (n != nrow(A)){
    return("No LU Factorization")
  } else {
    In <- diag(1, n)
    for (j in 1: (n-1)){
      for (i in (j+1):n){
        mult <- A[i, j]/A[j, j]
        A[i,] <- A[i, ] - mult*A[j, ]
        In[i, ] <- In[i, ] + mult*In[j, ]
      }
    }
    return(list(In, A)) #In is L and A is U
  }
}

C <- matrix(rnorm(16),nrow = 4)
LUList <- LU_factor(C)
L <- LUList[[1]]
U <- LUList[[2]]
L %*% U
C
