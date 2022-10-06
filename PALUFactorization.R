PLU_factor <- function(A){
  n = ncol(A)
  if (n != nrow(A)){
    return("No LU Factorization")
  } else {
    In <- diag(1, n)
    Pn <- diag(1, n)
    for (j in 1: (n-1)){
      #Pivoting
      res = A[j, j]
      for (i in (j+1):n){
        if (res <= A[i, j] ){
          #Swap for A
          r = A[i,] 
          A[i,] = A[j,]
          A[j,] = r
          res = A[i, j]
          #Swap for I
          r = Pn[i,] 
          Pn[i,] = Pn[j,]
          Pn[j,] = r
        }
      }
      for (i in (j+1):n){
        mult <- A[i, j]/A[j, j]
        A[i,] <- A[i, ] - mult*A[j, ]
        In[i, ] <- In[i, ] + mult*In[j, ]
      }
    }
    return(list(Pn, In, A)) #Pn is P, In is L and A is U
  }
}

C <- matrix(rnorm(16),nrow = 4)
PLUList <- PLU_factor(C)
P <- PLUList[[1]]
L <- PLUList[[2]]
U <- PLUList[[3]]
L %*% U
P %*% C