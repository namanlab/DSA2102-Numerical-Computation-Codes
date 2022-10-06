# Uses Gram Schmidt Orthogonalisation
full.QR <- function(A){
  m = nrow(A)
  n = ncol(A)
  if (m < n){
    return("Need to have independent vectors")
  }
  if (m > n){
    for (i in 1:(m-n)){
      repb <- rep(0, m)
      repb[i] <- 1
      A <- cbind(A, repb)
    }
    #Update
    m = nrow(A)
    n = ncol(A)
  }
  Q <- matrix(rep(0, m*m), nrow = m)
  R <- matrix(rep(0, m*n), nrow = m)
  R[1,1] <- norm(A[,1], type = "2")
  Q[,1] <- A[,1]/R[1,1]
  for (i in 2:n){
    res <- A[,i]
    for (j in 1:(i-1)){
      mult <- (A[,i] %*% Q[,j])
      res <- res - (matrix(rep(mult, m))*Q[,j])
    }
    Q[,i] <- res/norm(res, type = "2")
  }
  #Calculate R:
  for (i in 1:n){ #Through Columns
    for (j in 1:i){
      R[j, i] <- A[,i] %*% Q[,j]
    }
  }
  print(Q)
  print(R)
  return(Q %*% R)
}

A <- matrix(rnorm(25), ncol = 5)
det(matrix(A, nrow = 5))
A
fin = full.QR(A)
fin
A

A <- matrix(rnorm(35), ncol = 5)
A
fin = full.QR(A)
fin
A