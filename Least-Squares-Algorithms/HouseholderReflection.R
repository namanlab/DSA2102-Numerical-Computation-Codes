basisvec <- function(i, n){
  vec <- rep(0, n)
  vec[i] <- 1
  return(vec)
}
hhQR <- function(X){
  A = X
  n = nrow(A)
  c = ncol(A)
  Q = diag(n) #OG Product of H1, H2, ..
  R = A # Hk...H2H1A
  for (i in seq(1, c)){
    x <- A[,i]
    if (i > 1) {for (k in seq(1, i - 1)){x[k] <- 0}}
    if (x[1] > 0){v <- x + norm(x, type = "2")*basisvec(i, n)}
    else {v <- x - norm(x, type = "2")*basisvec(i, n)}
    nv <- norm(v, type = "2")
    v <- v/nv
    H <- diag(n) - 2*(v %*% t(v))
    Q <- Q %*% H
    R <- H %*% R
    A <- R
  }
  print("Q:")
  print(Q)
  print("R:")
  print(R)
  return(Q %*% R)
}


A <- matrix(10*rnorm(25), ncol = 5)
det(matrix(A, nrow = 5))
A
fin = hhQR(A)
fin
A

A <- matrix(rnorm(35), ncol = 5)
A
fin = hhQR(A)
fin
A

A <- matrix(c(1,1,1,1,1,2,3,4,1,4,9,16), nrow = 4)
A
fin = hhQR(A)
fin
A
