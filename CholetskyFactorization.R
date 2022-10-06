cholesky_factorization <- function(A){
  n <- ncol(A)
  #Check if X is symmetric:
  if (!(t(A) == A)){
    return("Invalid Input: Matrix must be symmetric")
  } else {
    #Play with only half of the entries ti save time
    A[upper.tri(A)] = 0
    for (j in 1:(n-1)){ #Iterate through columns
      #Check for positive definite:
      if (A[j, j] == 0 | A[j, j] < 0){
        return("Matrix is not positive definite")
      } else {
        A[j, j] = sqrt(A[j, j])
        for (i in (j+1): n){
          A[i, j] = A[i, j]/A[j, j]
        }
        for (i in (j+1):n){ #Iterate through the (n-j) rows below each column j
          for (k in i:n){#Iterate through the (n-i+1) rows below each row
            A[k, i] = A[k, i] - A[k, j]*A[i, j]
          } 
        }
      }
    }
    A[n, n] <- sqrt(A[n, n])
    return(A)
  }
}

X = matrix(rnorm(16), 4)
X <- X %*% t(X)
L <- cholesky_factorization(X)
X
L %*% t(L)


#Solve via Choletsky:


cholesky_factorization <- function(A, b){
  n <- ncol(A)
  #Check if X is symmetric:
  if (!(t(A) == A)){
    return("Invalid Input: Matrix must be symmetric")
  } else {
    #Play with only half of the entries ti save time
    A[upper.tri(A)] = 0
    for (j in 1:(n-1)){ #Iterate through columns
      #Check for positive definite:
      if (A[j, j] == 0 | A[j, j] < 0){
        return("Matrix is not positive definite")
      } else {
        A[j, j] = sqrt(A[j, j])
        for (i in (j+1): n){
          A[i, j] = A[i, j]/A[j, j]
        }
        for (i in (j+1):n){ #Iterate through the (n-j) rows below each column j
          for (k in i:n){#Iterate through the (n-i+1) rows below each row
            A[k, i] = A[k, i] - A[k, j]*A[i, j]
          } 
        }
      }
    }
    A[n, n] <- sqrt(A[n, n])
    print(A %*% t(A))
    #Forward Substitution:
    res1 <- rep(0, n)
    res1[1] <- b[1]/A[1,1]
    for (i in 2:n){
      res1[i] <- b[i]
      for (j in 1:(i-1)){
        res1[i] <- res1[i] - A[i, j]*b[j]
      }
      res1[i] <- res1[i]/A[i, i]
    }
    #Backward substitution:
    M <- t(A)
    res2 <- rep(0, n)
    res2[n] <- res1[n]/M[n, n]
    for (i in (n-1):1){
      res2[i] <- res1[i]
      for (j in (i+1):n){
        res2[i] <- res2[i] - M[i, j]*res2[j]
      }
      res2[i] <- res2[i]/M[i, i]
    }
    return(res2)
  }
}

X = matrix(rnorm(16), 4)
X <- X %*% t(X)
b <- rnorm(4)
X
cholesky_factorization(X, b)
solve(X, b)

