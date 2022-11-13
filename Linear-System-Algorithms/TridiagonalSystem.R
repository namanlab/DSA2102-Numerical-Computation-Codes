banded.solve <- function (A, b){
  n = ncol(A)
  res <- rep(0, n)
  #ELIMINATION
  for (i in 1:(n-1)) { #Repeat: n-1 times
    mult <- A[i+1,i]/A[i,i] #1 flop
    for (j in i:(i+1)){ #Repeat 2 times
      A[i+1, j] <- A[i+1, j] - A[i, j]*mult #2 flops
    }
    b[i+1] <- b[i+1] - b[i]*mult #2 flops
  }
  #Total Ops: (n-1)*(1 + 2*2 + 2) = 7*(n-1)
  #BACK SUBSTITUITION:
  res[n] <- b[n]/A[n, n] # 1 flop
  for (i in (n-1):1){ #Repeat: n-1 times
    res[i] <- (b[i] - A[i, i+1]*res[i+1])/A[i, i] #3 flops
  }
  #Total Ops: 1 + 3*(n-1)
  return(res)
}

A <- matrix(c(1,3,0,0, 2,4,3,0, 0,5,9,4, 0,0,6,1), nrow = 4, byrow = T)
b <- rnorm(4)
A
banded.solve(A, b)
solve(A, b)
