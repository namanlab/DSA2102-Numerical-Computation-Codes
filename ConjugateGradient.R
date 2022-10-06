conj_gradient <- function(A, b){
  n <- ncol(A)
  x <- matrix(rep(0, n))
  d <- b - (A %*% x)
  r <- d
  for (k in 1:n){
    if (identical(r, matrix(rep(0, n)))){
      return(x)
    }
    alpha <- (t(r) %*% r) / (t(d) %*% A %*% d)
    x <- x + alpha[1,1] * d
    r_prev <- r
    r <- r - alpha[1,1] * A %*% d
    beta <- (t(r) %*% r) / (t(r_prev) %*% r_prev)
    d <- r + (beta[1,1] *d)
  }
  return(x)
}

X = matrix(rnorm(16), 4)
X <- X %*% t(X)
b = matrix(rnorm(4))
conj_gradient(X, b)
solve(X, b)