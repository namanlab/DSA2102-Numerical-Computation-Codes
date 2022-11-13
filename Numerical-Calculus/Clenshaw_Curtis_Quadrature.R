#Clenshaw Curtis Quadrature

gen_chebychev_nodes <- function(a, b, n){
  res <- rep(0, n + 1)
  # (a + b)/2 + (b - a)/2*cos((i +1/2)*pi/(n+1))
  c <- (a + b)/2
  d <- (b - a)/2
  for (k in seq(0, n)){
    res[k + 1] <- cos((k + 0.5)*pi/(n + 1))
  }
  res <- c + d*res
  return(res)
}

clenshaw_curtis <- function(f, a, b, n){
  points <- gen_chebychev_nodes(a, b, n - 1)
  mat <- matrix(rep(1, n*n), nrow = n)
  y <- rep(0, n)
  for (r in seq(1, n)){
    for (c in seq(1, n)){
      mat[r, c] <- res[c]^(r - 1)
    }
    y[r] <- (b^r - a^r)/r
  }
  weights <- solve(mat, y)
  return(sum(weights*f(res)))
}

f <- function(x){1/sqrt(2*pi)*exp(-x^2/2)}
clenshaw_curtis(f, 0.2, 0.8, 5)  
pnorm(0.8) - pnorm(0.2)
