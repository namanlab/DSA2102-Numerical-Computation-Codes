# Newton Coates (n point) Rule
newton_coates_rule <- function(f, a, b, n){
  res <- rep(a, n)
  delta <- (b - a)/(n - 1)
  for (i in seq(1, n)){
    res[i] <- a + (i - 1)*delta
  }
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
newton_coates_rule(f, 0.2, 0.8, 10)  
pnorm(0.8) - pnorm(0.2)

# Composite newton coates Rule
composite_newton_coates_rule <- function(f, a, b, n, panels){
  st <- a
  delta <- (b - a)/panels
  nxt <- a + delta
  res <- 0
  while (nxt <= b){
    res <- res + newton_coates_rule(f, st, nxt, n)
    st <- st + delta
    nxt <- nxt + delta
  }
  return(res)
}

f <- function(x){1/sqrt(2*pi)*exp(-x^2/2)}
composite_newton_coates_rule(f, 0.2, 0.8, 10, 4)  
pnorm(0.8) - pnorm(0.2)