# Riemann Sums
riemann_sum <- function(f, a, b, h){
  st <- a
  delta <- (b - a)/h
  nxt <- a + delta
  res <- 0
  while (nxt <= b){
    mid <- (st + nxt)/2
    res <- res + (f(mid)*delta)
    st <- st + delta
    nxt <- nxt + delta
  }
  return(res)
}

f <- function(x){1/sqrt(2*pi)*exp(-x^2/2)}
riemann_sum(f, 0.2, 0.8, 10000)  
pnorm(0.8) - pnorm(0.2)