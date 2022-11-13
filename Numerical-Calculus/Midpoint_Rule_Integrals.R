# Midpoint Rule
midpt_rule <- function(f, a, b){
  h <- (b - a)
  m <- (a + b)/2
  res <- h*f(m)
  return(res)
}

f <- function(x){1/sqrt(2*pi)*exp(-x^2/2)}
midpt_rule(f, 0.2, 0.8)  
pnorm(0.8) - pnorm(0.2)

# Composite Midpt Rule
composite_midpt_rule <- function(f, a, b, panels){
  st <- a
  delta <- (b - a)/panels
  nxt <- a + delta
  res <- 0
  while (nxt <= b){
    res <- res + midpt_rule(f, st, nxt)
    st <- st + delta
    nxt <- nxt + delta
  }
  return(res)
}

f <- function(x){1/sqrt(2*pi)*exp(-x^2/2)}
composite_midpt_rule(f, 0.2, 0.8, 4)  
pnorm(0.8) - pnorm(0.2)