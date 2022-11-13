# Trapezoid Rule
trapezoid_rule <- function(f, a, b){
  h <- (b - a)/2
  res <- h*(f(a) + f(b))
  return(res)
}

f <- function(x){1/sqrt(2*pi)*exp(-x^2/2)}
trapezoid_rule(f, 0.2, 0.8)  
pnorm(0.8) - pnorm(0.2)

# Composite trapezoid Rule
composite_trapezoid_rule <- function(f, a, b, panels){
  st <- a
  delta <- (b - a)/panels
  nxt <- a + delta
  res <- 0
  while (nxt <= b){
    res <- res + trapezoid_rule(f, st, nxt)
    st <- st + delta
    nxt <- nxt + delta
  }
  return(res)
}

f <- function(x){1/sqrt(2*pi)*exp(-x^2/2)}
composite_trapezoid_rule(f, 0.2, 0.8, 4)  
pnorm(0.8) - pnorm(0.2)
