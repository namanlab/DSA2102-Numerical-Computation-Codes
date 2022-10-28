# Regula Falsi/Method of False Positive
mfp_method <- function(f, a, b, tol){
  if (a > b){return("Error: a > b must")}
  m <- a + (b - a)/2
  while (abs(f(m)) >= tol){
    g <- (f(b) - f(a))/(b - a)
    m <- a - (f(a)/g)
    if (sign(f(a)) == sign(f(m))){a <- m}
    else {b <- m}
    print(m)
  }
  return(c(a, b))
}
f <- function(x){return(x^2 - 4*sin(x))}
g <- function(x){return(2*x - 4*cos(x))}
mfp_method(f, -1, 3, 0.0000001)