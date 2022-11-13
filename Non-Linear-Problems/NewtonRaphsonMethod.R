# Newton-Raphson Algorithm
newton_raph <- function(f, g, x, tol){
  y <- x + 1
  while (abs(x - y) >= tol){
    y <- x
    x <- x - (f(x)/g(x))
  }
  return(x)
}
f <- function(x){return(x^2 - 4*sin(x))}
g <- function(x){return(2*x - 4*cos(x))}
newton_raph(f, g, 0.5, 0.0000001)