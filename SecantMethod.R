# Secant method
secant_method <- function(f, x, tol){
  y <- x + 1 #Maintaing 2 guesses at once
  while (abs(x - y) >= tol){
    g <- (f(y) - f(x))/(y - x)
    y <- x
    x <- x - (f(x)/g)
  }
  return(x)
}
f <- function(x){return(x^2 - 4*sin(x))}
g <- function(x){return(2*x - 4*cos(x))}
secant_method(f, 2, 0.0000001)