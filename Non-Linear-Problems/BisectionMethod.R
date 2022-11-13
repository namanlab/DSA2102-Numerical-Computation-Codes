bisectionMethod <- function(f, a, b, tol){
  if (a > b){return("Error: a > b must")}
  else {
    while (b - a >= tol){
      m <- a + (b-a)*0.5
      if (sign(f(a)) == sign(f(m))){a <- m}
      else {b <- m}
      vec <- c(a, b)
    }
    return(c(a, b))
  }
}

f <- function(x){return(x^2 - 4*sin(x))}
bisectionMethod(f, 1, 3, 0.0000001)

f <- function(x){return(x^2 - 0.24)}
bisectionMethod(f, 1, 3, 0.0000001)