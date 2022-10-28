fixedPointIter <- function(f, x, tol){
  y <- x + 1 #Fake first iter
  while (abs(y - x) > tol){
    y <- x
    x <- f(x)
    print(x)
  }
  return(x)
}

#Can give Error depending on initialization & function if NaN produced somewhere
f <- function(x){return(x^2 - x)}
fixedPointIter(f, 1, 0.001)

f <- function(x){return(x^2 - 0.24)}
fixedPointIter(f, 1, 0.00001)

f <- function(x){return(x^2 - 2 + x)} #No convergence :(
fixedPointIter(f, 1.4, 0.01)