# Returns n + 1 chebychev nodes
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

a <- -10
b <- 10
gen_chebychev_nodes(a, b, 20)
a <- 0
b <- 10
gen_chebychev_nodes(a, b, 10)

# h is size of interval around extrapolating value, where we intrapolate
natural_cubic_spline_chebychev <- function(f, extrapolate, h, n){
  a <- extrapolate - h
  b <- extrapolate + h
  x <- gen_chebychev_nodes(a, b, n)
  x <- rev(x)
  y <- f(x)
  n <- length(x)
  
  # Creating A and b
  A <- matrix(rep(0, 16*(n - 1)^2), nrow = 4*(n - 1))
  b <- rep(0, 4*(n-1))
  
  # First 2(n-1) rows: Interpolate
  A[1, c(1,2,3,4)] <- c(1, x[1], x[1]^2, x[1]^3)
  b[1] <- y[1]
  colUpdater <- 1
  rowUpdater <- 2
  for (i in seq(2, n - 1)){
    A[rowUpdater, c(colUpdater, colUpdater + 1, colUpdater + 2, colUpdater + 3)] <-
      c(1, x[i],x[i]^2, x[i]^3)
    b[rowUpdater] <- y[i]
    colUpdater <- colUpdater + 4
    rowUpdater <- rowUpdater + 1
    A[rowUpdater, c(colUpdater, colUpdater + 1, colUpdater + 2, colUpdater + 3)] <-
      c(1, x[i],x[i]^2, x[i]^3)
    b[rowUpdater] <- y[i]
    rowUpdater <- rowUpdater + 1
  }
  A[2*(n-1), c(4*(n-1) - 3,4*(n - 1) - 2,4*(n-1) - 1,4*(n-1))] <- c(1, x[n], x[n]^2, x[n]^3)
  b[2*(n-1)] <- y[n]
  
  # Next (n - 2) rows: First derivative equality
  colUpdater <- 1
  rowUpdater <- rowUpdater + 1
  for (i in seq(2, n - 1)){
    A[rowUpdater, c(colUpdater, colUpdater + 1, colUpdater + 2, colUpdater + 3)] <-
      c(0, 1, 2*x[i], 3*x[i]^2)
    A[rowUpdater, c(colUpdater + 4, colUpdater + 5, colUpdater + 6, colUpdater + 7)] <-
      c(0, -1,-2*x[i], -3*x[i]^2)
    colUpdater <- colUpdater + 4
    rowUpdater <- rowUpdater + 1
  }
  
  # Next (n - 2) rows: Second derivative equality
  colUpdater <- 1
  for (i in seq(2, n - 1)){
    A[rowUpdater, c(colUpdater, colUpdater + 1, colUpdater + 2, colUpdater + 3)] <-
      c(0, 0, 2, 6*x[i])
    A[rowUpdater, c(colUpdater + 4, colUpdater + 5, colUpdater + 6, colUpdater + 7)] <-
      c(0, 0,-2, -6*x[i])
    colUpdater <- colUpdater + 4
    rowUpdater <- rowUpdater + 1
  }
  
  # Last 2 rows: Boundary conditions
  A[rowUpdater, c(1, 2, 3, 4)] <- c(0, 1, 2*x[1], 3*x[1]^2)
  rowUpdater <- rowUpdater + 1
  A[rowUpdater, c(4*(n-1) - 3,4*(n - 1) - 2,4*(n-1) - 1,4*(n-1))] <- c(0, 1, 2*x[n], 3*x[n]^2)
  
  res <- solve(A, b) # 4 coeffs per spline
  print(res)
  indexPoly <- 0
  for (i in seq(1, n)){
    if (extrapolate < x[i]){
      indexPoly <- i - 2
      break
    }
  }
  calc <- res[4*indexPoly + 1] + res[4*indexPoly + 2]*(extrapolate) + res[4*indexPoly + 3]*(extrapolate^2) + res[4*indexPoly + 4]*(extrapolate^3)
  return(res)
}

f <- function(x){sin(x)}
n <- 7
h <- 2
extrapolate <- 1
r <- natural_cubic_spline_chebychev(f, 1, 2, n) #Evaluates functions via polynomial approximation of degree n
sin(1)

