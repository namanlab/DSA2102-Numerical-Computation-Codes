newton_basis_method <- function(y, x, extrapolate){
  n <- length(x)
  N <- matrix(rep(0, n*n), nrow = n)
  res <- rep(0, n)
  for (i in seq(1, n)){ # Rows
    term_res <- 1
    for (j in seq(1, i)){ # Cols
      term <- 1
      if (j > 1){
        term_res <- term_res*(extrapolate - x[j - 1])
        for (k in seq(1, j - 1)){
          term <- term*(x[i] - x[k])
        }
      }
      N[i, j] = term
    }
    res[i] <- term_res 
  }
  coeffs <- solve(N, y)
  return(sum(coeffs*res))
}

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
chebychev_interpol <- function(f, extrapolate, h, n){
  a <- extrapolate - h
  b <- extrapolate + h
  x <- gen_chebychev_nodes(a, b, n)
  y <- f(x)
  res <- newton_basis_method(y, x, extrapolate)
  return(res)
}

f <- function(x){sin(x)}
n <- 7
chebychev_interpol(f, 1, 2, n) #Evaluates functions via polynomial approximation of degree n
sin(1)