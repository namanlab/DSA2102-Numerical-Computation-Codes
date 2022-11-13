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

x <- c(2, 3, 4)
y <- c(1, 2, 3)
newton_basis_method(y, x, 10)