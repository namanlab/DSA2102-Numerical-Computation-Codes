# Gaussian Quadratures (Gauss-Legendre Quadratures)

gaussian_quad_deg2 <- function(f, a, b){
  w1 <- 1
  w2 <- 1
  x1 <- -1/sqrt(3)
  x2 <- 1/sqrt(3)
  transformed_f <- function(x){f((x*(b - a) + a + b)/2)} # As gaussian quads evaluate -1 to 1
  res <- w1*transformed_f(x1) + w2*transformed_f(x2)
  res <- (b - a)/2*res
  return(res)
}

f <- function(x){1/sqrt(2*pi)*exp(-x^2/2)}
gaussian_quad_deg2(f, 0.2, 0.8)  
pnorm(0.8) - pnorm(0.2)

gaussian_quad_deg3 <- function(f, a, b){
  w1 <- 8/9
  w2 <- 5/9
  w3 <- 5/9
  x1 <- 0
  x2 <- sqrt(3/5)
  x3 <- -sqrt(3/5)
  transformed_f <- function(x){f((x*(b - a) + a + b)/2)} # As gaussian quads evaluate -1 to 1
  res <- w1*transformed_f(x1) + w2*transformed_f(x2) + w3*transformed_f(x3)
  res <- (b - a)/2*res
  return(res)
}

f <- function(x){1/sqrt(2*pi)*exp(-x^2/2)}
gaussian_quad_deg3(f, 0.2, 0.8)  
pnorm(0.8) - pnorm(0.2)