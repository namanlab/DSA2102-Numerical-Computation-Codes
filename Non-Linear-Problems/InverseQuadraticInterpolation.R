inverse_quadratic_interpolation <- function(f, g1, tol){
  g2 <- g1 + 1
  g3 <- g2 + 1
  while (abs((g3 - g2)) > tol){
    # Fit inverse quadratic polynomial
    # Lagrange interpolation
    t1 <- g1*(f(g2)*f(g3))/((f(g1) - f(g2))*(f(g1) - f(g3)))
    t2 <- g2*(f(g1)*f(g3))/((f(g2) - f(g1))*(f(g2) - f(g3)))
    t3 <- g3*(f(g2)*f(g1))/((f(g3) - f(g2))*(f(g3) - f(g1)))
    g1 <- g2
    g2 <- g3
    g3 <- t1 + t2 + t3
    print(g1)
    print(g2)
    print(g3)
  }
}

f <- function(x){return(x^2 - 5*sin(x))}
inverse_quadratic_interpolation(f, 1.5, 0.00001)
