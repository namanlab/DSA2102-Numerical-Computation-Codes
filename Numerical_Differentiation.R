# For comparing against computation by R
library(numDeriv)


twoPt_forward_first_derivative <- function(f, x, h){
  (f(x + h) - f(x))/h
}

threePt_central_first_derivative <- function(f, x, h){
  (f(x + h) - f(x - h))/(2*h)
}

fivePt_central_first_derivative <- function(f, x, h){
  (f(x - 2*h) - 8*f(x - h) + 8*f(x + h) - f(x + 2*h))/(12*h)
}

f <- function(x){sin(x) - 2*x + cos(x^3)}
grad(f, 2)
twoPt_forward_first_derivative(f, 2, 0.001)
threePt_central_first_derivative(f, 2, 0.001)
fivePt_central_first_derivative(f, 2, 0.001)

threePt_central_second_derivative <- function(f, x, h){
  (f(x + h) - 2*f(x) + f(x - h))/(h^2)
}

fivePt_central_second_derivative <- function(f, x, h){
  (-f(x - 2*h) + 16*f(x - h) - 30*f(x) + 16*f(x + h) - f(x + 2*h))/(12*h^2)
}

f <- function(x){sin(x) - 2*x + cos(x^3)}
g <- function(x){cos(x) - 2 - 3*x^2*sin(x^3)}
grad(g, 3)
threePt_central_second_derivative(f, 3, 0.001)
fivePt_central_second_derivative(f, 3, 0.001)


