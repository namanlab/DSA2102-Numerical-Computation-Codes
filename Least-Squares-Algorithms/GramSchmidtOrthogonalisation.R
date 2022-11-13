gram.schmidt <- function(v){
  k = length(v)
  n = length(v[[1]])
  fin <- vector(mode='list', length=k)
  fin[[1]] <- v[[1]]/norm(matrix(v[[1]]), type = "2")
  for (i in 2:k){
    res <- v[[i]]
    for (j in 1:(i-1)){
      mult <- (fin[[j]] %*% v[[i]])[1,1]
      res <- res - (mult*fin[[j]])
    }
    fin[[i]] <- res/norm(matrix(v[[i]]), type = "2")
  }
  return(fin)
}
v1 <- c(4, 0, 3)
v2 <- c(8,2,6)
v3 <- c(1,-2,7)
A <- list(v1, v2, v3)
det(matrix(unlist(A), nrow = 3))
matrix(unlist(A), nrow = 3)
fin = gram.schmidt(A)

v1 <- rnorm(5)*10
v2 <- rnorm(5)*10
v3 <- rnorm(5)*10
v4 <- rnorm(5)*10
v5 <- rnorm(5)*10
A <- list(v1, v2, v3, v4, v5)
det(matrix(unlist(A), nrow = 5))
matrix(unlist(A), nrow = 5)
fin = gram.schmidt(A)
matrix(unlist(fin), nrow = 5)
fin[[1]] %*% fin[[2]]

v1 <- rnorm(6)*10
v2 <- rnorm(6)*10
v3 <- rnorm(6)*10
v4 <- rnorm(6)*10
v5 <- rnorm(6)*10
A <- list(v1, v2, v3, v4, v5)
matrix(unlist(A), nrow = 5)
fin = gram.schmidt(A)
matrix(unlist(fin), nrow = 5)
fin[[1]] %*% fin[[2]]
