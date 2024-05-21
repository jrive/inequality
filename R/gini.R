gini <- function (X, weights = rep(1, length = length(X))){
  # this function borrowed from reldist
  id <- order(X)
  X <- X[id]
  weights <- weights[id]/sum(weights)
  p <- cumsum(weights)
  nu <- cumsum(weights*X)
  n <- length(nu)
  nu <- nu/nu[n]
  sum(nu[-1] * p[-n]) - sum(nu[-n] * p[-1])
}