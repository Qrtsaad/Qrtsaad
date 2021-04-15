#' Optimal Partitioning
#'
#'
#' @description
#'
#' @param data vector of data points
#' @param cout a number
#' @param beta a number
#'
#' @return a vector of changepoints, a number for the complexity (cost of computations)
#' @export
#'
#' @examples
myOP <- function(data, cout = cost_gauss, beta = best_beta(data)) {
  n <- length(data)
  t <- 1
  cpx <- 0

  cp <- rep(0,n)
  Q <- rep(0,n)
  Q[1] <- - beta


  for(j in 2:(n+1)) {
    a <- Q[1] + cout(data[1:(j-1)]) + beta
    tau <- 1
    cpx <- cpx + 2 # + cout = ?

    for (i in 2:(j-1)) {
      val <- Q[i] + cout(data[i:(j-1)]) + beta
      if (val < a ) {
        a <- val
        tau <- i
        print(tau)
      }
      cpx <- cpx + 2 # + cout = ?
    }
    cp[t] <- tau
    t <- t + 1
  }


  v <- cp[n]
  P <- c(NULL)
  l <- 1
  while (v > cp[1]) {
    P[l] <- v
    v <- cp[v]

    v <- v - 1
    l <- l + 1
  }

  return(list(cp = cp, desc = P, complexity = cpx))
}

