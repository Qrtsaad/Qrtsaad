#' Best Beta
#'
#'
#' @description
#'
#' @param data vector of data points
#'
#' @return a number
#' @export
#'
#' @examples
best_beta <- function(data){
  n = length(data)
  return (2*var(data)*log(n))
}
