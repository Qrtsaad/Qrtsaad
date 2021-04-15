#' Cost Gauss
#'
#'
#' @description
#'
#' @param trajectoire vector of trajectoire from data points
#'
#' @return a number
#' @export
#'
#' @examples
cost_gauss <- function(trajectoire) {
  n = length(trajectoire)
  if (n == 1) {res = 0} else {res = (n-1)*var(trajectoire)}
  return (res)
}



