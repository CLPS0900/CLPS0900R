#' Return sigma for assumed population
#'
#' @param x a vector consisting of the elements of the population
#'
#' @return sigma for the population
#'
#' @examples
#' sdpop(x=c(1:6))
#'
#' @export
sdpop <- function(x){
  #return sd assuming x is a population and thus var = SS/N
  ss <- sum((x - mean(x))^2)
  s <- sqrt(ss/length(x))
  s
}
