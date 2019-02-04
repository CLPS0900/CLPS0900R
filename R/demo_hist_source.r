#' Illustration of histogram
#'
#' Creates a histogram of using default R settings.  This function is used only to test
#' package installation.
#'
#' @param x numeric data
#'
#' @return None
#'
#' @examples
#' demo_hist(sunspots)
#'
#' @export
demo_hist <- function(x=sunspots){
  hist(x,main="Histogram of Sunspots")
}
