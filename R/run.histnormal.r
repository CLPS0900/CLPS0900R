#' Manipulate histnormal function
#'
#' @param x Data to be plotted
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' histnormal()
#' }
#'
#' @export
run.histnormal <- function(x=rnorm(1000)) {

  #################################################################
  #manipulate histnormal function
  #################################################################

  manipulate(histnormal(x=x,plot.points=plot.points,
                        plot.quantile=plot.quantile,
                        plot.curve=plot.curve),
             plot.points=checkbox(label="Plot points for heights"),
             plot.curve=checkbox(label="Show normal curve"),
             plot.quantile=checkbox(label="Normal quantile plot")
            )
}
