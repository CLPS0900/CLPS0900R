#' Manipulate t distribution.
#' Displays a t distribtution for dfs 1-100, compares t distribtution
#' to normal distribution if desired, and shows 'excess' area in the tails
#' of a t distribution relative to a standard normal distribution.
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' run.tdist()
#' }
#'
#' @export
run.tdist <- function(){

  #illustrate t distribution compared to normal

  manipulate(display.tfunction(n=n,plot.normal=plot.normal,add.quantile=add.quantile),
             n=slider(2,100,initial=2,label="Sample Size (N)"),
             plot.normal=checkbox(label="Display Normal Distribution"),
             add.quantile=checkbox(label="Show Area in Tails"))
}
