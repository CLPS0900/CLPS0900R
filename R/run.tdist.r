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

  manipulate(display.tfunction(df=df,plot.normal=plot.normal,add.quantile=add.quantile),
             df=slider(2,200,initial=2,label="df"),
             plot.normal=checkbox(label="Display Normal Distribution"),
             add.quantile=checkbox(label="Show Area in Tails"))
}
