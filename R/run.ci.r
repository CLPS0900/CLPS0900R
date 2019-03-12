#' Manipulate confidence intervals
#'
#' @return None
#'
#' @examples
#' \dontrun{
#'  run.ci()
#' }
#'
#' @export
run.ci <- function(){

  cat("\n\n
  ################################################################
  #Demonstrate confidence intervals for clps0900.
  #Allow user to set mu, sigma of population, sample size, number
  #of samples, confidence level, and whether sigma is known or
  #unknown.  Population mu is shown as dotted black line; average
  #of all confidence intervals is shown as dotted blue line.
  #Vertical position of each sample is arbitrary and used only
  #to allow viewing of intervals.
  ################################################################\n")

  set.seed(5)  #4 to check results for Question 1

  manipulate(show.ci(nsamp,ssize=ssize,conf.level=conf.level,do.pause=do.pause,ci.method=ci.method,
                         show.data=show.data,go.button=go.button,mu=mu,sigma=sigma,xaxis.type=xaxis.type),
             mu=slider(0,100,initial=100,label="Population mu"),
             sigma=slider(1,100,initial=15,label="Population sigma"),
             nsamp=slider(10,100,label="Number of samples"),
             ssize=slider(2,100,label="Sample size",initial=4),
             conf.level=slider(.68,.96,step=.01,initial=.95,label="Confidence Level"),
             ci.method=picker("Sigma known","Sigma unknown",label="CI Method"),
             do.pause=picker("None",initial="None",label="Type of Pause"),
             xaxis.type=picker("Fixed","Variable",initial="Fixed",label="Horizontal Axis"),
             show.data=checkbox(label="Show data",initial=TRUE),
             go.button=button("Go")
  )


}

