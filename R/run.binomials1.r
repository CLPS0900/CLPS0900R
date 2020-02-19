#' Demonstrate binomials using manipulate controls that allow
#' you to set number of flips, probability of success, whether
#' the plot is counts or probabilities, and whether a Gaussian
#' distribution is superimposed.  Also calculates areas to the
#' left and right of critical values for counts or probabilities.

#' @examples
#'\dontrun{
#' binomials1()
#'}
#' @export
run.binomials1 <- function(){

  ###################################################################
  #manipulate binomials
  ###################################################################

  par(mfrow=c(1,1))

  manipulate(binomials1(n=n,p=p,plot.type=plot.type,x.axis.type=x.axis.type,y.axis.type=y.axis.type,
                        add.normal=add.normal,
                        add.values=add.values,
                        crit.value.c=crit.value.c,
                        crit.value.p=crit.value.p,
                        show.critical=show.critical),
             p=slider(.01,.99,step=.01,initial=.5,label="Probability of Success (p)"),
             n=slider(3,500,initial=3,label="Sample Size (N)",step=1),
             crit.value.c=slider(0,200,initial=0,label="Critical Value (Counts)"),
             crit.value.p=slider(0,1,step=.01,initial=.5,label="Critical Value (Proportions)"),
             plot.type=picker("Counts","Proportions",label="Plot Type"),
             x.axis.type=picker("Full Range","Gaussian",initial="Gaussian",label="X Axis Type"),
             y.axis.type=picker("Full Range","Fixed",label="Y Axis Type"),
             add.normal=checkbox(label="Add Normal Distribution"),
             add.values=checkbox(label="Display probabilities"),
             show.critical=checkbox(label="Show Critical Value")
  )

}

