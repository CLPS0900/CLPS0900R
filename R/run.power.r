#' Manipulate plot the null or alternative sampling distribution for a power analysis
#'
#' @param mu0 A value for the mean under the null hypothesis
#' @param mu1 A value for the mean under the alternative hypothesis
#' @param sd A positive value for the standard deviation for the population (null)
#' @param n.init  A value for the sample size
#'
#' @return
#' res Summary, included Type I error (alpha), Type II error (beta), and Power
#'
#' @examples
#' \dontrun{
#' run.power()
#' }

#' @export
run.power <- function(mu0=50,mu1=50,sd=10,n.init=20){

  sem <- sd/sqrt(n.init)
  mu.lower <- round(mu0-4*sem,1)
  mu.upper <- round(mu0+4*sem,1)
  step.val <- .1

  manipulate(power1(mu0=mu0,mu1=mu1,sd=sd,n=n,alpha=alpha,alt.hyp=alt.hyp),
           mu0=slider(mu.lower,mu.upper,step=step.val,initial=mu0,label="Mean for Null Hyp. (mu0)"),
           mu1=slider(mu.lower,mu.upper,step=step.val,initial=mu1,label="Mean for Alternative Hyp. (mu1)"),
           alt.hyp=picker("mu1<>mu0","mu1<mu0","mu1>mu0",label="Alternative Hypothesis"),
           sd=slider(1,100,initial=10,label="Population Sigma"),
           n=slider(2,200,initial=n.init,step=1,label="Sample Size (N)"),
           alpha=slider(.005,.2,step=.005,initial=.05,label="alpha")
      )


}
