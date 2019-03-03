#' Manipulate plot the null or alternative sampling distribution for a power analysis
#'
#' @param mu0 A value for the mean under the null hypothesis
#' @param mu1 A value for the mean under the alternative hypothesis
#' @param sd A positive value for the standard deviation for the population (null)
#' @param n  A value for the sample size
#'
#' @return
#' res Summary, included Type I error (alpha), Type II error (beta), and Power
#'
#' @examples
#' \dontrun{
#' run.power()
#' }

#' @export
run.power <- function(mu0=60,mu1=57,sd=10,n=100){

  sem <- sd/sqrt(n)
  mu.lower <- mu0-4*sem
  mu.upper <- mu0+4*sem
  step.val <- sem/10

  manipulate(power1(mu0=mu0,mu1=mu1,sd=sd,n=n,alpha=alpha,alt.hyp=alt.hyp),
           mu0=slider(mu.lower,mu.upper,step=step.val,initial=mu0,label="mu0"),
           mu1=slider(mu.lower,mu.upper,step=step.val,initial=mu1,label="mu1"),
           alt.hyp=picker("mu1<>mu0","mu1<mu0","mu1>mu0",label="Alternative Hypothesis"),
           sd=slider(1,100,initial=10,label="sd"),
           n=slider(2,200,initial=100,label="N"),
           alpha=slider(.005,.1,step=.005,initial=.05,label="alpha")

      )


}
