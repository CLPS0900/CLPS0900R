#' Get quantiles needed for power analysis
#'
#' @param mu A value for the mean of the distribution
#' @param sd A positive value for the standard deviation of the distribution
#' @param alpha A value between .005 and .1 for the Type I error rate
#' @param alt.hyp One of: "mu1<>mu0", "mu1<mu0", or "mu1>mu0"
#' @param xlimits Values for the range of the x-axis
#'
#' @return
#' qs The 4 quantiles needed to obtain areas for Ho and Ha
#'
#' @examples
#'  get.quantiles()
#'
#' @export
get.quantiles <-function(mu=60,sd=1,alpha=.05,alt.hyp="mu1<mu0",xlimits=c(54,64)){

  qlo <- xlimits[1]
  qhi <- xlimits[2]

  if(alt.hyp=="mu1<mu0"){
    q <- qnorm(p=alpha,mu,sd)
    qs <- c(qlo,q,qhi,qhi)
  }
  if(alt.hyp=="mu1>mu0"){
    pright <- 1 - alpha
    q <- qnorm(p=pright,mu,sd)
    qs <- c(qlo,qlo,q,qhi)
  }
  if(alt.hyp=="mu1<>mu0"){
    a2 <- alpha/2
    a3 <- 1 - a2
    q2 <- qnorm(p=a2,mu,sd)
    q3 <- qnorm(p=a3,mu,sd)
    qs <- c(qlo,q2,q3,qhi)
  }

  qs

}
