#' Perform a power analysis, with plots for null and alternative distributions
#'
#' @param mu0 A value for the mean under the null hypothesis
#' @param mu1 A value for the mean under the alternative hypothesis
#' @param sd A positive value for the standard deviation for the population (null)
#' @param n  A value for the sample size
#' @param alpha A value between .005 and .1 for the Type I error rate
#' @param alt.hyp One of: "mu1<>mu0", "mu1<mu0", or "mu1>mu0"
#' @param show.values Select whether Type I, II, and Power are displayed in plot.
#' @param show.means Whether to plot means for Ho and Ha
#'
#' @return
#' res Summary, included Type I error (alpha), Type II error (beta), and Power
#'
#' @examples
#'  power1()
#'
#' @export
power1 <- function(mu0=50,mu1=50,sd=10,n=100,alpha=.05,alt.hyp="mu1<>mu0",show.values=FALSE,show.means=FALSE){

  par(mfrow=c(2,1))
  par(mai=c(.5,.5,.5,.5))

  #sem

  sem <- sd/sqrt(n)
  xlo <- min(c(mu0,mu1)) - 5*sem
  xhi <- max(c(mu0,mu1)) + 5*sem
  xlimits <- c(xlo,xhi)

  #get quantiles for area of rejection under Ho

  qs <- get.quantiles(mu=mu0,sd=sem,alpha=alpha,alt.hyp=alt.hyp,xlimits=xlimits)

  #plot Ho

  pow <- power.plot1(mu=mu0,sd=sem,xcors=qs,alt.hyp=alt.hyp,xlimits=xlimits,plot.type="Ho",alpha=alpha,
                     show.values=show.values,show.means=show.means)


  #plot Ha

  if(alt.hyp=="mu1<>mu0"){
    pow <- power.plot1(mu=mu1,sd=sem,xcors=qs,alt.hyp=alt.hyp,xlimits=xlimits,plot.type="Ha",show.values=show.values,
                       show.means=show.means,
                shade.colors=c("cadetblue3","brown3","cadetblue3"))
  }

  if(alt.hyp=="mu1<mu0" & mu1<=mu0){
   #effect can be detected...
    pow <- power.plot1(mu=mu1,sd=sem,xcors=qs,alt.hyp=alt.hyp,xlimits=xlimits,plot.type="Ha",show.values=show.values,
                       show.means=show.means,
              shade.colors=c("cadetblue3","brown3","cadetblue3"))
  }

  if(alt.hyp=="mu1>mu0" & mu1>=mu0){
    pow <- power.plot1(mu=mu1,sd=sem,xcors=qs,alt.hyp=alt.hyp,xlimits=xlimits,plot.type="Ha",show.values=show.values,
                       show.means=show.means,
                shade.colors=c("cadetblue3","brown3","cadetblue3"))
  }

  if(alt.hyp=="mu1<mu0" & mu1>mu0){
    cat("\n
    *************************************************************************
    WARNING!
    Your alternative hypothesis is mu1 < mu0, but your mu1 > mu0!!
    Under these conditions, you might reject the null, but for the WRONG REASON.
    This is sometimes called a Type III Error.  You should consider using a non-
    directional alternative hypothesis and/or re-evaluate whether the directional
    alternative you selected is apppropriate.
    *************************************************************************
    \n")

    pow <- power.plot1(mu=mu1,sd=sem,xcors=qs,alt.hyp=alt.hyp,xlimits=xlimits,plot.type="Ha",
                       show.means=show.means,
                shade.colors=c("brown3","brown3","brown3"))

  }

  if(alt.hyp=="mu1>mu0" & mu1<mu0){
    cat("\n
    *************************************************************************
    WARNING!
    Your alternative hypothesis is mu1 > mu0, but your mu1 < mu0!!
    Under these conditions, you might reject the null, but for the WRONG REASON.
    This is sometimes called a Type III Error.  You should consider using a non-
    directional alternative hypothesis and/or re-evaluate whether the directional
    alternative you selected is apppropriate.
    *************************************************************************
    \n")
    pow <- power.plot1(mu=mu1,sd=sem,xcors=qs,alt.hyp=alt.hyp,xlimits=xlimits,plot.type="Ha",
                       show.means=show.means,
                shade.colors=c("brown3","brown3","brown3"))

  }

  beta <- 1 - pow

  res <- matrix(c(mu0,mu1,sd,n,sem,alpha,beta,pow))
  vnames <- c("mu0","mu1","sd","n","sem","alpha","beta","power")
  dimnames(res)[[1]] <- vnames
  dimnames(res)[[2]] <- "value"
  res <- round(t(res),3)
  cat("\n")

  res


}
