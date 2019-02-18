#' Demonstration of binomial distributions
#'
#' @param n Integer for number of flips
#' @param p Probability of success (a value between 0 and 1)
#' @param x.axis.type ("Gaussian","Full Range")
#' @param y.axis.type ("Full Range","Fixed")
#' @param plot.type ("Counts","Proportions")
#' @param add.normal Logical (False)
#' @param add.values Logical (False)
#' @param crit.value.c Critical value for counts
#' @param crit.value.p Critical value for probability
#' @param show.critical Logical (False)
#'
#' @return None
#'
#' @examples
#' binomials1()
#'
#' @export
binomials1 <- function(n=5,p=.5,x.axis.type="Gaussian",y.axis.type="Full Range",plot.type="Counts",add.normal=FALSE,
                       add.values=FALSE,crit.value.c=1,crit.value.p=.5,show.critical=FALSE){

  ###################################################################
  #Recreate binomial demonstrations using mosaic
  ###################################################################

  ###################################################################
  #setup

  lwd.setting <- 3

  ps <- dbinom(0:n,n,prob=p)
  q <- 1 - p
  p <- round(p,3)
  q <- round(q,3)

  if(plot.type=="Counts"){
    x <- c(0:n)
    mu <- n*p
    sigma <- round(sqrt(n*p*q),3)
    m.title <- paste("Binomial Distribution (Counts) N=",n,"p=",p,"\nmu=",mu,"s=",sigma)
    xlabel <- "# Successes"
    crit.value <- crit.value.c
  }

  if(plot.type=="Proportions"){
    x <- c(0:n)/n
    mu <- p
    sigma <- round(sqrt((p*q)/n),3)
    m.title <- paste("Binomial Distribution (Proportions) N=",n,"p=",p,"\nmu=",mu,"s=",sigma)
    xlabel <- "Proportion Successes"
    crit.value <- crit.value.p
  }

  np <- n*p; nq <- n*q
  np.text <- paste("Np=",np)
  nq.text <- paste("Nq=",nq)
  m.title <- paste(m.title,np.text,nq.text)

  df <- data.frame(x=x,ps=ps)

  if(x.axis.type=="Gaussian"){
    xl <- mu - 5*sigma
    xh <- mu + 5*sigma
    xlimits <- c(xl,xh)
  }

  if(x.axis.type=="Full Range"){
    xlimits <- range(x)
  }

  if(y.axis.type=="Full Range"){
    yl <- 0
    yh <- max(ps) + .1*(max(ps)-min(ps))
    ylimits <- c(yl,yh)
  }

  if(y.axis.type=="Fixed"){
    ylimits <- c(0,.35)
  }

  ###################################################################
  #plot

  par(cex=1)

  plot(x,ps,type="n",xlim=xlimits,ylim=ylimits,xlab=xlabel,ylab="Probability",main=m.title)
  points(x,ps,pch=19,cex=1.25,col="blue")
  lines(x,ps,lty=1,lwd=lwd.setting,col="blue")

  ###################################################################
  #add elements

  if(add.normal==TRUE){
    xfit <- seq(min(xlimits),max(xlimits),length=500)
    yfit <- dnorm(xfit,mu,sigma)

    if(plot.type=="Proportions"){
      yfit <- yfit/n
    }
    lines(xfit,yfit,col="red",lwd=lwd.setting)
  }

  if(add.values==TRUE){
    val <- paste("p=",round(ps,3))
    text(x[x<mu],ps[x<mu],val[x<mu],adj=1.25,cex=.7)
    text(x[x>=mu],ps[x>=mu],val[x>=mu],adj=-.4,cex=.7)
  }

  if(show.critical==TRUE){

    abline(v=crit.value,lwd=1,lty=2,col="blue")

    p.exact.lo <- round(sum(ps[x < crit.value]),3)
    p.exact.hi <- 1 - p.exact.lo
    p.est.lo <- round(pnorm(crit.value,mu,sigma),3)
    p.est.hi <- 1 - p.est.lo

    p.exact.lo.t <- paste("Actual p(<CV)=",p.exact.lo)
    p.exact.hi.t <- paste("Actual p(>=CV)=",p.exact.hi)

    p.est.lo.t <- paste("Est. p(<CV)=",p.est.lo)
    p.est.hi.t <- paste("Est. p(>=CV)=",p.est.hi)

    text(crit.value,ylimits[2],p.exact.lo.t,adj=1.05,cex=.75,col="blue")
    text(crit.value,ylimits[2],p.exact.hi.t,adj=-.05,cex=.75,col="blue")

    yl2 <- ylimits[2] - .05*(ylimits[2]-ylimits[1])
    text(crit.value,yl2,p.est.lo.t,adj=1.05,cex=.75,col="red")
    text(crit.value,yl2,p.est.hi.t,adj=-.05,cex=.75,col="red")

  }

}
