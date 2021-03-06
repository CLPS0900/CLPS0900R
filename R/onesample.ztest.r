#' One sample NHST with known mu and sigma
#'
#' @param mu  A value for assumed population true mean
#' @param sigma  A value for assumed population true sigma
#' @param n  An integer for sample size (if data not provided).
#' @param observed.mean  A value for the observed sample mean if data not provided.
#' @param test.type  one- or two-sided test
#' @param conf.level A value for confidence level for interval (.68-.95)
#'
#' @return
#' none
#'
#' @examples
#'  onesample.ztest(observed.mean=110,n=4,mu=100,sigma=15)
#'
#' @export
onesample.ztest <- function(observed.mean=110,mu=100,sigma=15,n=10,test.type="2-sided",conf.level=.95){

  #illustrate test of mean, with known population mu and sigma

  sem <- sigma/sqrt(n)
  xlo <- mu - 4*sem
  xhi <- mu + 4*sem
  X <- seq(xlo,xhi,sem/100)
  abs.diff <- abs(observed.mean - mu)

  limit.lo <- mu-abs.diff
  limit.hi <- mu+abs.diff
  p1 <- round(pnorm(limit.lo,mean=mu,sd=sem),4)
  p2 <- 2*p1
  z <- round((observed.mean - mu)/sem,3)

  p.smaller <- (1-conf.level)/2
  zcrit <- abs(qnorm(p.smaller))
  ci.lower <- round(observed.mean - zcrit*sem,2)
  ci.upper <- round(observed.mean + zcrit*sem,2)

  ci.text <- paste("Confidence Interval (",conf.level,"): ",ci.lower," ",ci.upper,sep="")

  main.text <- paste("NHST of Mean (Ho: mu=",mu,", sigma=", sigma,")\nN=",n,", SEM=",round(sem,2),
                     ", Observed Mean=",round(observed.mean,2),sep="")
  plot(X,dnorm(X,mean=mu,sd=sem),type="l",lwd=2,main="",xlab="Mean",ylab="Density")
  mtext(main.text,side=3,cex=1,line=1)

  x.seq <- seq(xlo,xhi,.01)
  cord.x <- c(xlo,x.seq,xhi)
  cord.y <- c(0,dnorm(x.seq,mu,sem),0)
  polygon(cord.x,cord.y,col='lightblue')

  ymax <- max(dnorm(X,mean=mu,sd=sem))

  observed.mean <- round(observed.mean,2)

   if(test.type=="1-sided" & observed.mean <= mu){

     ptext <- paste("p(M<=",limit.lo,")=",p1)
     x.seq <- seq(xlo,limit.lo,.01)
     cord.x <- c(xlo,x.seq,limit.lo)
     cord.y <- c(0,dnorm(x.seq,mu,sem),0)
     polygon(cord.x,cord.y,col='tan')
     text(limit.lo,max(cord.y),ptext,adj=1.05,cex=.8)
     result <- paste("Z = ",z,"p (1-tailed) = ",p1)
     segments(limit.lo,0,limit.lo,max(cord.y),lwd=2,col="red")
   }

   if(test.type=="1-sided" & observed.mean > mu){

     ptext <- paste("p(M>=",limit.hi,")=",p1)
     x.seq <- seq(limit.hi,xhi,.01)
     cord.x <- c(limit.hi,x.seq,xhi)
     cord.y <- c(0,dnorm(x.seq,mu,sem),0)
     polygon(cord.x,cord.y,col='tan')
     text(limit.hi,max(cord.y),ptext,adj=-.05,cex=.8)
     result <- paste("Z = ",z,"p (1-tailed) = ",p1)
     segments(observed.mean,0,observed.mean,max(cord.y),lwd=2,col="red")
   }

  if(test.type=="2-sided"){

    ptext <- paste("p(M<=",limit.lo,")=",p1)
    x.seq <- seq(xlo,limit.lo,.01)
    cord.x <- c(xlo,x.seq,limit.lo)
    cord.y <- c(0,dnorm(x.seq,mu,sem),0)
    polygon(cord.x,cord.y,col='tan')
    text(limit.lo,max(cord.y),ptext,adj=1.05,cex=.8)

    ptext <- paste("p(M>=",limit.hi,")=",p1)
    x.seq <- seq(limit.hi,xhi,.01)
    cord.x <- c(limit.hi,x.seq,xhi)
    cord.y <- c(0,dnorm(x.seq,mu,sem),0)
    polygon(cord.x,cord.y,col='tan')
    text(limit.hi,max(cord.y),ptext,adj=-.05,cex=.8)
    result <- paste("Z = ",z,"p (2-tailed) = ",p2)

    segments(observed.mean,0,observed.mean,max(cord.y),lwd=2,col="red")

  }

  cat("\n\n*****************************\n\n")
  result <- cat(result,"\n",ci.text)
  cat(result)

}
