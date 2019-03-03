#' One sample t test
#'
#' @param x  Data to be tested
#' @param test.type  one- or two-sided test
#'
#' @return
#' none
#'
#' @examples
#'  onesample.ttest(x=rnorm(10),test.type="2-sided")
#'
#' @export
onesample.ttest <- function(x=rnorm(10,0,1),test.type="1-sided"){

  #illustrate one-sample t-test of observed mean vs Ho with assumed mu, sigma unknown

  mu <- 0
  observed.mean <- round(mean(x),3)
  s <- round(sd(x),3)
  n <- length(x)
  df <- n-1

  sem <- s/sqrt(n)
  t.obs <- round((observed.mean - mu)/sem,3)

  xlo <- -6
  xhi <- 6

  X <- seq(xlo,xhi,sem/100)
  limit.lo <- 0 - abs(t.obs)
  limit.hi <- abs(t.obs)

  p1 <- round(pt(q=limit.lo,df=df),4)
  p2 <- 2*p1

  main.text <- paste("1-Sample t-test (Ho: mu=",mu,") (Sample: M=",observed.mean," s=", s,")\nN=",n,", SEM=",round(sem,2),
                     ", t observed (df=",df,")= ",round(t.obs,2),sep="")
  plot(X,dt(X,df=df),type="l",lwd=2,main="",xlab="Students t",ylab="Density")
  mtext(main.text,side=3,cex=1,line=1)

  x.seq <- seq(xlo,xhi,.01)
  cord.x <- c(xlo,x.seq,xhi)
  cord.y <- c(0,dt(x.seq,df=df),0)
  polygon(cord.x,cord.y,col='lightblue')

  ymax <- max(dt(X,df=df))

   if(test.type=="1-sided" & t.obs <= 0){

     ptext <- paste("p(t <=",limit.lo,")=",p1)
     x.seq <- seq(xlo,limit.lo,.01)
     cord.x <- c(xlo,x.seq,limit.lo)
     cord.y <- c(0,dt(x.seq,df=df),0)
     polygon(cord.x,cord.y,col='tan')
     text(limit.lo,max(cord.y),ptext,adj=1.05,cex=.8)
     result <- paste("t(df=",df,") = ",t.obs,", p = ",p1," (1-tailed)",sep="")
     segments(t.obs,0,t.obs,max(cord.y),lwd=2,col="red")

   }

   if(test.type=="1-sided" & t.obs > 0){

     ptext <- paste("p(t >=",limit.hi,")=",p1)
     x.seq <- seq(limit.hi,xhi,.01)
     cord.x <- c(limit.hi,x.seq,xhi)
     cord.y <- c(0,dt(x.seq,df=df),0)
     polygon(cord.x,cord.y,col='tan')
     text(limit.hi,max(cord.y),ptext,adj=-.05,cex=.8)
     result <- paste("t(df=",df,") = ",t.obs,", p = ",p1," (1-tailed)",sep="")
     segments(t.obs,0,t.obs,max(cord.y),lwd=2,col="red")

   }

  if(test.type=="2-sided"){

    ptext <- paste("p(t <=",limit.lo,")=",p1)
    x.seq <- seq(xlo,limit.lo,.01)
    cord.x <- c(xlo,x.seq,limit.lo)
    cord.y <- c(0,dt(x.seq,df=df),0)
    polygon(cord.x,cord.y,col='tan')
    text(limit.lo,max(cord.y),ptext,adj=1.05,cex=.8)
    segments(t.obs,0,t.obs,max(cord.y),lwd=2,col="red")

    ptext <- paste("p(t >=",limit.hi,")=",p1)
    x.seq <- seq(limit.hi,xhi,.01)
    cord.x <- c(limit.hi,x.seq,xhi)
    cord.y <- c(0,dt(x.seq,df=df),0)
    polygon(cord.x,cord.y,col='tan')
    text(limit.hi,max(cord.y),ptext,adj=-.05,cex=.8)
    segments(t.obs,0,t.obs,max(cord.y),lwd=2,col="red")

    result <- paste("t(df=",df,") = ",t.obs,", p = ",p2," (2-tailed)",sep="")


  }

  result
}
