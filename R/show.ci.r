#' Plot Confidence Intervals for CLPS0900
#'
#' @param nsamp Integer for number of samples to display
#' @param ssize The size of each sample
#' @param mu Population mean
#' @param sigma Population sigma
#' @param conf.level Confidence level
#' @param do.pause Whether to pause between each CI
#' @param ci.method Whether sigma is known or unknown
#' @param show.data Whether sample data are displayed
#' @param xaxis.type Whether x axis if fixed or variable
#' @param go.button Used for manipulate only
#' @return None
#'
#' @examples
#' show.ci()
#'
#' @export
show.ci <- function(nsamp=100,ssize=2,mu=0,sigma=1,conf.level=.68,
                        do.pause="None",ci.method="Sigma known",show.data=FALSE,
                        xaxis.type="Fixed",go.button=NULL){

  show.miss <- "Highlight" #Dont Highlight"
  samp.means <- rep(NA,nsamp)
  xlabel <- "Confidence Interval"
  if(nsamp > 10){ do.pause <- "None" }
  conf.level <- round(conf.level,2)

  par(mfrow=c(1,1))
  par(mai=c(1,1,1,1))

  se <- sigma/sqrt(ssize)

  cat("\n
  ################################################################\n",
  " #mu=",mu,"sigma=",sigma,"N=",ssize,
  "\n  ################################################################")

  if(xaxis.type=="Fixed"){
    iw <- conf.level*6*sigma
    xlo <- mu-iw
    xhi <- mu+iw
   }

  if(xaxis.type=="Variable" & ci.method=="Sigma known"){
   xlo <- mu - 5*conf.level*se
   xhi <- mu + 5*conf.level*se
  }

  if(xaxis.type=="Variable" & ci.method=="Sigma unknown"){
    xlo <- mu - 10*conf.level*se
    xhi <- mu + 10*conf.level*se
  }

  xlimits <- c(xlo,xhi)
  z.crit <- abs(qnorm( ((1 - conf.level)/2),mean=0,sd=1))
  main.text <- paste("Simulation of CI",conf.level,"N=",ssize)
  plot(c(xlo,xhi),c(1,nsamp),type="n",main="",xlab=xlabel,ylab="Sample number")
  mtext(side=3,line=1.5,main.text,cex=1.5)

  nhits <- 0

 for(sn in 1:nsamp){

  x <- rnorm(ssize,mu,sigma)

  if(mu==100 & sigma==15 & ssize <= 4){ # & ci.method=="Sigma known"){
    x <- round(x,2)
  }

  xm <- mean(x)
  samp.means[sn] <- xm

  if(ci.method=="Sigma unknown"){
   tout <- t.test(x,conf.level=conf.level)
   ci <- tout$conf.int
  }

  if(ci.method=="Sigma known"){
    ci1 <- xm - z.crit *se
    ci2 <- xm + z.crit *se
    ci <- c(ci1,ci2)
  }

  line.color <- "red"

  if(show.miss != "Highlight"){
   line.color <- 1
  }

  if(ci[1] < mu & ci[2] > mu){
   line.color <- 1
   nhits <- nhits+1
  }

  segments(xm,sn-.3,xm,sn+.3,lwd=2)
  segments(ci[1],sn,ci[2],sn,col=line.color,lwd=2)

  if(1==2){
  if(show.data==TRUE & ssize <= 4 & nsamp <= 10){
    if(xm >= mu){
      text(ci[2],sn,toString(round(sort(x),0)),adj=-.1,cex=.7)
    }
    if(xm < mu){
      text(ci[1],sn,toString(round(sort(x),0)),adj=1.1,cex=.7)
    }
  }
  }

  if(show.data==TRUE & ssize <= 4 & nsamp <= 10){
   xt <- round(sort(x),2)
   cat("\n   Data: ",xt)
  }

  if( nsamp <= 50){ #show.data==TRUE ){ #& nsamp <= 10){

     ci.width <- (ci[2]-ci[1])/2
     ci <- round(ci,2)
     ci.width <- round(ci.width,2)
     cat("\n   Confidence Interval: ","[",ci[1],"-",ci[2],"]","  [",xm," +/- ",ci.width,"]",sep="")
   }

 }#sn

  abline(v=mean(samp.means),col="blue",lty=4,lwd=2)
  abline(v=mu,col="black",lwd=2,lty=2)
  p.hits <- round(nhits/nsamp,3)
  mtext(paste("Proportion hits=",p.hits),side=3,line=.5,cex=1.25)

}
