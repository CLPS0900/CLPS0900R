#' Plot a density histogram with a normal distribution superimposed
#'
#' @param x     #The data to be plotted
#' @param mu    #The assumed true value for the population mean
#' @param sigma #The assumed true value for the population sd
#' @param plot.points #Should heights for select points be plotted?
#' @param plot.quantile #Should a normal quantile plot be included?
#'
#' @return A data frame consisting of selected quantiles and their heights
#'
#' @examples
#' histnormal()
#'
#' @export
histnormal <- function(x=rnorm(1000),mu=NULL,sigma=NULL,plot.points=F,plot.quantile=F){

  #create histogram of x and superimpose
  #normal distribution at mean(x) with sd(x)

  if(plot.quantile==T){
    par(mfrow=c(1,2))
  }

  hist(x,col="tan",prob=T,main="Histogram + Normal")  #use density scale to facilitate plotting of normal curve

  if(is.null(mu)){
   m <- mean(x)
   s <- sd(x)
  }
  else {
    m <- mu
    s <- sigma
  }

  curve(dnorm(x,mean=m,sd=s),col="red",lwd=2,add=T)

  #extract densities at specified points for x

  z <- c(-2,-1,0,1,2)
  q <- (z*s) + m

  dout <- dnorm(q,mean=m,sd=s)

  if(plot.points==T){
   points(q,dout,pch=19,col="black")
  }

  if(plot.quantile==T){
    qqnorm(x,main="Normal Quantile Plot")
    qqline(x,col="red",lwd=2)
  }

  results <- data.frame(q,dout)
  results

}
