#' Plot a density histogram with a normal distribution superimposed
#'
#' @param x     #The data to be plotted
#' @param mu    #The assumed true value for the population mean
#' @param sigma #The assumed true value for the population sd
#' @param plot.points #Should heights for select points be plotted?
#' @param plot.quantile #Should a normal quantile plot be included?
#' @param plot.curve #Should normal function be superimposed?
#' @param ran.seed  #Seed value to allow replication of specific sample
#'
#' @return A data frame consisting of selected quantiles and their heights
#'
#' @examples
#' histnormal()
#' histnormal(sunspots)
#' histnormal(sunspots,plot.points=TRUE,plot.quantile=TRUE)
#'
#' @export
histnormal <- function(x=rnorm(2000),mu=NULL,sigma=NULL,plot.points=FALSE,plot.quantile=FALSE,
                       plot.curve=FALSE,ran.seed=4){

  #create histogram of x and superimpose
  #normal distribution at mean(x) with sd(x)

  if(plot.quantile==TRUE){
    par(mfrow=c(1,2))
  }
  else {
    par(mfrow=c(1,1))
  }

  set.seed(ran.seed)

  hist(x,col="tan",prob=T,main="Histogram")  #use density scale to facilitate plotting of normal curve

  if(is.null(mu)){
   m <- mean(x)
   s <- sd(x)
  }
  else {
    m <- mu
    s <- sigma
  }


  if(plot.curve==TRUE){
    curve(dnorm(x,mean=m,sd=s),col="red",lwd=2,add=TRUE)
  }

  #extract densities at specified points for x

  z <- c(-2,-1,0,1,2)
  q <- (z*s) + m

  dout <- dnorm(q,mean=m,sd=s)

  if(plot.points==TRUE){
   points(q,dout,pch=19,col="black")
  }

  if(plot.quantile==TRUE){
    qqnorm(x,main="Normal Quantile Plot")
    qqline(x,col="red",lwd=2)
  }

  results <- data.frame(q,dout)
  results

}
