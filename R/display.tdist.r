#' Plot a t distribution for dfs 1-100.
#' Compares t distribution to a normal distribution and shows
#' the 'excess' area in the tails of the selected t distribution
#' relative to the standard normal distribution.
#'
#' @param n An integer for sample size.
#' @param plot.normal Logical.  Sets whether normal is superimposed.
#' @param add.quantile Logical.  Sets whether excess area in tails is displayed.
#'
#' @return None
#'
#' @examples
#' display.tfunction()
#'
#' @export
display.tfunction <- function(n=2,plot.normal=TRUE,add.quantile=TRUE){

 #show t dist

 t.max <- 8
 par(cex=1.25)
 par(mai=c(1,1,1,1))
 df <- n-1

 t.min <- -1*t.max
 x <- seq(t.min,t.max,by=.01)
 dt.out <- dt(x,df=df)
 y.upper <- max(dnorm(x,0,1))
 ylimits <- c(0,y.upper)

 #get areas in tails for t and normal

 q.check <- -1.76
 area.norm <- pnorm(q.check,0,1) * 2
 area.t <- pt(q.check,df=df)*2
 a.diff <- round(area.t-area.norm,4)

 if(add.quantile==T){
  p.title <- paste("t(df=",df,")","\nTail Excess=",a.diff,sep="")
 }
 else{
   p.title <- paste("t(df=",df,")",sep="")
 }

 plot(x,dt.out,xlab="X",type="l",main="",lwd=2,col="red",ylab="Density",ylim=ylimits)
 mtext(side=3,line=1,text=p.title,cex=1.5)

 if(add.quantile==TRUE){
  segments(-1.76,0,-1.76,.08,col="blue",lwd=2)
  segments(1.76,0,1.76,.08,col="blue",lwd=2)
 }

 if(plot.normal==TRUE){
   lines(x,dnorm(x,0,1),lwd=2,col="black")
 }

}
