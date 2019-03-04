#' Plot the null or alternative sampling distribution for a power analysis
#'
#' @param mu A value for the mean of the distribution
#' @param sd A positive value for the standard deviation of the distribution
#' @param xlimits Values for the range of the x-axis
#' @param plot.type One of: "Ho" or "Ha" for the type of distribution to plot
#' @param alpha A value between .005 and .1 for the Type I error rate
#' @param xcors The 4 quantiles that delimit the areas of interest
#' @param alt.hyp One of: "mu1<>mu0", "mu1<mu0", or "mu1>mu0"
#' @param shade.colors colors for the regions of interest
#' @param show.values Select whether Type I, II, and Power are displayed in plot.
#'
#' @return
#' pow Probability of correct rejection (For plot.type="Ha")
#'
#' @examples
#'  power.plot1()
#'
#' @export
power.plot1 <- function(mu=60,sd=1,xlimits=c(55,65),plot.type="Ho",alpha=.05,
                        xcors=c(56,59,61,65),
                        alt.hyp="mu1<mu0",
                        shade.colors=c("darkgoldenrod1","cornsilk","darkgoldenrod1"),
                        show.values=FALSE){

  xlo <- xlimits[1]
  xhi <- xlimits[2]

  if(plot.type=="Ho"){
    alpha <- round(alpha,3)
    main.text<- paste(plot.type,"(alpha=",alpha,")")
    pow <- NULL
    plot.val1 <- (1-alpha) #paste("1-alpha:",(1- alpha))
    plot.val2 <- alpha #paste("alpha:",alpha)
  }

  if(plot.type=="Ha"){
   #for plots of Ha, power is area to left of q2 and to right of q3
   q2 <- xcors[2]
   q3 <- xcors[3]

   p1 <- pnorm(q2,mu,sd)
   p2 <- 1 - pnorm(q3,mu,sd)
   pow <-p1 + p2
   pow <- round(pow,3)

   main.text <- paste(plot.type,"(Power=",pow,")")

   plot.val1 <- 1-pow
   plot.val2 <- pow


  }

  if(alt.hyp=="mu1<mu0"){
    shade.left<-T;shade.central<-T;shade.right<-F
  }
  if(alt.hyp=="mu1>mu0"){
    shade.left<-F;shade.central<-T;shade.right<-T
  }
  if(alt.hyp=="mu1<>mu0"){
    shade.left<-T;shade.central<-T;shade.right<-T
  }

  X <- seq(xlo,xhi,.01)
  dout <- dnorm(X,mean=mu,sd=sd)
  plot(X,dout,type="l",lwd=1.75,main="",xlab="",ylab="",xlim=xlimits,yaxt="n")
  mtext(main.text,side=3,cex=1.25,line=.75)

  x.left <- seq(xcors[1],xcors[2],.01)
  x.central <- seq(xcors[2],xcors[3],.01)
  x.right <- seq(xcors[3],xcors[4],.01)

  if(shade.left==T){
    cord.x <- c(xcors[1],x.left,xcors[2])
    cord.y <- c(0,dnorm(x.left,mu,sd),0)
    polygon(cord.x,cord.y,col=shade.colors[1])
  }
  if(shade.central==T){
    cord.x <- c(xcors[2],x.central,xcors[3])
    cord.y <- c(0,dnorm(x.central,mu,sd),0)
    polygon(cord.x,cord.y,col=shade.colors[2])
  }
  if(shade.right==T){
    cord.x <- c(xcors[3],x.right,xcors[4])
    cord.y <- c(0,dnorm(x.right,mu,sd),0)
    polygon(cord.x,cord.y,col=shade.colors[3])
  }


  if(show.values==TRUE){
    ypos <- 1*(max(dout)-min(dout))
    xpos <- xcors[1]
    plot.val1 <- round(plot.val1,3)
    plot.val2 <- round(plot.val2,3)
    legend(xpos,ypos,fill=c(shade.colors[2],shade.colors[1]),legend=c(plot.val1,plot.val2),bty="n")
  }


   pow

}


