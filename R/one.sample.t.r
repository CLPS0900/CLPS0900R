#' One-sample Students t test, with display of t function and
#' areas beyond t observed.
#'
#' @param x Data to be used
#' @param sample.mean A value to be supplied if original observations are not.
#' @param sample.sd A value to be supplied if original observations are not.
#' @param n The number of observations. (Use when dat are not supplied.)
#' @param mu A value for the expected mean under the null hypothesis
#' @param test.type 2-sided or 1-sided
#'
#' @return Test results
#'
#' @examples
#' one.sample.t(x=rnorm(10),mu=0)
#'
#' @export
one.sample.t <- function(x=rnorm(10),sample.mean=NULL,sample.sd=NULL,n=NULL,mu=0,
                         test.type="2-sided"){

 if(is.null(x) & is.null(sample.mean)){
   stop("You must provided either sample data (x) or sample mean, sd, and N")
 }
 xdata <- x

 if(!is.null(x)){
   sample.mean <- mean(x)
   sample.sd <- sd(x)
   n <- length(x)
 }

 sem <- sample.sd/sqrt(n)
 t.obs <- (sample.mean-mu)/sem
 df <- n-1
 p1 <- 1-pt(abs(t.obs),df=df)

 max.t <- 8
 min.t <- max.t * -1

 if(test.type=="1-sided"){
   p <- p1
 }
 else {
   p <- 2*p1
 }

 t.obs.r <- round(t.obs,2)
 p.r <- round(p,4)

 if(abs(t.obs)<=max.t){

 x <- seq(-6,6,by=.01)
 dt.out <- dt(x,df=df)

 p.title <- paste("t(df=",df,") = ",t.obs.r,"\np = ",p.r," (",test.type,")",sep="")
 plot(x,dt(x,df=df),xlab="t",ylab="",type="l",main=p.title,col="black",lwd=2,yaxt="n")

 if(test.type=="2-sided"){
   shade.left <- T
   shade.right <- T
 }
 if(test.type=="1-sided" & t.obs < 0){
   shade.left <- T
   shade.right <- F
 }
 if(test.type=="1-sided" & t.obs > 0){
   shade.left <- F
   shade.right <- T
 }

 t2 <- abs(t.obs)
 t1 <- t2 * -1

 #shade entire regions as default

 x.all <- seq(min.t,max.t,.01)
 cord.x <- c(t1,x.all,t2)
 cord.y <- c(0,dt(x.all,df=df),0)
 polygon(cord.x,cord.y,col="cadetblue")

 if(shade.left==T){
   x.left <- seq(min.t,t1,.01)
   cord.x <- c(min.t,x.left,t1)
   cord.y <- c(0    ,dt(x.left,df=df),0 )
   polygon(cord.x,cord.y,col="tan")
 }

 if(shade.right==T){
   x.right <- seq(t2,max.t,.01)
   cord.x <- c(t2,x.right,max.t)
   cord.y <- c(0    ,dt(x.right,df=df),0 )
   polygon(cord.x,cord.y,col="tan")
 }

 }
 else {
   cat("\nYou t ratio is outside the plot range.  Your test results are shown below.\n")
 }

 tout <- t.test(x=xdata,mu=mu)
 c.int <- tout$conf.int

 results <- matrix(c(round(sample.mean,2),round(sample.sd,2),n,df,t.obs.r,p.r,mu,c.int[1],c.int[2]))
 dimnames(results)[[1]] <- c("mean","sd","n","df","t","p","mu(Ho)","CI.lower","CI.upper")
 dimnames(results)[[2]] <- "Value"

 cat("\n\n***********************************************************\n")
 cat(paste("\nStudents One-sample t (",test.type,")\n\n",sep=""))
 print(t(results))
 cat("\n***********************************************************")

}
