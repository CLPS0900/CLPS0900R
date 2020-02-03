#' Perform two-way anova and related methods using simulated anxiety data.
#' Typically called by run_anxiety()
#'
#' @param x A data frame consisting of gender, condition, and anxiety (DV)
#' @param show.hist Controls display of initializing histogram
#' @param show.summary Controls display of summary statistics
#' @param show.anova Controls display of 2-way ANOVA
#' @param plot.main.gender Controls display of gender main effect
#' @param plot.main.condition Controls display of main effect for treatment
#' @param plot.interaction Controls display of interaction
#' @param show.tukey Controls display of Tukey HSD for pairwise comparisons
#'
#' @return
#' none
#'
#' @examples
#' \dontrun{
#' eda.anxiety()
#' }
#' @export
eda.anxiety <- function(x=NULL,show.hist=TRUE,show.summary=FALSE,show.anova=FALSE,
                          plot.main.gender=FALSE,plot.main.condition=FALSE,plot.interaction=FALSE,
                          show.tukey=FALSE){

  #require psych

  par(mfrow=c(2,2))
  par(mai=c(.7,.8,.5,.2))
  cat("\014")

  if(is.null(x)){
    if(file.exists("MBSR.data.csv")){
      MBSR_DATA = read.csv("MBSR.data.csv")
      x <- MBSR_DATA
      #assign("MBSR_DATA",x,pos=1)
    }
    else {
      stop("\n
As noted in the instructions for this assignment, you
must create a data file that contains the data, and it
must be named MBSR.data.csv.  This is to help you learn
how data in a 2 x 2 factorial design are typically
formatted for this type of ANOVA.\n\n",call.=FALSE)
    }
  }

  if(show.hist==TRUE){
    cat("\n_______________________________________________________________________\n")
    cat("\n
As noted in the instructions for this assignment, our sample size
in this hypothetical data set is very small.  The histogram and normal
quantile plot are displayed here only to initialize your plot frame.\n")
    cat("\n_______________________________________________________________________\n\n")

   hist(x$anxiety,col="tan",main="Histogram",xlab="Anxiety")
   qqnorm(x$anxiety,main="Normal Quantile Plot")
  }

  dout <- describeBy(x,list(x$gender,x$condition),mat=TRUE)
  dout <- dout[-c(1:8),c(2,3,5,6,7)]
  variance <- round(dout$sd^2,3)
  se <- dout$sd/sqrt(dout$n)
  dout$sd <- round(dout$sd,2)
  dout2 <- data.frame(gender=dout$group1,condition=dout$group2,n=dout$n,
                      mean=dout$mean,sd=dout$sd,variance=variance,se=se)

  anova.result <- aov(anxiety ~ gender * condition,data=x)

  if(show.summary==TRUE){
   cat("\n__________________________________________________________________\n")

   cat("\nData:\n\n")
   print(x)
   cat("\n")

   cat("\nSummary of Data, by Gender and Condition:\n\n")
   print(dout2)

   cat("\n__________________________________________________________________\n")
  }

  ylimits <- c(10,22)

  if(plot.main.gender==TRUE){
    ym <- tapply(x$anxiety,INDEX=x$gender,FUN="mean")
    ysd <- tapply(x$anxiety,INDEX=x$gender,FUN="sd")
    yn <- tapply(x$anxiety,INDEX=x$gender,FUN="length")
    var.pool <- dout2$variance
    sd.pool <- sqrt(mean(var.pool))
    yse <- rep(sd.pool,2)/sqrt(yn)
    xt <- cbind(ym,ysd,yn,yse)
    xcor <- c(1,2)

    ylo <- ym - 2*yse
    yhi <- ym + 2*yse

    plot(xcor,ym,type="n",main="Gender Main Effect",xlab="",ylab="Mean Anxiety",xaxt="n",
         ylim=ylimits,xlim=c(.75,2.25))
    axis(at=xcor,labels=c("Female","Male"),side=1)
    points(xcor,ym,pch=19,cex=1.5)
    lines(xcor,ym,pch=19,col="black",lty=1,lwd=2)
    segments(xcor,ylo,xcor,yhi,lty=1,lwd=1)

  }

  if(plot.main.condition==TRUE){

    ym <- tapply(x$anxiety,INDEX=x$condition,FUN="mean")
    ysd <- tapply(x$anxiety,INDEX=x$condition,FUN="sd")
    yn <- tapply(x$anxiety,INDEX=x$condition,FUN="length")
    var.pool <- dout2$variance
    sd.pool <- sqrt(mean(var.pool))
    yse <- rep(sd.pool,2)/sqrt(yn)
    xt <- cbind(ym,ysd,yn,yse)

    xcor <- c(1,2)
    ylo <- ym - 2*yse
    yhi <- ym + 2*yse

    plot(xcor,ym,type="n",main="Treatment Main Effect",xlab="",ylab="Mean Anxiety",xaxt="n",
         ylim=ylimits,xlim=c(.75,2.25))
    axis(at=xcor,labels=c("Control","MBSR"),side=1)
    points(xcor,ym,pch=19,cex=1.5)
    lines(xcor,ym,pch=19,col="black",lty=1,lwd=2)
    segments(xcor,ylo,xcor,yhi,lty=1,lwd=1)
  }

  if(plot.interaction==TRUE){

    ylimits <- c(5,30)
    xcor <- c(1,2)
    IM.f <-dout2[dout2$gender=="F",]
    ym.f <- IM.f$mean
    se.f <- IM.f$se
    ylo.f <- ym.f - 2*se.f
    yhi.f <- ym.f + 2*se.f

    IM.m <-dout2[dout2$gender=="M",]
    ym.m <- IM.m$mean
    se.m <- IM.m$se
    ylo.m <- ym.m - 2*se.m
    yhi.m <- ym.m + 2*se.m

    plot(xcor,c(10,20),type="n",main="Gender x Treatment",xlab="",ylab="Mean Anxiety",xaxt="n",
         ylim=ylimits,xlim=c(.75,2.25))
    axis(at=xcor,labels=c("Control","MBSR"),side=1)
    points(xcor,ym.f,pch=19,cex=1.5)
    lines(xcor,ym.f,pch=19,col="black",lty=1,lwd=2)
    segments(xcor,ylo.f,xcor,yhi.f,lty=1,lwd=1)
    points(xcor,ym.m,pch=19,cex=1.5)
    lines(xcor,ym.m,pch=19,col="red",lty=3,lwd=2)
    segments(xcor,ylo.m,xcor,yhi.m,lty=1,lwd=1)

    text(2.1,ym.m[2],"M")
    text(2.1,ym.f[2],"F")
  }

 if(show.anova==TRUE){
   cat("\n__________________________________________________________________\n")
   cat("\nSummary of Analysis of Variance Model:\n\n")
   print(summary(anova.result))
   cat("\n__________________________________________________________________\n")
 }

  if(show.tukey==TRUE){
    cat("\n__________________________________________________________________\n\n")
    print(TukeyHSD(anova.result,which="gender:condition"))
    cat("Comparisons with /p adj/ < .05 exceed critical HSD for
experiment-wise alpha = .05.\n\n")
    cat("__________________________________________________________________\n\n")
  }

}
