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

  #require(foreign)
  #require(phia)
  #require(FSA)

 if(is.null(x)){
  MBSR_DATA = read.csv("MBSR.data.csv")
  x <- MBSR_DATA
 }

  par(mfrow=c(2,2))
  par(mai=c(.7,.8,.5,.2))
  cat("\014")

  if(show.hist==TRUE){
   hist(x$anxiety,col="tan",main="Histogram",xlab="Anxiety")
   qqnorm(x$anxiety,main="Normal Quantile Plot")
  }

  Sum = Summarize(anxiety ~ gender * condition, data=x, digits=2)
  Sum$se = Sum$sd / sqrt(Sum$n)
  Sum$se = signif(Sum$se, digits=3)
  anova.result <- aov(anxiety ~ gender * condition,data=x)

  if(show.summary==TRUE){
   cat("\n__________________________________________________________________\n")

   cat("\nData:\n\n")
   print(x)
   cat("\n")

   cat("\nSummary of Data, by Gender and Condition:\n\n")
   print(Sum,digits=3)
   cat("\n__________________________________________________________________\n")
  }

  ylimits <- c(10,22)

  if(plot.main.gender==TRUE){
    IM = interactionMeans(anova.result,factors="gender")
    ym <- IM$adj
    se <- IM$std
    xcor <- c(1,2)
    ylo <- ym - 2*se
    yhi <- ym + 2*se

    plot(xcor,ym,type="n",main="Gender Main Effect",xlab="",ylab="Mean Anxiety",xaxt="n",
         ylim=ylimits,xlim=c(.75,2.25))
    axis(at=xcor,labels=c("Female","Male"),side=1)
    points(xcor,ym,pch=19,cex=1.5)
    lines(xcor,ym,pch=19,col="black",lty=1,lwd=2)
    segments(xcor,ylo,xcor,yhi,lty=1,lwd=1)
  }

  if(plot.main.condition==TRUE){
    IM = interactionMeans(anova.result,factors="condition")
    ym <- IM$adj
    se <- IM$std
    xcor <- c(1,2)
    ylo <- ym - 2*se
    yhi <- ym + 2*se

    plot(xcor,ym,type="n",main="Treatment Main Effect",xlab="",ylab="Mean Anxiety",xaxt="n",
         ylim=ylimits,xlim=c(.75,2.25))
    axis(at=xcor,labels=c("Control","MBSR"),side=1)
    points(xcor,ym,pch=19,cex=1.5)
    lines(xcor,ym,pch=19,col="black",lty=1,lwd=2)
    segments(xcor,ylo,xcor,yhi,lty=1,lwd=1)
  }

  if(plot.interaction==TRUE){

    ylimits <- c(5,30)

    IM = interactionMeans(anova.result)
    xcor <- c(1,2)

    IM.f <-IM[IM$gender=="F",]
    ym.f <- IM.f$adj
    se.f <- IM.f$std
    ylo.f <- ym.f - 2*se
    yhi.f <- ym.f + 2*se

    IM.m <-IM[IM$gender=="M",]
    ym.m <- IM.m$adj
    se.m <- IM.m$std
    ylo.m <- ym.m - 2*se
    yhi.m <- ym.m + 2*se

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
    cat("Comparisons with /p adj/ < .05 exceed HSD for alpha = .05.\n")
    cat("__________________________________________________________________\n\n")
  }

}
