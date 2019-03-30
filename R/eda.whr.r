#' Perform one-way anova and related methods using WHR_DATA.
#' Typically called by run_whr()
#'
#' @param dv A continuous quantitative dependent variable
#' @param iv A categorical variable using as the independent variable
#' @param f1.name The name of the dv
#' @param f2.name The name of the iv
#' @param add.normal Logical. Controls display of normal curve on histogram.
#' @param add.qnorm Logical. Controls display of normal quantile plot
#' @param show.aov Logical. controls display of ANOVA model
#' @param show.tukey Logical. controls display of Tukeys HSD for pairwise comparisons.
#' @param show.barplot Controls barplot + CIs of means
#' @param conf.level Confidence level used for barplot + CIs
#' @param show.boxplots Controls display of boxplots
#' @param n.int number of intervals used in histogram
#' @param adj.barlimits Controls limits for y axis in barplot
#' @param show.stats Controls display of descriptive statistics
#'
#' @return
#' none
#'
#' @examples
#' \dontrun{
#' eda.whr()
#' }
#' @export
eda.whr <- function(dv,iv,f1.name,f2.name,add.normal=FALSE,add.qnorm=FALSE,
                    show.aov=FALSE,show.tukey=FALSE,show.barplot=FALSE,
                    conf.level=.95,show.boxplots=FALSE,n.int=10,
                    adj.barlimits=FALSE,show.stats=FALSE){

  par(mfrow=c(2,2))

  gnames <- c("V-Low","Low","High","V-High")
  p.colors <- c("tan","cadetblue","antiquewhite3")

  x <- data.frame(dv=dv,Groups=as.factor(iv))
  x <- x[!is.na(dv) & !is.na(iv),]

  hout <- hist(x$dv,breaks=n.int,plot=FALSE)
  hout <- hout$density

  ylo <- 0
  yhi <- max(hout) + .1*(max(hout)-min(hout))
  ylimits <- c(ylo,yhi)

  hist(x$dv,col=p.colors[1],prob=TRUE,main="",xlab="Value",breaks=n.int ,ylim=ylimits)
  hname <- paste("Histogram of",f1.name)
  mtext(side=3,line=1,text=hname,cex=1)


  if(add.normal==TRUE){
    xm <- mean(x$dv)
    xsd <- sd(x$dv)
    curve(dnorm(x,mean=xm,sd=xsd),col="red",lwd=2,add=TRUE)
  }

  if(add.qnorm==TRUE){
   qqnorm(x$dv,main="",xlab="Expected Z-scores",ylab="Observed Values")
   qname <- paste("Quantile Plot for",f1.name)
   mtext(side=3,line=1,text=qname,cex=1)
  }

  xm <- tapply(x$dv,x$Groups,"mean")
  xsd <- tapply(x$dv,x$Groups,"sd")
  xvar <- xsd^2
  xn <- tapply(x$dv,x$Groups,"length")
  xdf <- xn-1
  xse <- xsd/sqrt(xn)
  ss <- sum(xvar*xdf)
  msb <- var(xm)*mean(xn)
  msw <- ss/sum(xdf)
  s.pooled <- sqrt(msw)

  if(show.boxplots==TRUE){

    if(f2.name!="region.broad"){
    cen <- boxplot(dv ~ Groups,data=x,col=p.colors[2],xlab="",ylab=f1.name,
                   main="",names=gnames)
    }
    else {
      cen <- boxplot(dv ~ Groups,data=x,col=p.colors[2],xlab="",ylab=f1.name,
                     main="")
    }

    bpname <- paste("Boxplots:",f1.name,"~",f2.name)
    mtext(side=3,line=1,text=bpname,cex=1)
    mtext(side=1,line=2.5,text=f2.name,cex=1)
  }


  d12 <- (xm[2]-xm[1])/s.pooled
  d23 <- (xm[3]-xm[2])/s.pooled
  d34 <- (xm[4]-xm[3])/s.pooled

  cohens.d <- c(d12,d23,d34)
  gdp.d1 <- 7862-1891
  gdp.d2 <- 16852 - 7862
  gdp.d3 <- 47142-16852
  gdp.diff <- c(gdp.d1,gdp.d2,gdp.d3)


  #print(cohens.d)

  dfb <- length(xm)-1
  dfw <- sum(xdf)
  f.obs <- msb/msw

  ci.lo <- xm - xse * qt((conf.level)/2 + .5, xdf)
  ci.hi <- xm + xse * qt((conf.level)/2 + .5, xdf)

  my_sum <- data.frame(groups=gnames,mean=xm,sd=xsd,var=xvar,n=xn,se=xse,ci.lo=ci.lo,ci.hi=ci.hi)

  ylo <- min(ci.lo) - .2*(max(ci.hi)-min(ci.lo))
  yhi <- max(ci.hi) + .2*(max(ci.hi)-min(ci.lo))

  if(adj.barlimits==TRUE){
   ylimits <- c(ylo,yhi)
  }
  else {
   ylimits <- c(0,max(dv))
  }
  bnames <- sort(unique(x$Groups))

  if(show.barplot==TRUE){

  if(f2.name!="region.broad"){
   cen <- barplot(my_sum$mean,col=p.colors[3],xlab="",ylim=ylimits,names=gnames,xpd=FALSE)
  }
  else {
   cen <- barplot(my_sum$mean,col=p.colors[3],xlab="",ylim=ylimits,xpd=FALSE)
  }
    abline(h=ylimits[1])
   segments(cen,ci.lo,cen,ci.hi,lwd=1,col="black")
   mtext(side=1,text=f2.name,line=2.25,cex=1)
   mtext(side=2,text="Mean",line=2.2,cex=1)
   mtext(side=3,text=paste("Barplots:",f1.name,"~",f2.name),cex=1,line=1)
   abline(h=0)
  }

  if(show.stats==TRUE){

    my_sum[,-1] <- round(my_sum[,-1],4)

    cat("\n__________________________________________________________________\n\n")

    cat("Summary statistics: ")
    cat(f1.name,"~",f2.name,"\n")
    cat("\nConfidence level for CIs:",conf.level,"\n\n")
    print(my_sum)
    cat("\n__________________________________________________________________\n")

  }

  aov.out <- aov(dv ~ Groups,data=x)

  if(show.aov==TRUE){
    cat("\n__________________________________________________________________\n\n")
    cat("ANOVA model summary: ")
    cat(f1.name,"~",f2.name,"\n")
    cat("\n")
    print(summary(aov.out))
    cat("\n__________________________________________________________________\n")
  }

  if(show.tukey){

    cat("\n__________________________________________________________________\n\n")
    cat("Tukey HSD Comparisons: ")
    cat(f1.name,"~",f2.name,"\n")
    cat("\nFamily Wise adjusted alpha:",1-.95)
    cat("\n\n")

    hsd <- TukeyHSD(aov.out, "Groups", ordered = FALSE,conf.level=.95)
    hsd <- hsd$Groups
    pv <- hsd[,4]
    pv[pv <= .05] <- "<.05"
    pv[pv >  .05] <- "ns"

    hsd <- data.frame(round(hsd[,1:3],3),p=pv)
    print(hsd)
        cat("\n__________________________________________________________________\n")
  }

}
