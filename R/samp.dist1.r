#' Demonstration of sampling distribution of the mean
#'
#' @param ssize An integer for the size of the sample
#' @param pop.type Controls the type of population from which samples are drawn
#' @param add.normal #controls whether normal distribution is superimposed
#' @param custom.data User supplied vector of data to be used
#' @param show.pop display population
#' @param show.flex show sampling distribution with flexible x axis
#' @param show.fixed show sampling distribution with fixed x axis
#' @param show.qnorm show normal quantile plot
#'
#' @return None
#'
#' @examples
#' samp.dist1()
#'
#' @export
samp.dist1 <- function(ssize=5,pop.type="Uniform[1-10]",add.normal=TRUE,custom.data=NULL,
                       show.pop=TRUE,show.flex=TRUE,show.fixed=TRUE,show.qnorm=TRUE){

  #################################################################
  #sampling distributions of the mean
  #################################################################

  #cosmetics

  lwd.setting <- 3
  font.size3 <- 1.25
  font.size5 <- 1.25

  #define populations

  if(pop.type=="Uniform[1-10]"){
    x <- c(1:10)
  }
  if(pop.type=="Uniform[1-6]"){
    x <- c(1:6)
  }
  if(pop.type=="Binomial"){
    x <- c(0,1)
  }
  if(pop.type=="WAIS IQ"){
    mu.pop <- 100; sd.pop <- 15
    x <- rnorm(10000,mu.pop,sd.pop)
  }
  if(pop.type=="Old Faithful"){
    mean1 <- 60
    mean2 <- 140
    sd.pop <- 15
    x1 <- rnorm(5000,mean1,sd.pop)
    x2 <- rnorm(5000,mean2,sd.pop)
    x <- c(x1,x2)
    mu.pop <- mean(c(mean1,mean2))

    x <- faithful$eruptions
    mu.pop <- mean(x)

  }
  if(pop.type=="Sunspot Activity"){
    x <- scale(sunspots^2)*15
    x <- x + abs(min(x))
    x <- sunspots
  }

  if(pop.type=="World IPP"){
    x <- WHO_DATA$IPP
  }
  if(pop.type=="US Violent Crime"){
    x <- states_data$violent.crime
  }
  if(pop.type=="World Happiness Report"){
    x <- WHR_DATA$happiness
  }

  if(pop.type=="Custom"){
    if(is.null(custom.data)){
      cat("\nError: You must supply the custom.data to be used.\n\n")
    }
    x <- custom.data
  }

  nsamp <- 2500
  nc <- 20

  n <- length(x)

  if(pop.type != "Normal" & pop.type!="Bimodal"){
    mu.pop <- round(mean(x,na.rm=TRUE),2)
    sd.pop <- round(sdpop(x),2)
  }


  ###################################
  #display population

  main.text <- paste("Population\n", "mu=", mu.pop, "sd.pop=", sd.pop)

  x.lo <- mu.pop - 4 * sd.pop
  x.hi <- mu.pop + 4 * sd.pop
  xlimits.fixed <- c(x.lo,x.hi)

  if(show.pop==TRUE){

  if(pop.type=="Uniform[1-6]"){
    hist(x, probability = T, ylab = "Relative Frequency", main = "",
         include.lowest = T, xlab = "",xlim=xlimits.fixed,col="tan",
         breaks=c(0.99,1.99,2.99,3.99,4.99,5.99,6.99))
    mtext(main.text,line=1,cex=font.size5)
  }
  else {
   hist(x, probability = T, ylab = "Relative Frequency", main = "",
       include.lowest = T, xlab = "",xlim=xlimits.fixed,col="tan")
   mtext(main.text,line=1,cex=font.size5)
  }

  }

  ###################################
  #get samples

  smeans <- rep(NA, nsamp)

  for(sn in 1:nsamp) {
    if(ssize==1){
      smeans[sn] <- sample(x,1)
    }
    if(ssize > 1){
      xx <- sample(x,ssize,replace=TRUE)
      smeans[sn] <- mean(xx)
    }
  }

  m <- round(mean(smeans,na.rm=TRUE),2)
  smeans2 <- smeans[!is.na(smeans)]
  sd.samp <- round(sd(smeans2),2)  #here we use unbiased estimator, as we have random samples

  z <- seq(-4, 4, by = 0.1)
  q <- (z * sd.samp) + m

  ###################################
  #display sampling distribution

  main.text <- paste("Sampling Distribution n=", ssize, "\n", "M=", m, "s=", sd.samp,sep=" ")

  xlimits <- c(min(q), max(q))

  if(pop.type=="Uniform[1-6]" & ssize < 5){
    nct <- 10
  }
  else {
    nct <- nc
  }

  if(show.flex==TRUE){
   hist(smeans, main="",xlab = "", ylab = "", probability = T,
       xlim = xlimits, nclass = nct, yaxt = "n",col="tan")
   mtext(main.text,line=1,cex=font.size5)

   if(add.normal==TRUE){
     lines(q, dnorm(q, m, sd.samp), col = "red",lwd=lwd.setting)
   }
  }

  ###################################
  #again, using fixed xlimits

  xlimits <- xlimits.fixed

  if(show.fixed==TRUE){
   hist(smeans, main="",xlab = "Mean\n(original limits)", ylab = "",
        probability = T, xlim = xlimits, nclass = nct, yaxt = "n",col="tan")
   mtext(main.text,line=1,cex=font.size5)

   if(add.normal==TRUE){
    lines(q, dnorm(q, m, sd.samp), col = "red",lwd=lwd.setting)
   }
  }

  ###################################
  #show qqplot
  if(show.qnorm==TRUE){
   qout <- qqnorm(smeans,main="Normal Quantile Plot",xlab="Expected Z Scores",ylab="Observed Values")
   qqline(smeans,col="red",lwd=2)
  }

}
