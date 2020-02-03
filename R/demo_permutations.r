#' Demonstration means and variances using permutations
#'
#' @param population A vector of values to be sampled from
#' @param sample.size A number (2-10) for the number of observations to be drawn
#' @param pop.mu A value for population true mean
#' @param pop.sigma2 A value for population true variance
#' @param show.summary Controls display of summary statistics (TRUE/FALSE)
#' @param show.plots Controls whether plots are shown
#' @param show.details Controls whether permutations are shown
#'
#' @return
#'  ratio of estimated variance/true variance
#'
#' @examples
#' \dontrun{
#' demo_permutations(population=c(1:3),sample.size=2)
#' }
#' @export
demo_permutations <- function(population=c(1:3),
                               sample.size=2,
                               pop.mu=NULL,pop.sigma2=NULL,
                               show.summary=TRUE,
                               show.plots=TRUE,
                               show.details=TRUE){

cat("\n
*******************************************************************************
This function allows you to recreate the demonstrations shown in class of the
sampling distributions of the mean and variance.  You will define the values
that comprise your population and your sample size, ie, the number of
observations you want to draw from that population. You will be shown the
sampling distribution of the mean and variance.
*******************************************************************************\n\n")

#packages required:

   #install.packages("gtools") #new for this problem set
   #library(gtools)
   #library(mosaic)
   #library(psych)

pop.size <- length(population)
pop.mu <- mean(population)

if(sample.size < 2){
  stop("Sorry, the variance of a sample is undefined if you
       only have 1 observation.  Try a sample size between 2-10.")
}

if(sample.size > 10){
  stop("Sorry, your sample size would require more memory
       than R has allocated to your workspace. Try a sample size between 2-10.")
}

if(is.null(pop.mu)){
  pop.mu <- mean(population)
}

if(is.null(pop.sigma2)){
  pop.sigma2 <-  sum((population-pop.mu)^2)/(pop.size)  #force parasample.meanseter using N, not N-1
}

samples <- permutations(n=pop.size,r=sample.size,v=population,repeats.allowed=TRUE)
ntrials <- dim(samples)[[1]]
trials <- c(1:ntrials)

sample.means <- rowMeans(samples)
squared.deviations <- (samples - sample.means)^2
sum.sq <- rowSums(squared.deviations)
var.biased <- sum.sq/sample.size
var.unbiased <- sum.sq/(sample.size - 1)

long.run.mean <- mean(sample.means)
long.run.var.biased <- mean(var.biased)
long.run.var.unbiased <- mean(var.unbiased)
fraction.of.sigma2 <- long.run.var.biased/pop.sigma2

my.work <- data.frame(samples,sample.means,
                      squared.deviations,sum.sq,var.biased,var.unbiased)
summary.work <- data.frame(pop.mu,pop.sigma2,long.run.mean,long.run.var.biased,long.run.var.unbiased,
                           fraction.of.sigma2)

my.work <- round(my.work,2)
summary.work <- round(summary.work,2)


obs.names <- paste("Obs",1:sample.size,sep="")
dev.names <- paste("Dev",1:sample.size,sep="")

vnames <- c(obs.names,"Means",dev.names,"SUM.SQ","var(N)","var(N-1)")
dimnames(my.work) <- list(trials,vnames)

if(show.plots==TRUE){

  par(mfrow=c(1,2))
  if(sample.size==2 & round(pop.sigma2,2)==.67){
    cbreaks1 <- c(.75,1.25,1.75,2.25,2.75,3.25)
    cbreaks2 <- c(0,.2,.4,.8,1,1.2)
  }
  else{
    cbreaks1 <- NA; cbreaks2 <- NA
  }
  if(is.na(cbreaks1[1])){
   hist(sample.means,col="tan",main="",xlab="Sample Mean")
  }
  else {
    hist(sample.means,col="tan",main="",xlab="Sample Mean",breaks=cbreaks1)
  }
  mtext(side=3,text="Distribution of Sample Means",cex=1,line=1)
  abline(v=c(pop.mu,long.run.mean),col=c("red","blue"),lwd=3,lty=c(1,2))

  if(is.na(cbreaks1[1])){
   hist(var.biased,col="tan",main="",xlab="Sample Variance (using N)")
  }
  else {
    hist(var.biased,col="tan",main="",xlab="Sample Variance (using N)",breaks=cbreaks2)
  }
  mtext(side=3,text="Distribution of Sample Variances",cex=1,line=1)
  abline(v=c(pop.sigma2,long.run.var.biased),col=c("red","blue"),lwd=3,lty=c(1,2))
}

if(show.details==TRUE){
  cat("\n###################################################")

  cat("\n\nPermutations:\n\n")
  print(my.work)
}

if(show.summary==TRUE){

 cat("\n###################################################")

 cat("\n\nSummary of population, parameters and estimators:\n")

 cat("\nPopulation: ",population)
 cat("\nSample size (N): ",sample.size)
 cat("\nNumber of permutations:",ntrials,"\n")

 summary.stats <- round(colMeans(my.work),2)

 long.run.stats <- matrix(c(pop.mu,pop.sigma2,
                    long.run.mean,
                    long.run.var.biased,long.run.var.unbiased,
                    fraction.of.sigma2))
 long.run.stats <- round(long.run.stats,3)
 rnames <- c("pop.mu","pop.sigma2","long.run.mean","long.run.var.biased",
            "long.run.var.unbiased","fraction.of.sigma2")
 cnames <- ""
 dimnames(long.run.stats) <- list(rnames,cnames)
 print(long.run.stats)

 cat("\n###################################################\n\n")

}

fraction.of.sigma2

}



