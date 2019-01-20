#' Demonstration of 2-sample inference
#'
#' Demonstration on judging the difference between two independent samples,
#' as presented in class.  As shown in class, one part of the demonstration
#' asks you to judge the difference between the means for two treatment groups.
#' The second part of the demonstration illustrates the results we obtain
#' when the data are randomly shuffled over the two groups.
#' Note: On some platforms, the plot region may need to be initialized before
#' running the app.  This can be done with:
#' plot(1)
#'
#' @param demo.type a number (1=show actual groups 2=run random shuffling)
#' @param nreps a number (10-100) that controls number of replications
#' @param sleep.sec a number (0-3) that controls waiting between replications
#' @param add.diff number specifying the mean difference the two groups
#' @param show.ttest controls whether results of 2-sample t test is displayed.
#'
#' @return None
#'
#' @examples
#' demo_nhst(demo.type=1) #shows actual observations for 2 groups
#' demo_nhst(demo.type=2) #shows simulation of random shuffling
#'
#' @export
demo_nhst <- function(demo.type=1,nreps=100,sleep.sec=.2,add.diff=3.75,show.ttest=T){

  add.line <- T
  opt <- c("Show data for 2 actual groups","Run simulation of random shuffling")

  #get seed sample

  set.seed(9)
  x1 <- rnorm(100,100,10)
  x2 <- rnorm(100,100,10)
  x <- c(x1,x2)

  if(demo.type==1){
    nreps <- 1
    readline("Hit enter to view the actual data for the 2 treatment groups...")
  }

  mean.diffs <- rep(NA,nreps)

  #define difference for ttest purposes

  x2 <- x2 + add.diff

  if(show.ttest==T){
   print(t.test(x1,x2))
  }

  obs.diff <- mean(x2) - mean(x1)

  group.tags <- c(rep(1,100),rep(2,100))

   for(repnum in 1:nreps){

     #create 2 sets of y values to be used for plotting

     ycor1 <- rnorm(100,8,2) #group 1
     ycor2 <- rnorm(100,23,2) #group 2
     y <- c(ycor1,ycor2)

    #shuffle the groups

    if(demo.type==2){
      group.tags2 <- sample(group.tags,200,replace=F)
      x1 <- x[group.tags2==1]
      x2 <- x[group.tags2==2]
    }

   plot(x,y,yaxt="n",xlab="Memory Performance",main="Dotchart of Memory Performance",
       pch=19,xaxt="n",ylab="Group",type="n",ylim=c(0,30))

   par(cex=1.4)

   points(x1,ycor1,pch=19,col="red",cex=1)
   points(x2,ycor2,pch=15,col="blue",cex=1)

   abline(h=16)

   m1 <- mean(x1)
   m2 <- mean(x2)
   mean.diffs[repnum] <- m2 - m1

   if(add.line==T){
     if(nreps==1){
      readline("Hit enter to add lines at the mean for each group...")
     }
    abline(v=m1,col="red",lwd=3)
    abline(v=m2,col="blue",lwd=3)
   }

   Sys.sleep(sleep.sec)

  }

  if(nreps>1){

   #show hist of mean differences

   sem <- sd(mean.diffs)

   xlo <- mean(mean.diffs) - 4*sem
   xhi <- mean(mean.diffs) + 4*sem

   xrange <- c(xlo,xhi)

   readline("Hit enter to histogram of the mean differences between the simulated groups...")

   hist(mean.diffs,main="Histogram of Differences Between Means",
       xlab="Difference: Placebo - THC Condition",
       ylab="Frequency",
       xlim=xrange,
       col="lightblue",
       nclass=10
   )

   readline("Hit enter to show actual mean difference between the 2 treatments...")

   abline(v=obs.diff,col="red",lwd=4)

   #get fraction of outcomes >= the observed diff

   xx <- mean.diffs[mean.diffs >= obs.diff]
   prop.exceed <- length(xx)/nreps
   prop.exceed <- round(prop.exceed,4)

   cat("\nHit enter to show proportion of trials in which random shuffling produced")
   cat("\ndifferences larger than the actual difference between the 2 treatments...")
   readline()

   print("Percentage of cases >= observed difference")
   print(prop.exceed)

  }

}
