whew1 <- function(do.desc=FALSE,do.hist=TRUE,do.scatter=FALSE,do.corr.life=FALSE,
                  do.corr.happ=FALSE,
                  pause=FALSE,model.type=0,
                  show.lm.summary=FALSE){

  ############################################################
  #model healthy life expectancy, human development index
  #and alcohol consumption
  ############################################################

  #require(psych)
  #require(mosaic)
  #require(manipulate)

  cat("\014")

  x <- WHEW_DATA #see merge for cleaning steps
  x <- x[,-c(1,2)]

  if(do.scatter==TRUE | do.corr.life ==TRUE | do.corr.happ == TRUE | model.type != "None"){
    do.hist <- FALSE
  }

  if(model.type != "None"){
    do.scatter <- FALSE
    do.corr.life <- FALSE
    do.corr.happ <- FALSE
    do.hist <- FALSE
  }


  ############################################################
  #hist to initialize mplot display
  if(do.hist==TRUE){
   par(mfrow=c(2,2));par(mai=c(1,.8,.5,.4));par(cex=1)
   hist(WHEW_DATA$HLifeExp,col="tan",xlab="Healthy Life Expectancy",main="Histogram of HLifExp")
   hist(WHEW_DATA$Education,col="cadetblue",xlab="Education (years)",main="Hisogram of Education")
   hist(WHEW_DATA$GDP.pp,col="cornsilk4",xlab="GDP Per Person (x10k)",main="Histogram of GDP Per Person")
   #qqnorm(WHEW_DATA$HLifeExp)
  }


  ############################################################
  #descriptives

  if(do.desc==TRUE){
   dout <- describe(x[,c(1,2,3,6,7)])
   cat("\n________________________________________________________________\n\n")
   cat("Descriptive statistics:\n\n")
   print(dout)
   cat("\n________________________________________________________________\n\n")
  }


  ############################################################
  #scatterplots

  if(do.scatter==TRUE){
    #pairs(~Education + GDP.pp + HLifeExp,data=x,main="Pairwise Scatterplots")
    #if(pause==TRUE){readline("Press enter to continue...")}

    pairs.panels(x[,c(2,3,1)],smooth=FALSE,ellipses=FALSE,hist.col="tan",cex.cor=.75,pch=19,cex=1)
    if(pause==TRUE){readline("Press enter to continue...")}
  }

  ############################################################
  #correlations

  vnames <- dimnames(x)[[2]]

  if(do.corr.life==TRUE){
   r <- round(cor(x[,c(2,3,1)],use="complete"),2)
   cat("\n________________________________________________________________\n\n")
   cat("Correlation Matrix:\n\n")
   print(r)
   cat("\n________________________________________________________________\n\n")
   if(pause==TRUE){readline("Press enter to continue...")}
  }

  ############################################################
  #correlations

  vnames <- dimnames(x)[[2]]

  if(do.corr.happ==TRUE){
    r <- round(cor(x[,c(1,2,3,6,7)],use="complete"),2)
    cat("\n________________________________________________________________\n\n")
    cat("Correlation Matrix:\n\n")
    print(r)
    cat("\n________________________________________________________________\n\n")
    if(pause==TRUE){readline("Press enter to continue...")}
  }

  ############################################################
  #regressions

  if(model.type=="Life ~ Education"){

   #Hlife ~ predictors

   pout <- ggplot(data = WHEW_DATA,
                 aes(x = Education, y = HLifeExp)) +
                         geom_point(colour="darkblue",size=2)   +
                         stat_smooth(method = lm,colour="red",fill="tan",linetype = "dashed") +
                         labs(title = model.type) +
                         scale_x_continuous(breaks=seq(0,20,1)) +
                         theme(panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               panel.background = element_blank(),
                               axis.line = element_line(colour = "black"),
                               plot.margin=unit(c(1,1,1,1),"cm"),
                               text = element_text(size=15)
                               )
   print(pout)

   lmout <- lm(HLifeExp ~ Education,data=x)

   if(show.lm.summary==TRUE){
    cat("\n________________________________________________________________\n\n")
    cat("\nRegression:  Healthy Life Expectancy ~ Education\n")
    print(summary(lmout))
    if(pause==TRUE){readline("Press enter to continue...")}
    cat("\n________________________________________________________________\n\n")
   }

  }

  if(model.type=="Life ~ Education + GDP"){

    #Hlife ~ predictors

    pout <- ggplot(data = WHEW_DATA,
                   aes(x = Education, y = HLifeExp)) +
      aes(colour=GDP.groups) +
      geom_point(size=2)   +
      stat_smooth(method = lm, fill="tan",linetype = "dashed") +
      labs(title = "Life ~ Education | GDP Group") +
      scale_x_continuous(breaks=seq(0,20,1)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            plot.margin=unit(c(1,1,1,1),"cm"),
            legend.position = c(.1,.85),
            text = element_text(size=15)
      )

    print(pout)

    lmout <- lm(HLifeExp ~ Education + GDP.pp,data=x)

    if(show.lm.summary==TRUE){
    cat("\n________________________________________________________________\n\n")
    cat("\nRegression:  Healthy Life Expectancy ~ Education + GDP.pp\n")
    print(summary(lmout))
    if(pause==TRUE){readline("Press enter to continue...")}
    cat("\n________________________________________________________________\n\n")
    }

  }

  if(model.type=="Life ~ Education * GDP"){

    #Hlife ~ predictors , with interactions
    #
    pout <- ggplot(data = WHEW_DATA,
                   aes(x = Education, y = HLifeExp)) +
      aes(colour=GDP.groups) +
      geom_point(size=2)   +
      stat_smooth(method = lm, fill="tan",linetype = "dashed") +
      labs(title = "Life ~ Education | GDP Group") +
      scale_x_continuous(breaks=seq(0,20,1)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            plot.margin=unit(c(1,1,1,1),"cm"),
            legend.position = c(.1,.85),
            text = element_text(size=15)
      )

    print(pout)


    lmout <- lm(HLifeExp ~ Education * GDP.pp,data=x)
    if(show.lm.summary==TRUE){
    cat("\n________________________________________________________________\n\n")
    cat("\nRegression:  Healthy Life Expectancy ~ Education X GDP.pp\n")
    print(summary(lmout))
    if(pause==TRUE){readline("Press enter to continue...")}
    cat("\n________________________________________________________________\n\n")
    }
  }

  if(model.type=="Life ~ Education + log(GDP)"){

    #Hlife ~ predictors, with log for GDP
    xx <- WHEW_DATA
    xx$logGDP <- log(xx$GDP.pp)

    pairs.panels(xx[,c(4,11,3)],smooth=FALSE,ellipses=FALSE,hist.col="tan",cex.cor=.75,pch=19,cex=1)

    lmout <- lm(HLifeExp ~ Education + logGDP,data=xx)
    if(show.lm.summary==TRUE){
    cat("\n________________________________________________________________\n\n")
    cat("\nRegression:  Healthy Life Expectancy ~ Education + log(GDP.pp)\n")
    print(summary(lmout))
    if(pause==TRUE){readline("Press enter to continue...")}
    cat("\n________________________________________________________________\n\n")
    }
  }

  if(model.type=="Life ~ Education * log(GDP)"){

    #Hlife ~ predictors, with log for GDP
    xx <- WHEW_DATA
    xx$logGDP <- log(xx$GDP.pp)

    print(dimnames(xx)[[2]])
    pairs.panels(xx[,c(4,11,3)],smooth=FALSE,ellipses=FALSE,hist.col="tan",cex.cor=.75,pch=19,cex=1)

    lmout <- lm(HLifeExp ~ Education * logGDP,data=xx)
    if(show.lm.summary==TRUE){
    cat("\n________________________________________________________________\n\n")
    cat("\nRegression:  Healthy Life Expectancy ~ Education * GDP.pp\n")
    print(summary(lmout))
    if(pause==TRUE){readline("Press enter to continue...")}
    cat("\n________________________________________________________________\n\n")
    }
  }


  if(model.type=="Happiness ~ Education"){

    #Hlife ~ predictors

    pout <- ggplot(data = WHEW_DATA,
                   aes(x = Education, y = Happiness)) +
      #aes(colour=GDP.groups) +
      geom_point(size=2)   +
      stat_smooth(method = lm, fill="tan",linetype = "dashed") +
      labs(title = "Happiness ~ Education") +
      scale_x_continuous(breaks=seq(0,20,1)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            plot.margin=unit(c(1,1,1,1),"cm"),
            legend.position = c(.1,.85),
            text = element_text(size=15)
      )

    print(pout)

    lmout <- lm(Happiness ~ Education,data=x)
    if(show.lm.summary==TRUE){
      cat("\n________________________________________________________________\n\n")
      cat("\nRegression:  Happiness ~ Education\n")
      print(summary(lmout))
      if(pause==TRUE){readline("Press enter to continue...")}
      cat("\n________________________________________________________________\n\n")
    }

  }

  if(model.type=="Happiness ~ Education + GDP"){

    #Hlife ~ predictors

    pout <- ggplot(data = WHEW_DATA,
                   aes(x = Education, y = Happiness)) +
      aes(colour=GDP.groups) +
      geom_point(size=2)   +
      stat_smooth(method = lm, fill="tan",linetype = "dashed") +
      labs(title = "Happiness ~ Education | GDP Group") +
      scale_x_continuous(breaks=seq(0,20,1)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            plot.margin=unit(c(1,1,1,1),"cm"),
            legend.position = c(.1,.85),
            text = element_text(size=15)
      )

    print(pout)

    lmout <- lm(Happiness ~ Education + GDP.pp,data=x)
    if(show.lm.summary==TRUE){
    cat("\n________________________________________________________________\n\n")
    cat("\nRegression:  Happiness ~ Education + GDP.pp\n")
    print(summary(lmout))
    if(pause==TRUE){readline("Press enter to continue...")}
    cat("\n________________________________________________________________\n\n")
    }
  }

  if(model.type=="Happiness ~ Education * GDP"){

    #Hlife ~ predictors

    pout <- ggplot(data = WHEW_DATA,
                   aes(x = Education, y = Happiness)) +
      aes(colour=GDP.groups) +
      geom_point(size=2)   +
      stat_smooth(method = lm, fill="tan",linetype = "dashed") +
      labs(title = "Happiness ~ Education | GDP Group") +
      scale_x_continuous(breaks=seq(0,20,1)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            plot.margin=unit(c(1,1,1,1),"cm"),
            legend.position = c(.1,.85),
            text = element_text(size=15)
      )

    print(pout)

    lmout <- lm(Happiness ~ Education * GDP.pp,data=x)
    if(show.lm.summary==TRUE){
    cat("\n________________________________________________________________\n\n")
    cat("\nRegression:  Happiness ~ Education * GDP.pp\n")
    print(summary(lmout))
    if(pause==TRUE){readline("Press enter to continue...")}
    cat("\n________________________________________________________________\n\n")
    }
  }


  if(model.type=="Happiness ~ Alcohol"){

    #Hlife ~ predictors

    pout <- ggplot(data = WHEW_DATA,
                   aes(x = TAC, y = Happiness)) +
      #aes(colour=GDP.groups) +
      geom_point(size=2)   +
      stat_smooth(method = lm, fill="tan",linetype = "dashed") +
      labs(title = "Happiness ~ Alcohol") +
      scale_x_continuous(breaks=seq(0,20,1)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            plot.margin=unit(c(1,1,1,1),"cm"),
            legend.position = c(.1,.85),
            text = element_text(size=15)
      )

    print(pout)

    lmout <- lm(Happiness ~ TAC,data=x)
    if(show.lm.summary==TRUE){
    cat("\n________________________________________________________________\n\n")
    cat("\nRegression:  Happiness ~ Alcohol\n")
    print(summary(lmout))
    if(pause==TRUE){readline("Press enter to continue...")}
    cat("\n________________________________________________________________\n\n")
    }
  }

  if(model.type=="Happiness ~ Alcohol + GDP"){

    #Hlife ~ predictors

    pout <- ggplot(data = WHEW_DATA,
                   aes(x = TAC, y = Happiness)) +
      aes(colour=GDP.groups) +
      geom_point(size=2)   +
      stat_smooth(method = lm, fill="tan",linetype = "dashed") +
      labs(title = "Happiness ~ Alcohol | GDP Group") +
      scale_x_continuous(breaks=seq(0,20,1)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            plot.margin=unit(c(1,1,1,1),"cm"),
            legend.position = c(.1,.85),
            text = element_text(size=15)
      )

    print(pout)

    lmout <- lm(Happiness ~ TAC + GDP.pp,data=x)
    if(show.lm.summary==TRUE){
    cat("\n________________________________________________________________\n\n")
    cat("\nRegression:  Happiness ~ Alcohol + GDP.pp\n")
    print(summary(lmout))
    if(pause==TRUE){readline("Press enter to continue...")}
    cat("\n________________________________________________________________\n\n")
    }
  }

  if(model.type=="Happiness ~ Alcohol * GDP"){

    #Hlife ~ predictors

    pout <- ggplot(data = WHEW_DATA,
                   aes(x = TAC, y = Happiness)) +
      aes(colour=GDP.groups) +
      geom_point(size=2)   +
      stat_smooth(method = lm, fill="tan",linetype = "dashed") +
      labs(title = "Happiness ~ Alcohol | GDP Group") +
      scale_x_continuous(breaks=seq(0,20,1)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            plot.margin=unit(c(1,1,1,1),"cm"),
            legend.position = c(.1,.85),
            text = element_text(size=15)
      )

    print(pout)

    lmout <- lm(Happiness ~ TAC * GDP.pp,data=x)
    if(show.lm.summary==TRUE){
    cat("\n________________________________________________________________\n\n")
    cat("\nRegression:  Happiness ~ Alcohol * GDP.pp\n")
    print(summary(lmout))
    if(pause==TRUE){readline("Press enter to continue...")}
    cat("\n________________________________________________________________\n\n")
    }
  }





}
