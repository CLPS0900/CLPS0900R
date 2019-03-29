#' manipulate one-way anova and related methods using WHR_DATA
#'
#' @return
#' none
#'
#' @examples
#' \dontrun{
#' run.whr()
#' }
#' @export
run.whr <- function(){

  manipulate(eda.whr(dv=as.matrix(WHR_DATA[,factor1]),
                     iv=as.matrix(WHR_DATA[,factor2]),
                     f1.name=factor1,
                     f2.name=factor2,
                     add.normal=add.normal,
                     add.qnorm=add.qnorm,
                     show.boxplots=show.boxplots,
                     show.barplot=show.barplot,
                     show.aov=show.aov,
                     show.tukey=show.tukey,
                     adj.barlimits=adj.barlimits,
                     n.int=n.int,
                     show.stats=show.stats,
                     conf.level=conf.level),
                     factor1=picker("happiness","corruption","freedom","hlife.exp",
                                    "social.supp","generosity",label="Dependent Variable"),
                     factor2=picker("gdp.groups","corruption.groups",
                                    "freedom.groups","social.supp.groups",
                                    "generosity.groups","region.broad",
                                    label="Independent Variable"),
                     add.normal=checkbox(FALSE,"Superimpose Normal"),
                     add.qnorm=checkbox(FALSE,"Normal Quantile Plot"),
                     show.boxplots=checkbox(FALSE,"Show Boxplots"),
                     show.barplot=checkbox(FALSE,"Barplots + CIs"),
                     adj.barlimits=checkbox(FALSE,"Reset Y-axis"),
                     show.stats=checkbox(FALSE,"Show summary stats"),
                     show.aov=checkbox(FALSE,"Show ANOVA"),
                     show.tukey=checkbox(FALSE,"Tukey HSD"),
                     conf.level=slider(.68,.999,step=.01,initial=.95,label="Confidence level"),
                     n.int=slider(5,20,step=5,initial=5,label="# Intervals (Hist)")
             )


}
