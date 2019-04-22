#' manipulate whew1 for regression exercise using Health, Education, and GDP data
#'
#' @return
#' none
#'
#' @examples
#' \dontrun{
#' run.whew()
#' }
#' @export
#'
run.whew <- function(){manipulate(whew1(do.hist=do.hist,do.scatter=do.scatter,
                                        do.desc=do.desc,
                                        do.corr.life=do.corr.life,
                                        do.corr.happ=do.corr.happ,
                                        model.type=model.type,
                                        show.lm.summary=show.lm.summary),
                                     do.desc=checkbox(FALSE,"Descriptive Statistics"),
                                     do.hist=checkbox(TRUE,"Histogram of HLifeExp"),
                                     do.scatter=checkbox(FALSE,"Scatterplot + Histograms + Correlations"),
                                     do.corr.life=checkbox(FALSE,"Correlations with Life Exp."),
                                     do.corr.happ=checkbox(FALSE,"Correlations with Happiness"),
                                     model.type=picker("None","Life ~ Education","Life ~ Education + GDP",
                                                       "Life ~ Education * GDP","Life ~ Education + log(GDP)",
                                                       "Life ~ Education * log(GDP)",
                                                       "Happiness ~ Education",
                                                       "Happiness ~ Education + GDP",
                                                       "Happiness ~ Education * GDP",
                                                       "Happiness ~ Alcohol",
                                                       "Happiness ~ Alcohol + GDP",
                                                       "Happiness ~ Alcohol * GDP",
                                                       label="Regression"),
                                     show.lm.summary=checkbox(FALSE,"Show Regression Summary")

)

}
