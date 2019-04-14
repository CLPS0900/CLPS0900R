#' manipulate two-way anova and related methods using eda.anxiety
#'
#' @return
#' none
#'
#' @examples
#' \dontrun{
#' run.anxiety()
#' }
#' @export
#'
run.anxiety <- function(){manipulate(eda.anxiety(show.hist=show.hist,show.summary=show.summary,show.anova=show.anova,
                          plot.main.gender=plot.main.gender,plot.main.condition=plot.main.condition,
                          plot.interaction=plot.interaction,show.tukey=show.tukey),
                     show.hist=checkbox(TRUE,"Show Histogram"),
                     show.summary=checkbox(FALSE,"Show Summary Statistics"),
                     show.anova=checkbox(FALSE,"Show 2-way ANOVA"),
                     plot.main.gender=checkbox(FALSE,"Plot Gender Main Effect"),
                     plot.main.condition=checkbox(FALSE,"Plot Treatment Main Effect"),
                     plot.interaction=checkbox(FALSE,"Plot Interaction"),
                     show.tukey=checkbox(FALSE,"Show Tukey HSD")
             )

}
