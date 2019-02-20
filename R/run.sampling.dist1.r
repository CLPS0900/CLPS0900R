#' Demonstration of sampling distribution of the mean
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' run.sampling.dist1()
#' }
#'
#' @export
run.sampling.dist1 <- function() {

  #################################################################
  #manipulate sampling distributions
  #################################################################

  par(mfrow=c(2,2))
  par(mai=c(.7,.5,.7,.1))

  manipulate(samp.dist1(ssize=ssize,
                        pop.type=pop.type,
                        add.normal=add.normal),
             ssize=slider(1,100,step=1,initial=1,label="Sample Size (N)"),
             pop.type=picker("Uniform[1-6]","Uniform[1-10]","Binomial","Normal (IQ)",
                             "Skewed","Bimodal","World IPP","US Violent Crime",
                             "Custom",label="Population Type"),
             add.normal=checkbox(label="Add Normal Distribution")
  )
}
