#' Manipulate one-sample z test
#'
#' @param mu A value for assumed true mean of population
#' @param sigma A value for assume true sigma of population
#' @param n An integer for sample size
#' @param observed.mean A value for the observed sample size
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' run.onesample.ztest()
#' }
#'
#' @export
run.onesample.ztest <- function(mu=100,sigma=15,n=10,observed.mean=102){

  #################################################################
  #manipulate onesample.ztest
  #################################################################

  sem <- sigma/sqrt(n)
  mlo <- round((observed.mean - 5*sem),0)
  mhi <- round((observed.mean + 5*sem),0)

  manipulate(onesample.ztest(mu=mu,sigma=sigma,n=n,observed.mean=observed.mean,test.type=test.type),
    observed.mean=slider(mlo,mhi,initial=observed.mean,step=.1),
    n=slider(2,250,initial=n),
    test.type=picker("2-sided","1-sided")
  )

}
