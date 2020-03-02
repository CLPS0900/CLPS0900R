\name{BigFiveData}
\alias{BigFiveData}
\docType{data}
\title{
BigFiveData
}
\description{
Self Ratings on the Five Factor Inventory for 50 women and 50 men.Formatted for use by
students in CLPS0900 (Quantitative Methods in the Psychological Sciences).
}
\usage{data("BigFiveData")}
\format{
  A data frame with 100 observations on the following 6 variables.
  \describe{
    \item{\code{gender}}{a factor with levels \code{F} \code{M}}
    \item{\code{Self.N}}{a numeric vector}
    \item{\code{Self.E}}{a numeric vector}
    \item{\code{Self.O}}{a numeric vector}
    \item{\code{Self.A}}{a numeric vector}
    \item{\code{Self.C}}{a numeric vector}
  }
}
\details{
See survey distributed in class.
}
\source{
https://en.wikipedia.org/wiki/Revised_NEO_Personality_Inventory
}
\references{
https://www.parinc.com/Products/Pkey/274
}
\examples{
data(BigFiveData)
## maybe mplot(BigFiveData)
}
\keyword{datasets}
