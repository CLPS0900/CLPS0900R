\name{WHO_DATA}
\alias{WHO_DATA}
\docType{data}
\title{
WHO_DATA
}
\description{
WHO data for life expectancies, by country (2015).  Formatted for use by students in
CLPS0900 (Quantitative Methods in the Psychological Science).
}
\usage{data("WHO_DATA")}
\format{
A data frame consisting of 192 rows (countries) and 6 variables
 \itemize{
   \item Country     :  UN country name
   \item Region.Long : Long name for the region of the world for the country
   \item Region      : Short name for the above
   \item LifeE       : Average life expectancy for the country (in years)
   \item IPP         : Income per person, in U.S. dollars.
   \item IPP.log     : Log of IPP
 }

}
\details{
More info about data set goes here...
}
\source{
https://www.who.int/gho/mortality_burden_disease/life_tables/en/
}
\references{
https://www.who.int/gho/database/en/
}
\examples{
data(WHO_DATA)
## maybe mplot(WHO_DATA)
}
\keyword{datasets}
