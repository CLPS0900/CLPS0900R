\name{states_data}
\alias{states_data}
\docType{data}
\title{
states_data
}
\description{
Data for the 50 United States, including crime statistics.
}
\usage{data("states_data")}
\format{
  A data frame with 50 observations on the following 16 variables.
  \describe{
    \item{\code{state}}{a factor with levels \code{Alabama       } \code{Alaska        } \code{Arizona       } \code{Arkansas      } \code{California    } \code{Colorado      } \code{Connecticut   } \code{Delaware      } \code{Florida       } \code{Georgia       } \code{Hawaii        } \code{Idaho         } \code{Illinois      } \code{Indiana       } \code{Iowa          } \code{Kansas        } \code{Kentucky      } \code{Louisiana     } \code{Maine         } \code{Maryland      } \code{Massachusetts } \code{Michigan      } \code{Minnesota     } \code{Mississippi   } \code{Missouri      } \code{Montana       } \code{Nebraska      } \code{Nevada        } \code{New Hampshire } \code{New Jersey    } \code{New Mexico    } \code{New York      } \code{North Carolina} \code{North Dakota  } \code{Ohio          } \code{Oklahoma      } \code{Oregon        } \code{Pennsylvania  } \code{Rhode Island  } \code{South Carolina} \code{South Dakota  } \code{Tennessee     } \code{Texas         } \code{Utah          } \code{Vermont       } \code{Virginia      } \code{Washington    } \code{West Virginia } \code{Wisconsin     } \code{Wyoming       }}
    \item{\code{abbreviation}}{a factor with levels \code{AK} \code{AL} \code{AR} \code{AZ} \code{CA} \code{CO} \code{CT} \code{DE} \code{FL} \code{GA} \code{HI} \code{IA} \code{ID} \code{IL} \code{IN} \code{KS} \code{KY} \code{LA} \code{MA} \code{MD} \code{ME} \code{MI} \code{MN} \code{MO} \code{MS} \code{MT} \code{NC} \code{ND} \code{NE} \code{NH} \code{NJ} \code{NM} \code{NV} \code{NY} \code{OH} \code{OK} \code{OR} \code{PA} \code{RI} \code{SC} \code{SD} \code{TN} \code{TX} \code{UT} \code{VA} \code{VT} \code{WA} \code{WI} \code{WV} \code{WY}}
    \item{\code{region}}{a factor with levels \code{NC    } \code{NE    } \code{SO    } \code{WE    }}
    \item{\code{murder}}{a numeric vector}
    \item{\code{rape}}{a numeric vector}
    \item{\code{assault}}{a numeric vector}
    \item{\code{violent.crime}}{a numeric vector}
    \item{\code{urban.population}}{a numeric vector}
    \item{\code{total.population}}{a numeric vector}
    \item{\code{hs.graduation}}{a numeric vector}
    \item{\code{income}}{a numeric vector}
    \item{\code{Life.Exp.Total}}{a numeric vector}
    \item{\code{Life.Exp.Male}}{a numeric vector}
    \item{\code{Life.Exp.Female}}{a numeric vector}
    \item{\code{Temp.F}}{a numeric vector}
    \item{\code{Temp.C}}{a numeric vector}
  }
}
\details{
None.
}
\source{
U.S. Department of Commerce, Bureau of the Census Statistical Abstract of the United States.

U.S. Department of Commerce, Bureau of the Census County and City Data Book.
}
\references{
Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) The New S Language. Wadsworth & Brooks/Cole.
}
\examples{
data(states_data)
## maybe str(states_data)
}
\keyword{datasets}
