\name{InternalFunctions}
\alias{InternalFunctions}
\alias{estsInt}
\alias{precIdfree.test}
\alias{predIdfree.test}
\alias{precINorm.test}
\alias{predINorm.test}
\alias{predI.hist}

\title{Internal functions for calculating predint objects}
\description{
Internal functions. Please, use function predint to calculate intervals.
}
\usage{
estsInt(x, MR = FALSE, kwin)
precIdfree.test(xhist, xtest, conf.level = 0.95, alternative = c("two.sided"))
precINorm.test(est.hist, est.test, conf.level = 0.95, alternative = c("two.sided"))
predIdfree.test(xhist, xtest, k, direction = "increase", conf.level = 0.95, alternative = c("two.sided"))
predINorm.test(est.hist, est.test, k, conf.level = 0.95, alternative = c("two.sided"))
predI.hist(est.hist, conf.level = 0.95, alternative = c("two.sided"))
}
\arguments{
  \item{x}{A numeric vector with historical or test observations}
  \item{xhist}{Numeric vector: Sample of historical observations}
  \item{xtest}{Numeric vector: Sample of test observations}
  \item{est.hist}{Estimates for the historical observations}
  \item{est.test}{Estimates for the test observations}
  \item{k}{Numeric: Number of test observations to be included in a prediction interval}
  \item{conf.level}{Numeric: Confidence level}
  \item{alternative}{Character vector: One of "two.sided", "less" or "greater"}
  \item{direction}{Character vector: "decrease" or "increase" to indicate the direction of rounding for distribution free intervals}
  \item{MR}{Logical: If TRUE the moving average is used instead of the standard deviation}
  \item{kwin}{Numeric: number of winsorized observations}
}
\details{

}
\value{}
\references{}
\author{Daniel Gerhard <gerhard@biostat.uni-hannover.de>}
\note{}
\seealso{ predint }
\examples{}
\keyword{ htest }

