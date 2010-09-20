\name{predint-methods}
\alias{predint-methods}
\alias{predint.default}
\alias{print.predint}
\alias{plot.predint}
\alias{summary.predint}

\title{predint-Methods}
\description{
S3 Methods for objects of class predint
}
\usage{
\method{predint}{default}(xhist, xtest = NULL, type = "Prediction", distr = "free", k = length(xtest), conf.level = 0.95, alternative = "two.sided", direction = "increase", MR = FALSE, kwin=0)
\method{print}{predint}(x, ...)
\method{plot}{predint}(x, ...)
\method{summary}{predint}(object, ...)
}

\arguments{
  \item{x}{An object of class predint}
  \item{object}{An object of class predint}
  \item{xhist}{Numeric vector: Sample of historical observations}
  \item{xtest}{Numeric vector: Sample of test observations}
  \item{type}{Character vector: Either "Prediction" for intervals containing k of m future observations or "Precedence" for intervals containing the future median.}
  \item{distr}{Character vector: Either "free" for distribution free intervals or "normal" for intervals assuming  normal distribution}
  \item{k}{Numeric: Number of test observations to be included in a prediction interval}
  \item{conf.level}{Numeric: Confidence level}
  \item{alternative}{Character vector: One of "two.sided", "less" or "greater"}
  \item{direction}{Character vector: "decrease" or "increase" to indicate the direction of rounding for distribution free intervals}
  \item{kwin}{Number of observations to be winsorized at both extremes for intervals assuming normal distribution}
  \item{MR}{Logical: If TRUE the moving average is used instead of the standard deviation}
  \item{...}{Further arguments}
}
\details{
}
\value{
  Print, summary and plot for predint objects
}
\references{
Hahn, GJ and Meeker, WQ (1991): Statistical Intervals. Wiley, New York.

Chakraborti, S., Van der Laan, P., Van de Wiel, M.A. (2004): A class of distribution-free control charts. Applied Statistics 53(3):443-462.

Danziger, L. and Davis, S.A. (1964): Tables of distribution-free tolerance limits. Annals of Mathematical Statistics 35(3):1361-1365.

Odeh, RE (1990): 2-Sided prediction intervals to contain at least k out of m future observations from a normal distribution. Technometric 32(2): 203-216.
}
\author{Daniel Gerhard <gerhard@biostat.uni-hannover.de>}
\note{}
\seealso{predint}
\examples{}
\keyword{ htest }

