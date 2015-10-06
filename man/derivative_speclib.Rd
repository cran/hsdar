\name{derivative.speclib}
\alias{derivative.speclib}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Derivation
}
\description{
Calculate derivations of spectra
}
\usage{
derivative.speclib(x, m = 1, method = "sgolay", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class \code{Speclib}.
}
\item{m}{
Return the m-th derivative of the spectra.
}
  \item{method}{
Character string giving the method to be used. Valid options are \code{"finApprox"} or \code{"sgolay"}.
}
  \item{...}{
Further arguments passed to \code{\link{sgolayfilt}}.
}
}
\details{
  Two different methods are available:
  \itemize{
    \item{Finite approximation (\code{finApprox}):}{
      \deqn{\frac{dr}{d\lambda}=\frac{r(\lambda_i)-r(\lambda_{i+1})}{\Delta\lambda},}
      where \eqn{r_i} is the reflection in band \eqn{i} and \eqn{\Delta\lambda} the spectral difference between adjacent bands.}
    \item{Savitzky-Golay derivative computation (\code{sgolay})%:
      
    }{
%       \deqn{\frac{dr^q}{d\lambda}=\sum_{i=-m}^{m}P_i^{(q)}r_{j+i},}
    }
  }
}
\value{
Object of class \code{\linkS4class{Speclib}}.
}
\references{
Tsai, F. & Philpot, W. (1998): Derivative analysis of hyperspectral data. Remote Sensing of Environment 66/1. 41-51.
}
\author{
Lukas Lehnert
}
\seealso{
\code{\link{sgolayfilt}}, \code{\link{vegindex}}
}
\examples{
data(spectral_data)

## Calculate 1st derivation
d1 <- derivative.speclib(spectral_data)

## Calculate 2nd derivation
d2 <- derivative.speclib(spectral_data, m = 2)

## Calculate 3rd derivation
d3 <- derivative.speclib(spectral_data, m = 3)

par(mfrow=c(2,2))
plot(spectral_data)
plot(d1)
plot(d2)
plot(d3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
