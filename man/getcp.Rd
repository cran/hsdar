\name{getcp}
\alias{getcp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get fix points
}
\description{
Get fix points of continuum line within spectral range.
}
\usage{
getcp(x, ispec, subset = NULL) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    Object of class \code{Clman}.
}
  \item{ispec}{
    ID or index of spectrum to be analysed.
}
  \item{subset}{
    Vector of \code{length = 2} giving the lower and upper limit of spectral range.
}
}
\author{
Lukas Lehnert and Hanna Meyer
}
\value{
  Object of class \code{list} containing two elements:
  \itemize{
    \item{ptscon:}{ Data frame with wavelength and reflectance of fix points}
    \item{ispec:}{ Index of analysed spectrum within passed \code{Clman}-object.}
  }
}
\seealso{
\code{\link{transformSpeclib}}, \code{\link{deletecp}}, \code{\link{addcp}}, \code{\linkS4class{Clman}}
}

\examples{
## Model spectra using PROSAIL
parameter <- data.frame(N = rep.int(c(1, 1.5),2), LAI = c(1,1,3,3))
spec <- PROSAIL(parameterList=parameter)

## Transform spectra
spec_clman <- transformSpeclib(spec, method = "sh", out = "raw")

## Fix points
spec_cp <- getcp(spec_clman, 1, c(400, 800))
spec_cp
}
\keyword{ utilities }
