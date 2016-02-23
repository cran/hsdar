\name{deletecp}
\alias{deletecp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Delete fix point
}
\description{
Delete fix point from continuum line.
}
\usage{
deletecp(x, ispec, cpdelete) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    Object of class \code{Clman}.
}
  \item{ispec}{
    ID or index of spectrum to be modified.
}
  \item{cpdelete}{
    Single value or vector of wavelength containing fix point(s) to be deleted.
}
}
\author{
Lukas Lehnert and Hanna Meyer
}
\value{
  Object of class \code{\linkS4class{Clman}} containing the updated version of x.
}
\seealso{
\code{\link{transformSpeclib}}, \code{\link{addcp}}, \code{\link{getcp}}, \code{\link{checkhull}}, \code{\link{makehull}}, \code{\link{updatecl}}
}

\examples{
## Model spectra using PROSAIL
parameter <- data.frame(N = rep.int(c(1, 1.5),2), LAI = c(1,1,3,3))
spec <- PROSAIL(parameterList=parameter)
## Mask parts not necessary for the example
mask(spec) <- c(1600, 2600)

## Transform spectra
spec_clman <- transformSpeclib(spec, method = "sh", out = "raw")

## Plot original line
par(mfrow = c(1,2))
plot(spec_clman, ispec = 1, subset = c(1100, 1300))

## Find wavelength of fix point to be deleted
getcp(spec_clman, 1, subset = c(1100, 1300))

## Delete all fix points between 1200 and 1240 nm
spec_clman <- deletecp(spec_clman, 1, c(1200:1240))

## Plot new line
plot(spec_clman, ispec = 1, subset = c(1100, 1300))

## Check new hull
hull <- checkhull(spec_clman, 1)
hull$error
}
\keyword{ utilities }
