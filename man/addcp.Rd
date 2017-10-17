\name{addcp}
\alias{addcp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Manually add fix point to continuum line
}
\description{
This function is used to add an additional fix point to a manually created hull of a single spectrum. This fix point is then used to construct a continuum line.
}
\usage{
addcp(x, ispec, cpadd) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    Object of class \code{Clman}.
}
  \item{ispec}{
    ID or index of spectrum to be modified.
}
  \item{cpadd}{
    Single value or vector of wavelength containing new fix point(s).
}
}
\author{
Lukas Lehnert and Hanna Meyer
}
\value{
  Object of class \code{Clman} containing the updated version of x.
}
\seealso{
\code{\link{transformSpeclib}}, \code{\link{deletecp}}, \code{\link{getcp}}, \code{\link{checkhull}}, \code{\link{makehull}}, \code{\link{updatecl}}, 

\code{\link{idSpeclib}}
}

\examples{
## Model spectra using PROSAIL
parameter <- data.frame(N = rep.int(c(1, 1.5),2), LAI = c(1,1,3,3))
spec <- PROSAIL(parameterList=parameter)

## Transform spectra
spec_clman <- transformSpeclib(spec, method = "sh", out = "raw")

## Plot original line
par(mfrow = c(1,2))
plot(spec_clman, ispec = 1, subset = c(2480, 2500))

## Add fix point at 4595 nm to continuum line of first spectrum
spec_clman <- addcp(spec_clman, 1, 2495)

## Plot new line
plot(spec_clman, ispec = 1, subset = c(2480, 2500))

## Check new hull
hull <- checkhull(spec_clman, 1)
hull$error
}
\keyword{ utilities }
