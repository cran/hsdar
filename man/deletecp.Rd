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
\details{
In some cases, it might be desirable to manually adapt automatically 
constructed segmended hulls (\code{\link{transformSpeclib}}). For example
local maxima could be removed because they are very small and maybe afflicted with uncertainties which might legitimate it to manipulate the continuum line. Therefore, hsdar provides functions to remove and add "continuum points" from or to a continuum line. Manually adapted continuum lines can then be used to update band depth or ratio transformation. Handle these functions with care to avoid continuum lines too much build by subjective decisions. In the typical workflow, spectra are first transformed 
(\code{\link{transformSpeclib}}). Continuum points can then be retrieved (\code{\link{getcp}}) and manually adapted by adding \code{\link{addcp}} and deleting (\code{\link{deletecp}}) of points. Use \code{\link{checkhull}} to check for errors. If all uncertainties
are removed, re-calculate the hull (\code{\link{makehull}}) and update the
transformed spectrum (\code{\link{updatecl}}).
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
plot(spec_clman, ispec = 1, xlim = c(1100, 1300),ylim=c(0.17,0.21))

## Find wavelength of fix point to be deleted
getcp(spec_clman, 1, subset = c(1100, 1300))

## Delete all fix points between 1200 and 1240 nm
spec_clman <- deletecp(spec_clman, 1, c(1200:1240))

## Plot new line
plot(spec_clman, ispec = 1, xlim = c(1100, 1300),ylim=c(0.17,0.21))

## Check new hull
hull <- checkhull(spec_clman, 1)
hull$error
}
\keyword{ utilities }
