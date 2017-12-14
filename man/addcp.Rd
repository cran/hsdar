\name{addcp}
\alias{addcp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Manually add fix point to continuum line
}
\description{
This function is used to add an additional fix point to a manually created hull of a single spectrum. This fix point is then used to re-construct a continuum line.
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
  Object of class \code{Clman} containing the updated version of x.
}
\seealso{
\code{\link{transformSpeclib}}, \code{\link{deletecp}}, \code{\link{getcp}}, 
\code{\link{checkhull}}, \code{\link{makehull}}, \code{\link{updatecl}}, 

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
plot(spec_clman, ispec = 1, xlim = c(2480, 2500), ylim=c(0.022,0.024))

## Add fix point at 4595 nm to continuum line of first spectrum
spec_clman <- addcp(spec_clman, 1, 2495)

## Plot new line
plot(spec_clman, ispec = 1, xlim = c(2480, 2500), ylim=c(0.022,0.024))

## Check new hull
hull <- checkhull(spec_clman, 1)
hull$error
}
\keyword{ utilities }
