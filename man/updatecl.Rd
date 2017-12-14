\name{updatecl}
\alias{updatecl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Check transformed Speclib
}
\description{
Update a transformed Speclib with a re-calculated hull
}
\usage{
updatecl(x, hull) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    Object of class \code{\linkS4class{Speclib}} transformed by \code{\link{transformSpeclib}}.
}
  \item{hull}{
    Hull to be applied to x. Output of function \code{\link{makehull}}.
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
  Object of class \code{\linkS4class{Speclib}}.
}
\seealso{
\code{\link{transformSpeclib}}, \code{\link{makehull}}, \code{\linkS4class{Speclib}}
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

## Add fix point at 4596 nm to continuum line of first spectrum
spec_clman <- addcp(spec_clman, 1, 2496)

## Check new hull
hull <- checkhull(spec_clman, 1)
hull$error

## Re-calculate hull
hull <- makehull(spec_clman, 1)

## Transform spectra using band depth
spec_bd <- transformSpeclib(spec, method = "sh", out = "bd")

## Update continuum line of first spectrum
spec_bd <- updatecl(spec_bd, hull)

## Plot modified transformed spectrum
plot(spec_bd, FUN = 1)
}
\keyword{ utilities }
