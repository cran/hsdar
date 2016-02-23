\name{makehull}
\alias{makehull}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Check continuum line
}
\description{
Check if continuum line is intersecting the reflectance curve.
}
\usage{
makehull(x, ispec) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    Object of class \code{Clman}.
}
  \item{ispec}{
    Name or index of spectrum to be checked.
}
}
\author{
Lukas Lehnert and Hanna Meyer
}
\value{
  Object of class \code{list}.
}
\seealso{
\code{\link{transformSpeclib}}, \code{\link{addcp}}, \code{\link{deletecp}}, \code{\link{makehull}}, \code{\link{updatecl}}

\code{\linkS4class{Clman}}
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

## Add fix point at 4596 nm to continuum line of first spectrum
spec_clman <- addcp(spec_clman, 1, 2496)

## Check new hull
hull <- checkhull(spec_clman, 1)
hull$error

hull <- makehull(spec_clman, 1)

## Transform spectra using band depth
spec_bd <- transformSpeclib(spec, method = "sh", out = "bd")

## Update continuum line of first spectrum
spec_bd <- updatecl(spec_bd, hull)

## Plot modified transformed spectrum
plot(spec_bd, FUN = 1)
}
\keyword{ utilities }
