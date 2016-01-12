\name{import_USGS}
\alias{USGS_get_available_files}
\alias{USGS_retrieve_files}

\title{
import USGS spectra
}
\description{
Import and download spectral data from USGS spectral library
}
\usage{
USGS_get_available_files(url = NULL)

USGS_retrieve_files(avl = USGS_get_available_files(), 
                    pattern = NULL, retrieve = TRUE, 
                    loadAsSpeclib = TRUE, tol = 0.1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{url}{
Character passing the url of the data. If NULL, the following URL is used:
'ftp://ftpext.cr.usgs.gov/pub/cr/co/denver/speclab/pub/spectral.library/splib06.library/ASCII/'
}
  \item{avl}{
List of available files. Typically the result of \code{USGS_get_available_files}.
}
  \item{pattern}{
Search pattern to define a subset of all available spectra.
}
  \item{retrieve}{
Logical. Should the data be downloaded?
}
  \item{loadAsSpeclib}{
Logical. If TRUE, an object of class "Speclib" is retured
}
\item{tol}{
Discrepancy of the wavelength values between different spectra.
}
}

\author{
Lukas Lehnert
}

\examples{
\dontrun{
## Retrieve all available spectra
avl <- USGS_get_available_files()

## Download all spectra matching "grass-fescue"
grass_spectra <- USGS_retrieve_files(avl = avl, pattern = "grass-fescue")

plot(grass_spectra)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{multivariate}
