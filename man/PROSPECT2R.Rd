\name{PROSPECT}
\alias{PROSPECT}
\title{
Simulate plant spectrum
}
\description{
Simulate plant spectrum using PROSPECT 5
}
\usage{
PROSPECT(N = 1.5, Cab = 40, Car = 8, Cbrown = 0.0, 
         Cw = 0.01, Cm = 0.009, transmittance = FALSE,
         parameterList = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{N}{
Structure parameter
}
  \item{Cab}{
Chlorophyll content% (in \%)
}
\item{Car}{
Carotenoid content %(in \%)
}
\item{Cbrown}{
Brown pigment content% (arbitrary units)
}
\item{Cw}{
Equivalent water thickness% (\eqn{\textnormal{cm}}{cm})
}
\item{Cm}{
Dry matter content% (\eqn{\textnormal{g}/\textnormal{cm}^2}{g/cm^2})
}
\item{transmittance}{
  Logical flag, if transmittance instead of reflectance values are returned.
}
\item{parameterList}{
  An optional object of class \code{'data.frame'}. Function will iterate over rows of parameterList setting missing entries to default values. See examples section.
}
}
\details{
This function uses the FORTRAN code of PROSPECT model (Version 5). For a general introduction see following web page and the links to articles provided there:

\url{http://teledetection.ipgp.jussieu.fr/prosail/}

The following table summarises the abbreviations of parameters and gives their units as used in PROSPECT. Please note that default values of all parameters were included with the intention to provide an easy access to the model and should be used with care in any scientific approach!
\tabular{lll}{
\tab\tab\cr
Parameter \tab Description of parameter\tab  Units\cr
\tab\tab\cr
N\tab Leaf structure parameter\tab  NA \cr
Cab\tab Chlorophyll a+b concentration\tab \eqn{\mu}{u}g/cm\eqn{^2}{2} \cr
Car\tab Carotenoid concentration\tab  \eqn{\mu}{u}g/cm\eqn{^2}{2} \cr
Cw\tab Equivalent water thickness\tab cm \cr
Cbrown \tab Brown pigment \tab NA \cr
Cm\tab  Dry matter content\tab  g/cm\eqn{^2}{2}\cr
}
}
\value{
An object of class \code{Speclib}.
}
\references{
Jacquemoud, S. and Baret, F. (1990). PROSPECT: A model of leaf optical properties spectra, Remote Sensing of Environment 34: 75 - 91.
}
\author{
Lukas Lehnert
}
\note{
The function is based on the FORTRAN version of the PROSPECT-code initially developed by Jean-Baptiste FERET, Stephane JACQUEMOUD and Christophe FRANCOIS.
}
\seealso{
\code{\link{PROSAIL}}, \code{\linkS4class{Speclib}}
}
\examples{
## Single spectrum
spectrum <- PROSPECT(N = 1.3, Cab = 30, Car = 10, Cbrown = 0, 
                     Cw = 0.01, Cm = 0.01)
plot(spectrum)

## Example using parameterList
## Test effect of leaf structure and chlorophyll content on 
## spectra
parameter <- data.frame(N = c(rep.int(seq(0.5, 1.5, 0.5), 2)),
                        Cab = c(rep.int(40, 3), rep.int(20, 3)))
spectra <- PROSPECT(parameterList = parameter)

## Print attributes table
attribute(spectra)

## Plot spectra for range from 400 to 800 nm
#plot(subset(spectra, Cab == 20), col = "red", ylim = c(0, 0.5),
#     subset = c(400, 800))
#plot(subset(spectra, Cab == 40), col = "green", new = FALSE,
#     subset = c(400, 800))
}
