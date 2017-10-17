\name{PROSPECT}
\alias{PROSPECT}
\alias{PROSPECTinvert}
\title{
Simulate plant spectrum
}
\description{
Simulate plant spectrum using PROSPECT 5b or PROSPECT D. The inversion uses the concept after Feret et al. (2008) and uses PROSPECT 5B.
}
\usage{
PROSPECT(N = 1.5, Cab = 40, Car = 8, Anth = 1.0, Cbrown = 0.0, 
         Cw = 0.01, Cm = 0.009, transmittance = FALSE,
         parameterList = NULL, version = "D")
## Inversion
PROSPECTinvert(x, P0 = NULL, transmittance_spectra = NULL, 
               sam = FALSE, ...)
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
\item{Anth}{
 Anthocyanin content 
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
\item{version}{
  Sets the version of PROSPECT to be used (either "5B" or "D").
}
\item{x, transmittance_spectra}{Speclib(s) containing the reflectance/transmittance values to be simulated during inversion of PROSPECT.}
\item{P0}{Initial set of parameters (N, Cab etc.).}
\item{sam}{Logical if spectral angle mapper is used as distance measurement. If FALSE, the root mean square error is used. Note that this flag has only an effect if no transmittance spectra are passed.}
\item{...}{Parameters passed to \code{\link[pracma]{anms}} from the \pkg{pracma}-package}
}
\details{
This function uses the FORTRAN code of PROSPECT model (Version 5B an D). For a general introduction see following web page and the links to articles provided there:

\url{http://teledetection.ipgp.jussieu.fr/prosail/}

The following table summarises the abbreviations of parameters and gives their units as used in PROSPECT. Please note that default values of all parameters were included with the intention to provide an easy access to the model and should be used with care in any scientific approach!
\tabular{lll}{
\tab\tab\cr
Parameter \tab Description of parameter\tab  Units\cr
\tab\tab\cr
N\tab Leaf structure parameter\tab  NA \cr
Cab\tab Chlorophyll a+b concentration\tab \eqn{\mu}{u}g/cm\eqn{^2}{2} \cr
Car\tab Carotenoid concentration\tab  \eqn{\mu}{u}g/cm\eqn{^2}{2} \cr
Anth\tab  Anthocyanin content \tab  \eqn{\mu}{u}g/cm\eqn{^2}{2} \cr
Cw\tab Equivalent water thickness\tab cm \cr
Cbrown \tab Brown pigment \tab NA \cr
Cm\tab  Dry matter content\tab  g/cm\eqn{^2}{2}\cr
}

The inversion uses the function \code{\link[pracma]{anms}} from the \pkg{pracma}-package and implements the Matlab-Code developed by Feret et al. (2008). Please note that the inversion currently only uses version 5B.
}
\value{
An object of class \code{Speclib}.
}
\references{
Jacquemoud, S. and Baret, F. (1990). PROSPECT: A model of leaf optical properties spectra, Remote Sensing of Environment 34: 75 - 91.

Feret J.B., Francois C., Asner G.P., Gitelson A.A., Martin R.E., Bidel L.P.R., Ustin S.L., le Maire G., & Jacquemoud S. (2008), PROSPECT-4 and 5: advances in the leaf optical properties model separating photosynthetic pigments. Remote Sensing of Environment, 112, 3030-3043.

Feret J.B., Gitelson A.A., Noble S.D., & Jacquemoud S. (2017), PROSPECT-D: towards modeling leaf optical properties through a complete lifecycle, Remote Sensing of Environment, 193, 204-215. 
}
\author{
Lukas Lehnert
}
\note{
The function is based on the FORTRAN version of the PROSPECT-code initially developed by Jean-Baptiste FERET, Stephane JACQUEMOUD and Christophe FRANCOIS.
}
\seealso{
\code{\link{PROSAIL}}, \code{\link[pracma]{anms}}, \code{\linkS4class{Speclib}}
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

## Print SI table
SI(spectra)

## Plot spectra for range from 400 to 800 nm
spectra <- spectra[,wavelength(spectra) >= 400 & 
                    wavelength(spectra) <= 800]

plot(subset(spectra, Cab == 20), col = "red", ylim = c(0, 0.5))
plot(subset(spectra, Cab == 40), col = "green", new = FALSE)
}
