\name{PROSAIL}
\alias{PROSAIL}
\title{
Simulate canopy spectrum
}
\description{
Simulate a canopy spectrum using PROSAIL 5B
}
\usage{
PROSAIL(N = 1.5, Cab = 40, Car = 8, Cbrown = 0.0,
        Cw = 0.01, Cm = 0.009, psoil = 0, LAI = 1, 
        TypeLidf = 1, lidfa = -0.35, lidfb = -0.15,
        hspot = 0.01, tts = 30, tto = 10, psi = 0,
        parameterList = NULL, rsoil = NULL)
}
\arguments{
  \item{N}{
Structure parameter
}
  \item{Cab}{
Chlorophyll content% (in \eqn{\mu}{u}g/cm\eqn{^2}{2})
}
\item{Car}{
Carotenoid content %(in \eqn{\mu}{u}g/cm\eqn{^2}{2})
}
\item{Cbrown}{
Brown pigment content %(arbitrary units)
}
\item{Cw}{
Equivalent water thickness %(\eqn{\textnormal{cm}}{cm})
}
\item{Cm}{
Dry matter content %(\eqn{\textnormal{g}/\textnormal{cm}^2}{g/cm^2})
}
\item{psoil}{
Dry/Wet soil factor
}
\item{LAI}{
Leaf area index
}
\item{TypeLidf}{
 Type of leaf angle distribution. See details section
}
\item{lidfa}{
 Leaf angle distribution. See details section
}
\item{lidfb}{
 Leaf angle distribution. See details section
}
\item{hspot}{
Hotspot parameter
}
\item{tts}{
  Solar zenith angle
}
\item{tto}{
Observer zenith angle
}
\item{psi}{
Relative azimuth angle
}
\item{parameterList}{
  An optional object of class \code{'data.frame'}. Function will iterate over rows of parameterList setting missing entries to default values. See examples section.
}
\item{rsoil}{
  An optional object of class \code{'Speclib'} containing the background (soil) reflectance. Note that reflectance values must be in range [0...1].
}
}
\details{
This function uses the FORTRAN code of PROSAIL model (Version 5B). For a general introduction see following web page and the links to articles provided there:

\url{http://teledetection.ipgp.jussieu.fr/prosail/}

The following table summarises the abbreviations of parameters and gives their units as used in PROSAIL. Please note that default values of all parameters were included with the intention to provide an easy access to the model and should be used with care in any scientific approach!
\tabular{lll}{
\tab\tab\cr
Parameter \tab Description of parameter\tab  Units\cr
\tab\tab\cr
N\tab Leaf structure parameter\tab  NA \cr
Cab\tab Chlorophyll a+b concentration\tab \eqn{\mu}{u}g/cm\eqn{^2}{2} \cr
Car\tab Carotenoid concentration\tab  \eqn{\mu}{u}g/cm\eqn{^2}{2} \cr
Caw\tab Equivalent water thickness\tab cm \cr
Cbrown \tab Brown pigment \tab NA \cr
Cm\tab  Dry matter content\tab  g/cm\eqn{^2}{2}\cr
LAI \tab Leaf Area Index \tab NA\cr
psoil\tab Dry/Wet soil factor\tab NA \cr
hspot\tab Hotspot parameter\tab NA \cr
tts\tab Solar zenith angle\tab  deg\cr
tto\tab Observer zenith angle\tab deg \cr
psi\tab Relative azimuth angle\tab  deg \cr
}

Functions for distribution of leaf angles within the canopy may work in two modes, which is controlled via \code{TypeLidf}:
\enumerate{
  \item{\code{TypeLidf == 1} (default):}{ \code{lidfa} is the average leaf slope and \code{lidfb} describes bimodality of leaf distribution. The following list gives an overview on typical settings:
    \tabular{lrr}{
      LIDF type\tab      \code{lidfa}   \tab  \code{lidfb}\cr
      \tab\tab\cr
      Planophile  \tab   1  \tab   0\cr
      Erectophile \tab    -1 \tab    0\cr
      Plagiophile  \tab  0 \tab   -1\cr
      Extremophile\tab   0  \tab   1\cr
      Spherical (default)   \tab    -0.35 \tab  -0.15\cr
    }
  }
  \item{\code{TypeLidf != 1}:}{ \code{lidfa} is the average leaf angle in degree (0 = planophile / 90 = erectophile); \code{lidfb} is 0}
}
}
\value{
An object of class \code{Speclib}. If parameterList is used, the parameter are stored in SI table of \code{Speclib}.
}
\references{
Jacquemoud, S., Verhoef, W., Baret, F., Bacour, C., Zarco-Tejada, P.J., Asner, G.P., Francois, C., and Ustin, S.L. (2009): PROSPECT + SAIL models: a review of use for vegetation characterization, Remote Sensing of Environment, 113, S56-S66.
}
\author{
Lukas Lehnert
}
\note{
The function is based on the FORTRAN version of the PROSAIL-code initially developed by Stephane JACQUEMOUD, Jean-Baptiste FERET, Christophe FRANCOIS and Eben BROADBENT. SAIL component has been developed by Wout VERHOEF.
}
\seealso{
\code{\link{PROSPECT}}, \code{\linkS4class{Speclib}}
}
\examples{
## Single spectrum
spectrum <- PROSAIL(N = 1.3)
plot(spectrum)

## Example using parameterList
## Test effect of leaf structure and LAI on spectra
parameter <- data.frame(N = c(rep.int(seq(0.5, 1.5, 0.5), 2)),
                        LAI = c(rep.int(0.5, 3), rep.int(1, 3)))
spectra <- PROSAIL(parameterList = parameter)

## Print SI table
SI(spectra)

## Plot spectra
plot(subset(spectra, LAI == 0.5), col = "red", ylim = c(0, 0.3))
plot(subset(spectra, LAI == 1), col = "green", new = FALSE)
}
