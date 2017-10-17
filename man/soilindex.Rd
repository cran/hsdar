\name{soilindex}
\alias{soilindex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
soilindex
}
\description{
Function calculates a variety of hyperspectral soil indices
}
\usage{
soilindex(x, index, returnHCR = "auto", weighted = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class \code{Speclib}
}
  \item{index}{
Character string. Name or definition of index or vector with names/definitions of indices to calculate. See Details section for further information.
}
  \item{returnHCR}{
If TRUE, the result will be of class HyperSpecRaster, otherwise it is a data frame. If "auto", the class is automatically determined by passed Speclib.
}
\item{weighted}{
Logical indicating if reflectance values should be interpolated to fit wavelength position. If \code{FALSE} the reflectance values of nearest neighbour to passed position are returned. See \code{\link{get_reflectance}} for further explanation.
}
  \item{...}{
Further arguments passed to derivative functions. Only used for indices requiring derivations.
}
}
\details{
Index must be a charater vector containing pre-defined indices (selected by their name) or self defined indices or any combination of pre- and self-defined indices.
\subsection{Pre-defined indices}{
The following indices are available:
\tabular{lll}{
\tab\tab\cr
\strong{Name}\tab \strong{Formula} \tab \strong{Reference*}\cr
\tab\tab\cr
BI_TM \tab \eqn{((TM\_1^2 + TM\_2^2+ TM\_3^2)/3)^{0.5}}**                                        \tab Mathieu et al. (1998)\cr
CI_TM \tab \eqn{(TM\_3 - TM\_2)/ (TM\_3 + TM\_2)}**                                              \tab Escadafal and Huete \cr \tab \tab (1991)\cr
HI_TM \tab \eqn{(2\cdot TM\_3 - TM\_2 - TM\_1)/ (TM\_2-TM\_1)}**                                 \tab Escadafal et al. (1994)\cr
NDI   \tab \eqn{(R_{840} - R_{1650})/(R_{840} + R_{1650})}                                       \tab McNairn, H. and Protz, R.\cr \tab \tab (1993)\cr
NSMI   \tab \eqn{(R_{1800} - R_{2119})/(R_{1800} + R_{2119})}                                    \tab Haubrock et al. (2008)\cr
RI \tab \eqn{R_{693}^2/(R_{447}\cdot R_{556}^3)}                                                 \tab Ben-Dor et al. (2006)\cr
RI_TM \tab \eqn{TM\_3^2 / (TM\_1 \cdot TM\_2^3)}**                                               \tab Madeira et al. (1997),\cr
      \tab                                                                                       \tab Mathieu et al. (1998)\cr
SI_TM \tab \eqn{(TM\_3 - TM\_1)/ (TM\_3 + TM\_1)}**                                              \tab Escadafal et al. (1994) \cr
SWIR SI\tab \eqn{-41.59 \cdot (R_{2210} - R_{2090}) + }                                          \tab Lobell et al. (2001)\cr
       \tab \eqn{1.24 \cdot (R_{2280} - R_{2090}) + 0.64}                                        \tab \cr 
}

* For references please type: \code{hsdardocs("References.pdf")}.\cr
** TM_1 denotes the first band of Landsat Thematic Mapper. Consequently, the hyperspectral data is resmapled to Landsat TM using \code{\link{spectralResampling}} prior to the calculation of the index. For resampling, the spectral response function is used.
}
\subsection{Self-defining indices}{  
Self-defined indices may be passed using the following syntax:
\itemize{
  \item{Rxxx: }{Reflectance at wavelength 'xxx'. Note that R must be upper case.}
  \item{Dxxx: }{First derivation of reflectance values at wavelength 'xxx'. Note that D must be upper case.}
}
Using this syntax, complex indices can be easily defined. Note that the entire definition of the index must be passed as one character string. Consequently, the NSMI would be written as\cr
"(R1800-R2119)/(R1800+R2119)".
}
}
\value{
A vector containing indices values. If index is a vector with length > 1, a data frame with ncol = length(index) and nrow = number of spectra in x is returned.

If function is called without any arguments, return value will be a vector containing all available indices in alphabetical order.
}
\references{
See \code{hsdardocs("References.pdf")}
}
\author{
Lukas Lehnert
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{vegindex}}, \code{\link{get_reflectance}}
}
\examples{
data(spectral_data)
## Example calculating all available indices
## Get available indices

avl <- soilindex()
vi <- soilindex(spectral_data, avl)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{multivariate}
