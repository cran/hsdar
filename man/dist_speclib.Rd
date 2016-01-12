\name{dist.speclib}
\alias{dist.speclib}
\alias{sam}
\alias{sam_distance}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Distance between spectra
}
\description{
Calculation of distance matrices by using one of the various distance measure to compute the distances between the spectra in \code{Speclib}. Spectral Angle Mapper (SAM) is calculated with \code{sam} giving reference spectra or with \code{sam_distance} taking all combinations between spectra in single Speclib into account.
}
\usage{
dist.speclib(x, method = "sam", ...)

## Direct call to Spectral Angle Mapper function
sam(x, ref)
sam_distance(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class \code{Speclib}.
}
  \item{method}{
The distance measure to be used. This must be one of "sam", "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski".
}
\item{ref}{
Object of class \code{Speclib} containing reference spectra.
}
  \item{...}{
Further arguments, passed to other methods.
}
}
\details{
  Available distance measures are "spectral angle mapper" (\code{sam}) and all distance measures available in \code{\link{dist}}. Spectral angle mapper is calculated with the following formula:
  \deqn{sam=\cos^{-1}\left(\frac{\sum_{i=1}^{nb}{t_i r_i}}{\sqrt{\sum_{i=1}^{nb}{t_i^2}}\sqrt{\sum_{i=1}^{nb}{r_i^2}}}\right)}{
    \cos^{-1}(\sum_{i=1}^{nb}{t_i r_i} \sum_{i=1}^{nb}{t_i^2}^{-0.5} \sum_{i=1}^{nb}{r_i^2}^{-0.5})}
  \eqn{nb} is the number of bands in Speclib. \eqn{t_i} and \eqn{r_i} are the reflectances of target and reference spectrum in band \eqn{i}, respectively.
}
\value{
The \code{dist-method} for Speclibs returns an object of class \code{"dist"}. See \code{\link{dist}} for further information on class \code{"dist"}. Both other functions return an object of class matrix.
}
\references{
Kruse, F. A.; Lefkoff, A. B.; Boardman, J. W.; Heidebrecht, K. B.; Shapiro, A. T.; Barloon, P. J. & Goetz, A. F. H. (1993). The spectral image processing system (SIPS) -- interactive visualization and analysis of imaging spectrometer data. Remote Sensing of Environment, 44, 145-163.
}
\author{
Lukas Lehnert
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{dist}}, \code{\linkS4class{Speclib}}
}
\examples{
data(spectral_data)

## Mask channel crossing part (arround 1050 nm) and strong 
## water absorption part (above 1350 nm)
mask(spectral_data) <- c(1045, 1055, 1350, 1706)

## Calculate distance between all spectra from spring 
## using spectral angle mapper 
dist.speclib(subset(spectral_data, season == "spring"))


## Calculate spectral angle mapper between reference spectrum
## and spectral_data
## Use first spectrum from summer as reference
distance <- sam(subset(spectral_data, season == "spring"), 
                subset(spectral_data, season == "summer")[1,])

}

