\name{endmember_spectra}
\alias{endmember_spectra}
\docType{data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Hyperspectral samples from pure endmembers
}
\description{
Hyperspectral samples from pure endmembers of two ecosystems on the Tibetan Plateau
}
\usage{
data(endmember_spectra)
}
\details{
  Data has been sampled during vegetation period 2012 from two of the most wide-spread vegetation types on the Tibetan Plateau: \var{Kobresia} meadows and dry alpine steppes. Measurements were taken with a HandySpec Field portable spectrometer (tec5 AG Oberursel, Germany). This device has two channels measuring incoming and reflected radiation simultaneously between 305 and 1705 nm in 1 nm steps. Note that example data only contains spectral ranges between 305 and 1350 nm.
  
  The \code{\linkS4class{Speclib}} contains 8 spectra, which are mean values of several 10th of single samples. The 8 spectra encompass soil (dry/wet), photosynthetically active vegetation, open \var{Kobresia} turf, stones, and photosynthetically non-active vegetation.
}
\format{An object of class \code{Speclib}}
\author{Lukas Lehnert, Hanna Meyer}
\references{
    Lehnert, L. W.; Meyer, H.; Meyer, N.; Reudenbach, C. & Bendix, J. (2014): A hyperspectral indicator system for rangeland degradation on the Tibetan Plateau: A case study towards spaceborne monitoring. Ecological Indicators, 39, 54 - 64. DOI: 10.1016/j.ecolind.2013.12.005
}
\keyword{datasets}

