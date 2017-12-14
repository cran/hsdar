\encoding{utf-8}
\name{hsdar-package}
\alias{hsdar-package}
\alias{hsdar}
\docType{package}
\title{
Manage, analyse and simulate hyperspectral data in R
}
\description{ 
The \pkg{hsdar} package contains classes and functions to manage, analyse and simulate 
hyperspectral data. These might be either spectrometer measurements or hyperspectral 
images through the interface of \pkg{raster}. 
}
\details{
\pkg{hsdar} provides amongst others the following functionality.
\itemize{
\item{Data handling: }{
	\pkg{hsdar} is designed to handle even large sets of spectra. Spectra are
	stored in a \code{\linkS4class{Speclib}} containing, amongst other details, the wavelength and reflectance for each spectrum.
	\pkg{hsdar} further contains functions for \code{\link[=plot.Speclib]{plot}}ting spectral data and \code{\link[=apply.Speclib]{apply}}ing functions to spectra.
} 
\item{Data manipulation: }{
	A variety of established methods for data manipulation such as filter functions (\code{\link{noiseFiltering}}) for noise reduction, 
	resampling of bands to various satellite sensors (\code{\link{spectralResampling}}), 
	continuum removal (\code{\link{transformSpeclib}}),
	calculations of derivations (\code{\link{derivative.speclib}})
	and extraction of absorption features (\code{\link{cut_specfeat}}) are implemented.
} 
\item{Data analysis: }{
	Supported methods to analyse vegetation spectra are the calculation of red edge parameters (\code{\link{rededge}}), 
	vegetation (\code{\link{vegindex}}) and soil (\code{\link{soilindex}}, \code{\link{smgm}}) indices as well as ndvi-like narrow band indices (\code{\link{nri}}).
	\pkg{hsdar} further enables to perform linear spectral unmixing of spectra (\code{\link{unmix}}) by use of endmember spectra. Note that some functions allow the parallel execution using the \pkg{doMPI}-, \pkg{doMC}- and \pkg{foreach}-packages. Execute '\code{\link{hsdar_parallel}()}' to get supporting functions.
} 
\item{Data simulation: }{
	\pkg{hsdar} has implemented the models PROSAIL 5B (\code{\link{PROSAIL}}, Jacquemoud et al. 2009) and 
	PROSPECT 5 and D (\code{\link{PROSPECT}}, Jacquemoud and Baret 1990, Feret et al. 2017) 
	to simulate spectra of canopy and plants.
} 
}

Several classes are defined and used in \pkg{hsdar}. Most of the classes are used and respective objects are created internally. However, the following figure gives an overview which class is used at which stage of processing.
\if{html}{\figure{classes.png}{width="35\%" alt="Figure: classes.png"}}
\if{latex}{\figure{classes.pdf}{options: width=10cm}}

Note that the asterisk marks all classes for which wrapper functions for the \pkg{caret} package exist.

To see the preferable citation of the package, type
\code{citation("hsdar")}.  
}
\references{
Feret J.B., Gitelson A.A., Noble S.D., & Jacquemoud S. (2017), PROSPECT-D: towards modeling leaf optical properties through a complete lifecycle, Remote Sensing of Environment, 193, 204-215.  

Jacquemoud, S., Verhoef, W., Baret, F., Bacour, C., Zarco-Tejada, P.J., Asner, G.P., Francois, C., and Ustin, S.L. (2009): PROSPECT + SAIL models: a review of use for vegetation characterization, Remote Sensing of Environment, 113, S56-S66.

Jacquemoud, S. and Baret, F. (1990). PROSPECT: A model of leaf optical properties spectra, Remote Sensing of Environment 34: 75 - 91.
}
\section{Acknowledgements}{
  Development initially funded by German Federal Ministry of Education and Research (03G0808C) in the scope of the project PaDeMoS as precondition to develop a space-based Pasture Degradation Monitoring System for the Tibetan Plateau.
  
}

\author{
  Lukas Lehnert, Hanna Meyer, \ifelse{latex}{\out{J\"{o}rg}}{Jörg} Bendix
}

\keyword{ package }
