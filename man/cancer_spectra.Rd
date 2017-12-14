\name{cancer_spectra}
\alias{cancer_spectra}
\docType{data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Hyperspectral samples
}
\description{
Hyperspectral samples from the human larynx
}
\usage{
data(cancer_spectra)
}
\details{
This dataset contains hyperspectral data from the human larynx. The data were acquired in a project aiming to test the feasibility  to use hyperspectral imaging for the non-invasive detection of cancer of the human larynx (head-and-neck squamous cell carcinoma). In \pkg{hsdar}, a subset of the total dataset is kindly provided by the project. This subset includes hyperspectral images from 25 patients including 10 cases with histopathological diagnosis of cancer. The images were acquired using an endoscope which was coupled with a monochromatic CCD camera. As light source, a special Polychrome V light machine was used. This allowed to change the wavelength of the impinging radiation so that hyperspectral cubes could be acquired by combining several images taken under different illuminations. The images were preprocessed using the methodology proposed by Regeling et al. (2015).  The spectra were manually classified into cancerous and non-cancerous tissue by medical experts which is included in the SI of the data.
}

\format{An object of class \code{Speclib}}

\author{Bianca Regeling, Lukas Lehnert}

\references{
Regeling, B., Laffers, W., Gerstner, A.O.H., Westermann, S., Mueller, N.A., Schmidt, K., Bendix, J., Thies, B. (2015): Development of an Image Pre-processor for Operational Hyperspectral Laryngeal Cancer Detection. Journal of Biophotonics, 1-11.
}

\keyword{datasets}

