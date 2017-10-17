\name{Clman}

\docType{class}
\alias{Clman}

\title{
* Clman class
}
\description{
Class to store and handle manual continuum lines. 
}


\details{
  The class is only required if a continuum line is manually adopted or entirely manually created. This is useful if the automatic approaches are not able to identify absorption features because, for instance, the spectrum has two pronounced maxima within the absortion feature of interest. 
  
  \code{Clman} is defined as \code{\linkS4class{Speclib}} extended by the following two slots:
  \itemize{
  \item{cp:}{ Matrix containing the fix points (continuum points) of each spectrum.}
  \item{hull:}{ Matrix containing the hull of each spectrum.}
  }
  
  Normally, it is not necessary to manually change the values in any of the slots above. Use the functions \code{\link{addcp}} and \code{\link{deletecp}} to change the hulls manually. Functionality for conversion back to a \code{\linkS4class{Speclib}} with the final hull and the transformed spectra provides function \code{\link{updatecl}}.
}
\note{
See figure in \code{\link{hsdar-package}} for an overview of classes in hsdar.
}
\author{
Lukas Lehnert and Hanna Meyer
}


\seealso{
\code{\link{transformSpeclib}}, \code{\link[=plot.Speclib]{plot}}, \code{\linkS4class{Speclib}}, \code{\link{addcp}}, \code{\link{deletecp}}, \code{\link{updatecl}}
}

\keyword{classes}

