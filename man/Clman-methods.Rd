\name{clman}
\docType{methods}
\alias{initialize,Clman-method}
\alias{spectra,Clman-method}
\alias{spectra<-,Clman,data.frame-method}
\alias{spectra<-,Clman,matrix-method}
\alias{spectra<-,Clman,numeric-method}
\alias{plot,Clman-method}

\title{
Methods to create, manipulate and query objects of class 'Clman'.
}
\description{
Methods to create, manipulate and query objects of class 'Clman'. The class 'Clman' is used to store manually defined continuum lines and the associated spectra.
}
\usage{
## Creation of objects
\S4method{initialize}{Clman}(.Object, ...)  
  
\S4method{spectra}{Clman}(object, ...)

\S4method{spectra}{Clman,data.frame}(object) <- value

\S4method{spectra}{Clman,matrix}(object) <- value

\S4method{spectra}{Clman,numeric}(object) <- value

\S4method{plot}{Clman}(x, ispec, subset = NULL, numeratepoints = TRUE, 
    hull.style = NULL, points.style = list(), ...)

}

\arguments{
\item{.Object,object}{
  Matrix, numeric or array in cases of creation of 'Clman' objects otherwise object of class 'Clman'.
}
\item{value}{
  Object of class numeric, matrix or array which is used for replacement of the values in x.
}
\item{...}{
  Arguments passed to \code{\link{createspeclib}} or \code{plot.default}.
}
\item{x}{
Object of class \code{clman}.
}
\item{ispec}{
Name or index of spectrum to be plotted.
}
\item{subset}{
Lower and upper spectral limits used for plot.
}
\item{numeratepoints}{
Flag if points should be numerated in plot.
}
\item{hull.style}{
List of arguments passed to \code{\link{lines}} to construct the continuum line.
}
\item{points.style}{List of arguments passed to \code{\link{points}} to construct the continuum points. May be \code{NULL} to suppress plotting of fix points.}
}

\value{
For \code{spectra<-}, the updated object. Otherwise a matrix returning the spectra in the Clman object.
}
\note{
  The functions to create objects of class \code{Clman} are mainly internally needed by \code{\link{transformSpeclib}}.
}
\author{
Lukas Lehnert
}
\examples{
## Model spectra using PROSAIL
parameter <- data.frame(N = rep.int(c(1, 1.5),2), LAI = c(1,1,3,3))
spec <- PROSAIL(parameterList=parameter)

## Transform spectra
spec_clman <- transformSpeclib(spec, method = "sh", out = "raw")

## Return first spectrum
spectra(spec_clman)[1,]

## Plot clman
plot(spec_clman, ispec = 1, subset = c(400, 1000))
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{dist.speclib}}, \code{\link{Clman}}, \code{\link{transformSpeclib}}, \code{\link[=plot.Speclib]{plot}}
}

\keyword{aplot}

