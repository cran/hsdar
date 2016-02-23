\name{clman}
\docType{methods}
\alias{initialize,Clman-method}
\alias{spectra,Clman-method}
\alias{spectra<-,Clman,data.frame-method}
\alias{spectra<-,Clman,matrix-method}
\alias{spectra<-,Clman,numeric-method}

\title{
Methods to create, manipulate and query objects of class 'Clman'.
}
\description{
Methods to create, manipulate and query objects of class 'Clman'.
}
\usage{
## Creation of objects
\S4method{initialize}{Clman}(.Object, ...)  
  
\S4method{spectra}{Clman}(object, ...)

\S4method{spectra}{Clman,data.frame}(object) <- value

\S4method{spectra}{Clman,matrix}(object) <- value

\S4method{spectra}{Clman,numeric}(object) <- value
}

\arguments{
\item{.Object,object}{
  Matrix, numeric or array in cases of creation of 'Clman' objects otherwise object of class 'Clman'.
}
\item{value}{
  Object of class numeric, matrix or array which is used for replacement of the values in x.
}
\item{...}{
  Arguments passed to \code{\link{createspeclib}}.
}
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


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{dist.speclib}}, \code{\link{Clman}}, \code{\link{transformSpeclib}}, \code{\link[=plot.Speclib]{plot}}
}

