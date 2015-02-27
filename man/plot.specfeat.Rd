\name{plot.specfeat}
\alias{plot,Specfeat,ANY-method}

\title{
Plot function for class Specfeat
}
\description{
Plot spectra in Specfeat objects
}
\usage{
\S4method{plot}{Specfeat,ANY}(x, fnumber = 1, stylebysubset = NULL, changecol = TRUE,
          changetype = FALSE, autolegend = TRUE, new = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object to be plotted
}
  \item{fnumber}{
Subscript of feature(s) to be plotted
}
\item{stylebysubset}{
Name of column in attributes to be used for colour.
}
\item{changecol}{
Flag indicating if line colours change according to values in coloumn defined by stylebysubset
}
\item{changetype}{
Flag indicating if line types change according to values in coloumn defined by stylebysubset
}
\item{autolegend}{
Flag if legend is plotted. 
}
\item{new}{
  Flag if a new plot is started.
} 
  \item{...}{
Further arguments passed to \code{plot.default}
}
}


\author{
Lukas Lehnert
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{nri}}, \code{\link{glm.nri}}, \code{\link{glm}}, \code{\link{cor.test,Nri-method}}, \code{\link{t.test,Nri-method}}
}
\examples{
## See examples in specfeat
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}
