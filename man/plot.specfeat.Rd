\name{plot.Specfeat}
\alias{plot,Specfeat,ANY-method}

\title{
Plot Specfeat
}
\description{
Plot spectra in  objects of class \code{\linkS4class{Specfeat}}. Specfeats contain spectral data after applying a transformation such as continuum removal (see function \code{\link{transformSpeclib}}.
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
Name of column in SI table to be used for colour.
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
  Flag if a new plot should be started.
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
\code{\link{nri}}, \code{\link{glm.nri}}, \code{\link{glm}}, \code{\link{cor.test,Nri-method}}, \code{\link{t.test,Nri-method}}, \code{\linkS4class{Specfeat}}
}
\examples{
\dontrun{
data(spectral_data)

## Transform speclib
bd <- transformSpeclib(spectral_data, method = "sh", out = "bd")

## Define features automatically
features <- define.features(bd)

##Example to isolate the features around 450nm, 700nm, 1200nm and 1500nm.
featureSelection <- specfeat(features, c(450,700,1200,1500))

## Plot features
plot(featureSelection, 1:4)

## Advanced plotting example
plot(featureSelection, 1:4, stylebysubset = "season")

plot(featureSelection, 1:4, stylebysubset = "season", changecol = FALSE, 
     changetype = TRUE)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}
