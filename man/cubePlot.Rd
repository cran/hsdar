\name{cubePlot}
\alias{cubePlot}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
cubePlot
}
\description{
Plotting 3D cube of hyperspectral data using \pkg{rgl}-package
}
\usage{
cubePlot(x, r, g, b, ncol = 1, nrow = 1, 
         sidecol = colorRamp(palette(heat.colors(100))), ...)
}

\arguments{
  \item{x}{
Object of class \code{HyperSpecRaster}.
}
\item{r}{
Integer. Index of band used as red channel. If omitted, the band closest to 680 nm is selected.
}
\item{g}{
Integer. Index of band used as green channel. If omitted, the band closest to 540 nm is selected.
}
\item{b}{
Integer. Index of band used as blue channel. If omitted, the band closest to 470 nm is selected.
}
\item{ncol}{
Integer giving the column(s) in \code{x} which is/are used to plot the spectral dimension.
}
\item{nrow}{
Integer giving the row(s) in \code{x} which is/are used to plot the spectral dimension.
}
\item{sidecol}{
ColorRamp used to illustrate spectral dimension.
}
  \item{...}{
Further arguments (currently ignored)
}
}


\author{
Lukas Lehnert
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\linkS4class{HyperSpecRaster}}
}
\examples{
\dontrun{
data(spectral_data)
ras <- HyperSpecRaster(spectral_data, nrow = 9, ncol = 9)

cubePlot(ras)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
