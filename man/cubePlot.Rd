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
         sidecol = colorRamp(palette(heat.colors(100))), 
         z_interpolate = FALSE, ...)
}

\arguments{
  \item{x}{
Object of class \code{Speclib}.
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
\item{z_interpolate}{
  Interpolate spectral dimension. This is useful if a cube is plotted which has a much larger spatial compared to spectral dimension. If \code{TRUE} the spectral dimension will be interpolated to the minimum of the spatial dimension. Alternatively, an integer value may be passed.
}
  \item{...}{
Further arguments passed to \code{\link{plotRGB}} for the top of the cube. Currently, the following two arguments are supported:
\itemize{
\item{\code{scale}: }{Maximum (possible) value in the three channels. Defaults to the maximum value in the red, green and blue band selected by arguments \code{r}, \code{g} and \code{b}.}
\item{\code{stretch}: }{Option to stretch the values to increase the contrast of the image: "lin" (default) or "hist"}
}
}
}


\author{
Lukas Lehnert
}
\note{The function may take a lot of time if the images are large. Consider plotting a subset of the entire image instead of plotting the entire image. Please note that the example below demonstrates the functionality with a very small image.

For unknown reasons, it may be necessary to execute the function twice in order to get the right colors at the walls of the cube.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\linkS4class{Speclib}}
}
\examples{
\dontrun{
## Create raster file using PROSPECT D
## Run PROSPECT for 1600 random chlorophyll content values
parameter <- data.frame(Cab = round(runif(1600, min = 10, max = 40), 0))
spectra <- PROSPECT(parameterList = parameter)
## Create SpatialPixelsDataFrame and fill data with spectra from PROSPECT
rows <- round(nspectra(spectra)/40, 0)
cols <- ceiling(nspectra(spectra)/rows)
grd <- SpatialGrid(GridTopology(cellcentre.offset = c(1,1,1),
cellsize = c(1,1,1),
cells.dim = c(cols, rows, 1)))
x <- SpatialPixelsDataFrame(grd, data = as.data.frame(spectra(spectra)))
## Write data to example file (example_in.tif) in workingdirectory
writeGDAL(x, fname = "example_in.tif", drivername = "GTiff")

## Read the raster and plot 3D cube
wavelength <- wavelength(spectra)
ras <- speclib("example_in.tif", wavelength)

cubePlot(ras)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
