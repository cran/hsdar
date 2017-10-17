\name{SI}
\alias{SI}
\alias{SI.speclib}
\alias{SI,Speclib-method}
\alias{SI,Nri-method}
\alias{SI,Nri,ANY,ANY-method}
\alias{SI,Nri,ANY,missing-method}
\alias{SI,Nri,missing,ANY-method}
\alias{SI,Nri,missing,missing-method}
\alias{SI,Speclib,ANY,ANY-method}
\alias{SI,Speclib,ANY,missing-method}
\alias{SI,Speclib,missing,ANY-method}
\alias{SI,Speclib,missing,missing-method}

\alias{SI<-}
\alias{SI<-,Speclib,data.frame-method}
\alias{SI<-,Speclib,matrix-method}
\alias{SI<-,Speclib,ANY-method}

\alias{SI<-,Nri,ANY-method}
\alias{SI<-,Nri,data.frame-method}
\alias{SI<-,Nri,matrix-method}


\alias{[,.SI,ANY,ANY,ANY-method}
\alias{initialize,.SI-method}
\alias{names,.SI-method}
\alias{ncol,.SI-method}
\alias{nrow,.SI-method}

% \alias{setSI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Handling supplementary information (SI) of spectra
}
\description{
Supplementary information (SI) can be any additional data available for each spectrum in a \code{Speclib}- or \code{Nri}-object. These functions are used to set or return SI-data of a \code{Speclib} or \code{Nri}-object. Note that SI-data is automatically subsetted if indexing and extracting single spectra from a \code{Speclib}- or \code{Nri}-object. SI-data may encompass (several) raster files which must have the same extent, resolution and x- and y-dimensions as the raster file used as spectral information.
}
\usage{
\S4method{SI}{Speclib}(object)

\S4method{SI}{Speclib,ANY,ANY}(object, i, j)

\S4method{SI}{Speclib,data.frame}(object) <- value

\S4method{SI}{Speclib,matrix}(object) <- value

\S4method{SI}{Nri}(object)

\S4method{SI}{Nri,data.frame}(object) <- value

\S4method{SI}{Nri,matrix}(object) <- value

%setSI(x, SI, matchcol = NULL, fuzzy = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Object of class \code{Speclib} or \code{Nri}.
}
\item{i}{
Index of rows to keep. Note that in combination with raster files in the SI, it is MUCH faster to pass row index instead of cutting the resulting data frame. Thus, \code{SI(object, i)} should be used instead of \code{SI(object)[i,]}.
}
\item{j}{
Index of columns to keep. See comment above for usage with raster files in the SI.
}
  \item{value}{
Data frame with \code{nrow(value) == nspectra(object)}, NULL or vector with length \code{nspectra(object)}. Alternatively, objects of class \code{RasterLayer} are accepted. Please note that the function does not check for integrity in the latter case (e.g., no error will occur if number of spectra does not match number of pixel in the \code{RasterLayer}-object).
}
}
\value{
For \code{SI<-}, the updated object. \code{SI} returns a data frame with SI data.% The return value of \code{setSI} is an object of class speclib.
}
\details{
Names of items in \code{SI} are used within the function \code{\link{subset}} to select/deselect spectra via logical expression. Values can be accessed via the "\$"-sign (see examples).
}

\author{
Lukas Lehnert
}

\seealso{
\code{\linkS4class{Speclib}}, \code{\linkS4class{Nri}}
}
\examples{
data(spectral_data)

## Returning SI
si_spec <- SI(spectral_data)
head(si_spec)

## Adding new SI item
SI(spectral_data)$MeasurementID <- c(1:nspectra(spectral_data))
head(SI(spectral_data))

## Replacing SI
SI(spectral_data) <- SI(spectral_data)[,c(1:3)]
head(SI(spectral_data))

## Example for raster file in SI

## Create raster file using PROSAIL
## Run PROSAIL
parameter <- data.frame(N = c(rep.int(seq(0.5, 1.4, 0.1), 6)),
                        LAI = c(rep.int(0.5, 10), rep.int(1, 10), 
                                rep.int(1.5, 10), rep.int(2, 10), 
                                rep.int(2.5, 10), rep.int(3, 10)))
spectra <- PROSAIL(parameterList = parameter)

## Create SpatialPixelsDataFrame and fill data with spectra from 
## PROSAIL
rows <- round(nspectra(spectra)/10, 0)
cols <- ceiling(nspectra(spectra)/rows)
grd <- SpatialGrid(GridTopology(cellcentre.offset = c(1,1,1), 
                                cellsize = c(1,1,1), 
                                cells.dim = c(cols, rows, 1)))
x <- SpatialPixelsDataFrame(grd, 
                            data = as.data.frame(spectra(spectra)))

## Write data to example file (example_in.tif) in workingdirectory
writeGDAL(x, fname = "example_in.tif", drivername = "GTiff")

infile <- "example_in.tif"
wavelength <- wavelength(spectra)
ra <- speclib(infile, wavelength)
tr <- blockSize(ra)

## Write LAI to separate raster file
LAI <- SI(spectra)$LAI
SI_file <- "example_SI.tif" 
SI_raster <- setValues(raster(infile), LAI)
SI_raster <- writeRaster(SI_raster, SI_file)

## Read LAI file and calculate NDVI for each pixel where LAI >= 1
outfile <- "example_result_ndvi.tif" 
SI(ra) <- raster(SI_file)
names(SI(ra)) <- "LAI"
res <- writeStart(ra, outfile, overwrite = TRUE, nl = 1)
for (i in 1:tr$n) 
{
  v <- getValuesBlock(ra, row=tr$row[i], nrows=tr$nrows[i])
  mask(v) <- c(1350, 1450)
  LAI <- SI(v)$LAI
  v <- as.matrix(vegindex(v, index="NDVI"))
  v[LAI <= 1] <- NA
  res <- writeValues(res, v, tr$row[i])
}
res <- writeStop(res)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
