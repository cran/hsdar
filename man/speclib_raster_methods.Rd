\name{speclib_raster-methods}
\alias{blockSize,Speclib-method}
\alias{getValuesBlock,Speclib-method}
\alias{writeStart,Speclib,character-method}
\alias{writeValues,Speclib,Speclib-method}
\alias{writeValues,Speclib,matrix-method}
\alias{writeValues,Speclib,numeric-method}
\alias{writeStop,Speclib-method}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Functions for processing large hyperspectral raster files
}
\description{
Functions for processing large hyperspectral raster files using the low-level functions provided by the \pkg{raster}-package. For a detailed overview see the vignette "Writing functions for large raster files" shipped along with the \pkg{raster}-package.
}
\usage{
\S4method{blockSize}{Speclib}(x)

\S4method{writeStart}{Speclib,character}(x, filename, ...) 

\S4method{getValuesBlock}{Speclib}(x, ...)

\S4method{writeValues}{Speclib,Speclib}(x, v, start) 

\S4method{writeValues}{Speclib,matrix}(x, v, start) 

\S4method{writeValues}{Speclib,numeric}(x, v, start) 

\S4method{writeStop}{Speclib}(x) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    Object of class Speclib.
}
  \item{filename}{
    Name of the new file to create.
}
  \item{v}{
    Object to write the data to file. May be one of the following classes: "Speclib", "matrix" or "numeric".
}
  \item{start}{
    Integer. Row number (counting starts at 1) from where to start writing v.
}
\item{...}{
    Further arguements passed to respective functions in the \pkg{raster}-packages.
}
}

\examples{ 
\dontrun{
## Create raster file using PROSAIL
## Run PROSAIL
parameter <- data.frame(N = c(rep.int(seq(0.5, 1.4, 0.1), 6)),
                        LAI = c(rep.int(0.5, 10), rep.int(1, 10), 
                                rep.int(1.5, 10), rep.int(2, 10), 
                                rep.int(2.5, 10), rep.int(3, 10)))
spectra <- PROSAIL(parameterList = parameter)

## Create SpatialPixelsDataFrame and fill data with spectra from PROSAIL
rows <- round(nspectra(spectra)/10, 0)
cols <- ceiling(nspectra(spectra)/rows)
grd <- SpatialGrid(GridTopology(cellcentre.offset = c(1,1,1), 
                                cellsize = c(1,1,1), 
                                cells.dim = c(cols, rows, 1)))
x <- SpatialPixelsDataFrame(grd, data = as.data.frame(spectra(spectra)))

## Write data to example file (example_in.tif) in workingdirectory
writeGDAL(x, fname = "example_in.tif", drivername = "GTiff")

## Examples for Speclib using file example_in.tif
## Example 1:
## Noise reduction in spectra
infile <- "example_in.tif"
outfile <- "example_result_1.tif"
wavelength <- spectra$wavelength

ra <- speclib(infile, wavelength)
tr <- blockSize(ra)

res <- writeStart(ra, outfile, overwrite = TRUE)
for (i in 1:tr$n) 
{
  v <- getValuesBlock(ra, row=tr$row[i], nrows=tr$nrows[i])
  v <- noiseFiltering(v, method="sgolay", n=25)
  res <- writeValues(res, v, tr$row[i])
}
res <- writeStop(res)

## Example 2:
## masking spectra and calculating vegetation indices
outfile <- "example_result_2.tif" 
n_veg <- as.numeric(length(vegindex()))
res <- writeStart(ra, outfile, overwrite = TRUE, nl = n_veg)
for (i in 1:tr$n) 
{
  v <- getValuesBlock(ra, row=tr$row[i], nrows=tr$nrows[i])
  mask(v) <- c(1350, 1450)
  v <- as.matrix(vegindex(v, index=vegindex()))
  res <- writeValues(res, v, tr$row[i])
}
res <- writeStop(res)
}
}
\author{Lukas Lehnert}



