\name{speclib_raster-methods}
\alias{blockSize,Speclib-method}
\alias{getValuesBlock,Speclib-method}
\alias{writeStart,Speclib,character-method}
\alias{writeValues,Speclib,Speclib-method}
\alias{writeValues,Speclib,matrix-method}
\alias{writeValues,Speclib,numeric-method}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Functions for processing of large hyperspectral raster files
}
\description{
Functions for processing of large hyperspectral raster files using the low-level functions provided by the raster-package. For a detailed overview see the vignette "Writing functions for large raster files" shipped along with the raster-package.
}
\usage{
\S4method{blockSize}{Speclib}(x)

\S4method{writeStart}{Speclib,character}(x, filename, ...) 

\S4method{getValuesBlock}{Speclib}(x, ...)

\S4method{writeValues}{Speclib,Speclib}(x, v, start) 

\S4method{writeValues}{Speclib,matrix}(x, v, start) 

\S4method{writeValues}{Speclib,numeric}(x, v, start) 
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
    Further arguements passed to respective functions in the raster-packages.
}
}

\author{Lukas Lehnert}



