\name{rastermeta}
\alias{rastermeta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create list containing rastermeta-information
}
\description{
Create valid objects for slot \code{rastermeta} in \code{\linkS4class{Speclib}}.
}
\usage{
rastermeta(x, dim, ext, crs) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    Optional. Object of one of the following classes: "Raster", "RasterBrick", "RasterStack", "HyperSpecRaster".
}
  \item{dim}{
    Optional. Vector with length == 2. The first and second elements give the number of rows and columns, respectively.
}
  \item{ext}{
    Optional. Object of class \code{extent}.
}
  \item{crs}{
    Optional. Object of class \code{CRS}.
}
}
\author{
Lukas Lehnert
}
\value{
 List with following elements (in exactly this order!):
  \itemize{
    \item{dim:}{ Vector with length == 2. The first and second elements give the number of rows and columns, respectively.}
    \item{ext:}{ Object of class \code{extent}.}
    \item{crs:}{ Object of class \code{CRS}.}
  }
}
\seealso{
\code{\linkS4class{Speclib}}, \code{\linkS4class{HyperSpecRaster}}
}

