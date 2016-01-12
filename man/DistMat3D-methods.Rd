\name{distMat3D}
\docType{methods}
\alias{distMat3D}
\alias{show,DistMat3D-method}
\alias{as.array,DistMat3D-method}
\alias{as.matrix,DistMat3D-method}
\alias{dim,DistMat3D-method}
\alias{ncol,DistMat3D-method}
\alias{nrow,DistMat3D-method}
\alias{distMat3D,matrix-method}
\alias{distMat3D,numeric-method}
\alias{distMat3D,array-method}
\alias{[,DistMat3D,ANY,ANY-method}
\alias{[,DistMat3D,ANY,ANY,ANY-method}
\alias{[<-,DistMat3D,ANY,ANY-method}
\alias{[<-,DistMat3D,ANY,ANY,ANY-method}
\alias{<,DistMat3D,ANY-method}
\alias{<=,DistMat3D,ANY-method}
\alias{>,DistMat3D,ANY-method}
\alias{>=,DistMat3D,ANY-method}
\alias{==,DistMat3D,ANY-method}
\alias{<,DistMat3D,DistMat3D-method}
\alias{<=,DistMat3D,DistMat3D-method}
\alias{>,DistMat3D,DistMat3D-method}
\alias{>=,DistMat3D,DistMat3D-method}
\alias{==,DistMat3D,DistMat3D-method}
\alias{<,ANY,DistMat3D-method}
\alias{<=,ANY,DistMat3D-method}
\alias{>,ANY,DistMat3D-method}
\alias{>=,ANY,DistMat3D-method}
\alias{==,ANY,DistMat3D-method}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Methods to create, manipulate and query objects of class 'DistMat3D'.
}
\description{
Methods to create, manipulate and query objects of class 'DistMat3D'. The following relational operators are defined to compare values between 'DistMat3D'-object(s): \code{<, <=, ==, >, >=}
}
\usage{
## Creation of objects
\S4method{distMat3D}{numeric}(x, ncol, nlyr)  
  
\S4method{distMat3D}{matrix}(x, lower_tri = TRUE)

\S4method{distMat3D}{array}(x, lower_tri = TRUE)

## Conversion methods
\S4method{as.array}{DistMat3D}(x)

\S4method{as.matrix}{DistMat3D}(x, lyr = 1)

## Query of properties
\S4method{dim}{DistMat3D}(x)

\S4method{ncol}{DistMat3D}(x)

\S4method{nrow}{DistMat3D}(x)

## Manipulate and query data in objects
\S4method{[}{DistMat3D,ANY,ANY}(x, i, j, n)

\S4method{[}{DistMat3D,ANY,ANY}(x, i, j, n) <- value


\S4method{show}{DistMat3D}(object)
}

\arguments{
\item{x,object}{
  Matrix, numeric or array in cases of creation of 'DistMat3D' objects otherwise object of class 'DistMat3D'.
}
\item{ncol}{
  Number of columns in the new 'DistMat3D' object.
}
\item{nlyr}{
  Number of layer in the new 'DistMat3D' object.
}
\item{lower_tri}{
  Flag if only the lower triangle is used.
}
\item{lyr}{
  Layer in the 'DistMat3D' object to be transformed into matrix.
}
\item{value}{
  Object of class numeric, matrix or array which is used for replacement of the values in x.
}
\item{i,j,n}{
  Subscripts to access data.
}
}


\author{
Lukas Lehnert
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\linkS4class{DistMat3D}},  \code{\link[=apply.DistMat3D]{apply}}, \code{\linkS4class{Nri}}
}
\examples{
data(spectral_data)

## Mask channel crossing part (arround 1050 nm) and strong 
## water absorption part (above 1350 nm)
mask(spectral_data) <- c(1045, 1055, 1350, 1706)

## Calculate SAM distances (object of class 'dist')
sam_dist <- dist.speclib(subset(spectral_data, season == "summer"))

## Convert to class 'distMat3D'
sam_dist <- distMat3D(as.matrix(sam_dist))

sam_dist
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{classes}
