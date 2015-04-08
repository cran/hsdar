\name{DistMat3D-class}
\docType{class}
\alias{DistMat3D-class}

\title{
* DistMat3D class
}
\description{
Class to store effectively (large) distance matrices (up to 3D).
}
\details{
Object with 3 slots:
\itemize{
  \item{values: }{Numerical vector containing distance values}
  \item{ncol: }{Number of columns in the 3D-matrix. Number of columns equals always the number of rows}
  \item{nlyr: }{Number of layers in the 3D-matrix}
}
}

\note{
See figure in \code{\link{hsdar-package}} for an overview of classes in hsdar.
}

\author{
Lukas Lehnert
}


\seealso{
\code{\link{distMat3D}}
}
\keyword{classes}
