\name{DistMat3D-class}
\docType{class}
\alias{DistMat3D-class}

\title{
* DistMat3D class
}
\description{
Class to store effectively (large) distance matrices (up to 3D), which can be interpreted as a stack of traditional 2-D distance matrices. Therefore, the first two dimensions are of equal length and usually describe the wavelength in \pkg{hsdar}. This third dimension is normally the number of samples or pixels. In \pkg{hsdar},  objects of class \code{DistMat3D} are used e.g., to store \code{\link{nri}}-values. In this case, the first and second dimensions store the information which band #1 is substraced by which band #2, respectively. The third dimension is the sample. Since it usually does not matter if band #1 is substracted from band #2 or vice versa, the \code{\link{nri}}-matrix would contain the same absolute values on both triangles (as 2-D distance matrices would do). Therefore, \pkg{hsdar} defines and uses the class  \code{DistMat3D} in which only one triangle is stored and memory demand is considerably reduced.
}
\details{
S4-class with 3 slots:
\itemize{
  \item{values: }{Numerical vector containing distance values}
  \item{ncol: }{Number of columns in the 3D-matrix. Number of columns equals always the number of rows}
  \item{nlyr: }{Number of layers in the 3D-matrix}
}
The data in the values slot is organized as follows: The first value is the distance at band #1 and band #2 for sample number #1, the second one is for band #1 and band #3 (sample #1) and so forth. Methods to create objects of class \code{DistMat3D} for matrix and array objects exist. Additionally, methods to apply functions to the values exist.
}

\note{
See figure in \code{\link{hsdar-package}} for an overview of classes in \pkg{hsdar}.
}

\author{
Lukas Lehnert
}


\seealso{
\code{\link{distMat3D}}, \code{\link{apply.DistMat3D}}
}
\keyword{classes}
