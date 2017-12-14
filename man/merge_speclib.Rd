\name{merge}
\alias{merge,Speclib,Speclib-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Merge speclibs
}
\description{
Merge two \code{Speclib}s and their SI data 
}
\usage{
\S4method{merge}{Speclib,Speclib}(x, y, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
1st Object of class \code{Speclib} to be merged.
}
  \item{y}{
2nd Object of class \code{Speclib} to be merged.
}
  \item{...}{
Further (optional) objects of class \code{Speclib}.
}
}

\value{
Object of class \code{Speclib}.
}
\author{
Lukas Lehnert
}

\seealso{
\code{\linkS4class{Speclib}}
}
\examples{
data(spectral_data)
sp1 <- spectral_data[c(1:10),]
sp2 <- spectral_data[c(11:20),]

## Merge two Speclibs
speclib_merged_1 <- merge(sp1, sp2)
nspectra(speclib_merged_1)

## Merge multiple Speclibs
sp3 <- spectral_data[c(21:30),]
speclib_merged_2 <- merge(sp1, sp2, sp3)
nspectra(speclib_merged_2)
}
\keyword{utilities}
