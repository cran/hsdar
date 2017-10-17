\name{apply.Speclib}
\alias{apply.Speclib}
\alias{apply,Speclib-method}

\title{
Apply function for class Speclib
}
\description{
Apply function over all spectra or a subset of spectra
}
\usage{
\S4method{apply}{Speclib}(X, FUN, bySI = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
Object of class \code{Speclib}
}
  \item{FUN}{
Function to be applied. Matched with \code{\link{match.fun}}.
}
  \item{bySI}{
Character string giving the name of the column in the SI to be used as subsets to apply function FUN on.
}
\item{...}{
Further arguments passed to FUN.
}
}

\value{
Object of class Speclib.
}

\author{
Lukas Lehnert
}

\seealso{
\code{\link{apply}}, \code{\link{match.fun}}, \code{\linkS4class{Speclib}}
}
\examples{
data(spectral_data)

mean_spectrum <- apply(spectral_data, FUN = mean)
plot(mean_spectrum)

## Same as above but seperately for both seasons
mean_spectra <- apply(spectral_data, FUN = mean, bySI = "season")
plot(mean_spectra, FUN = 1, ylim = c(0,50))
plot(mean_spectra, FUN = 2, new = FALSE)
SI(mean_spectra)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

