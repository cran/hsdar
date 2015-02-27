\name{idSpeclib}
\alias{idSpeclib}
\alias{idSpeclib<-}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Handling IDs of spectra
}
\description{
Returning and setting ID of spectra in Speclib
}
\usage{
idSpeclib(x)
idSpeclib(x) <- value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class \code{Speclib}.
}
  \item{value}{
Character vector of the same length as \code{nspectra(x)}, or NULL.
}
}

\value{
For \code{idSpeclib<-}, the updated object. Otherwise a vector giving the ID of each spectrum in Speclib is returned.
}

\author{
Lukas Lehnert
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\linkS4class{Speclib}}
}
\examples{
data(spectral_data)

idSpeclib(spectral_data)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
