\name{usagehistory}
\alias{usagehistory}
\alias{usagehistory<-}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
History of usage
}
\description{
Function to read and write history of usage for \code{Speclibs}. Similar to a log file, the history of usage records processing steps applied to a \code{Speclib}.
}
\usage{
usagehistory(x)
usagehistory(x) <- value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class Speclib
}
  \item{value}{
Character string to be added to usagehistory or NULL, if usagehistory should be deleted.
}
}

\value{
For \code{usagehistory<-}, the updated object. Otherwise a vector containing the history of usage of \code{Speclib} is returned.
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

## Return history of usage
usagehistory(spectral_data)

## Deleting history of usage
usagehistory(spectral_data) <- NULL
spectral_data

## Adding entries
usagehistory(spectral_data) <- "New entry"   ## Adding new entry
usagehistory(spectral_data) <- "New entry 2" ## Adding second entry
spectral_data

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
