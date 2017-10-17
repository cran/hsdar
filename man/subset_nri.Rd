\name{subset.nri}
\alias{subset.nri}
\alias{subset,Nri-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Subsetting \code{Nri}-objects
}
\description{
 Return subsets of \code{Nri}-objects which meet conditions.
}
\usage{
\S4method{subset}{Nri}(x, subset, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class 'Nri'.
}
  \item{subset}{
Logical expression indicating spectra to keep: missing values are taken as false. See details section. 
}
\item{...}{
Further arguments passed to \code{\link{agrep}}.
}
}
\details{
Matchable objects are SI data. Use column names to identify the respectrive SI. See \code{\link{SI}} to access SI of a \code{Nri}. IDs of samples may be accessed using "id.nri" as variable name. 
}
\value{
Object of class \code{Nri}.
}

\author{
Lukas Lehnert
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\linkS4class{Nri}}, \code{\link{SI}}
}
\examples{
%   \dontrun{
data(spectral_data)

## Calculate all possible combinations for WorldView-2-8
spec_WV <- spectralResampling(spectral_data, "WorldView2-8",
                              response_function = FALSE)
nri_WV <- nri(spec_WV, recursive = TRUE)

## Return names of SI data
names(SI(nri_WV))

## Devide into both seasons
sp_summer <- subset(nri_WV, season == "summer")
sp_spring <- subset(nri_WV, season == "spring")

## Print both Nri-objects
sp_summer
sp_spring
%   }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
