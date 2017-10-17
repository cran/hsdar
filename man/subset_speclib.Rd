\name{subset.speclib}
\alias{subset.speclib}
\alias{subset,Speclib-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Subsetting speclibs
}
\description{
 Return subsets of \code{Speclib}s which meet conditions.
}
\usage{
\S4method{subset}{Speclib}(x, subset, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class 'Speclib'.
}
  \item{subset}{
Logical expression indicating spectra to keep: missing values are taken as false. See details section. 
}
\item{...}{
Further arguments passed to \code{\link{agrep}}.
}
}
\details{
Matchable objects are SI data. Use column names to identify the respective SI. See \code{\link{SI}} to access SI of a \code{Speclib}. IDs of spectra may be accessed using "id.speclib" as variable name. To subset certain wavelength ranges of a \code{Speclib} refer to \code{\link{mask}}.
}
\value{
Object of class \code{Speclib}.
}

\author{
Lukas Lehnert
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\linkS4class{Speclib}}, \code{\link{SI}}, \code{\link{mask}}
}
\examples{
%   \dontrun{
data(spectral_data)

## Return names of SI data
names(SI(spectral_data))

## Devide into both seasons
sp_summer <- subset(spectral_data, season == "summer")
sp_spring <- subset(spectral_data, season == "spring")

## Plot both speclibs
plot(sp_summer, col="darkgreen")
plot(sp_spring, col="darkred", new=FALSE)
%   }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
