\name{nri_best_performance}
\alias{nri_best_performance}
\alias{mark_nri_best_performance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Best performing model(s) with NRI
}
\description{
Get or mark best performing model(s) between narrow band indices and environmental variables
}
\usage{
nri_best_performance(nri, n = 1, coefficient = "p.value", 
                     predictor = 2, abs = FALSE, findMax = FALSE, 
                     ...)
mark_nri_best_performance(best, glmnri, n = nrow(best$Indices), 
                          uppertriang = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nri}{
Object of class nri
}
\item{glmnri}{
Object of class glmnri
}
\item{n}{
Number of models to return or mark
}
  \item{coefficient}{
Name or index of coefficient to plot
}
\item{predictor}{
Name or index of term to plot
}
\item{abs}{
Use absolute value (e.g. for t-values)
}
\item{findMax}{
Find maximum or minimum values
}
\item{best}{
Output from \code{nri_best_performance}
}
\item{uppertriang}{
Flag to mark the upper triangle
}
  \item{...}{
Further arguments passed to \code{\link{glm}} function. These must be the same as used for initial creation of \code{\link{glm.nri}}. For \code{mark_nri_best_performance} arguments are passed to \code{\link{polygon}}.
}
}
\details{
  See details in \code{\link{glm.nri}} and \code{\link{glm}}.
}

\author{
Lukas Lehnert
}


\seealso{
\code{\link{glm.nri}}, \code{\link{glm}}
}
\examples{
data(spectral_data)

## Calculate all possible combinations for WorldView-2-8
spec_WV <- spectralResampling(spectral_data, "WorldView2-8",
                              response_function = FALSE)
nri_WV <- nri(spec_WV, recursive = TRUE)

## Build glm-models
glmnri <- glm.nri(nri_WV ~ chlorophyll, preddata = spec_WV)

## Return best 5 models
BM <- nri_best_performance(glmnri, n = 5, coefficient = "p.value")

## Get nri values for the 5 models
nri_BM <- getNRI(nri_WV, BM)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{multivariate}
