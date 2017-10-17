\name{cor.test}
\alias{cor.test.nri}
\alias{cor.test,Nri-method}
%\alias{print.cortestnri}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Test for association/correlation between nri values and vector of samples
}
\description{
Test for association between paired samples (with one variable being nri-values), using one of Pearson's product moment correlation coefficient, Kendall's tau or Spearman's rho.
}
\usage{
\S4method{cor.test}{Nri}(x, y, ...)

%\method{print}{cortestnri}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class \code{Nri} or numerical vector
}
  \item{y}{
Object of class \code{Nri} or numerical vector
}
  \item{...}{
Further arguments passed to \code{\link{cor.test}}
}
}
\details{
  NRI-values may be used as x and/or as y variable. If x and y are NRI-values the number of samples in both datasets must be equal. For additional information on correlation tests see details in \code{\link{cor.test}}.
}
\value{
  Object of class \code{\linkS4class{Nri}}
}

\author{
Lukas Lehnert
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  
\code{\link[=plot.Nri]{plot}}, \code{\link{cor.test}}, \code{\link{glm.nri}}, \code{\link{lm.nri}}, \code{\link{getNRI}}
}
\examples{
data(spectral_data)

## Calculate all possible combinations for WorldView-2-8
spec_WV <- spectralResampling(spectral_data, "WorldView2-8",
                              response_function = FALSE)
nri_WV <- nri(spec_WV, recursive = TRUE)

cortestnri <- cor.test(nri_WV, SI(spec_WV)$chlorophyll)

cortestnri
}

