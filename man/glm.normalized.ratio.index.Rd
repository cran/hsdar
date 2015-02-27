\name{glm.nri}
\alias{glm.nri}
\alias{lm.nri}
%\alias{print.glmnri}
%\alias{print.lmnri}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
(Generalised) Linear models from normalised ratio indices
}
\description{
Build (generalised) linear models of normalised ratio indices as response and predictor variables
}
\usage{
lm.nri(formula, preddata = NULL, ...)
glm.nri(formula, preddata = NULL, ...)
%\method{print}{glmnri}(x, ...)
%\method{print}{lmnri}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
Formula for (generalized) linear model
}
  \item{preddata}{
Data frame or speclib containing predictor variables
}
%\item{x}{
%Object of class \code{'(g)lmnri'}
%}
  \item{...}{
Further arguments passed to \code{\link{lm}}, \code{\link{glm}} and generic \code{\link{print.default}}
}
}
\details{
  NRI-values may be used as predictor or response variable. If NRI-values are predictors, the models are build only with one index as predictor instead of all available indices. In this case, only one predictor and one response variable is allowed, currently. For additional information on (generalised) linear models see details in \code{\link{lm}} and \code{\link{glm}}.
}
\value{
  \enumerate{
    \item{\code{lm.nri}:}{
      A list of class "lmnri" containing following items:
      \itemize{
        \item{Estimate:}{ Coefficient estimates for each index and term}
        \item{Std.Error:} { Standard errors}
        \item{t.value:}{ T-values}
        \item{p.value:}{ P-values}
        \item{r.squared:}{ R\eqn{^2} values}
      }
    }
    \item{\code{glm.nri}:}{
      A list of class "glmnri". List contains following items (depending on formula used):
      \itemize{
        \item{Estimate:}{ Coefficient estimates for each index and term}
        \item{Std.Error:} { Standard errors}
        \item{t.value/z.value:}{ T-values or Z-values}
        \item{p.value:}{ P-values}
      }
      
    }
  }
  See help pages for \code{\link{lm}} and \code{\link{glm}} for any additional information. Note that this function does not store the entire information returned from a normal (g)lm-model. To get full (g)lm-models use either function \code{\link{nri_best_performance}} to return best performing model(s) or extract nri-values with \code{\link{getNRI}} and build directly the model from respective index.
  
  See details in \code{\link[=plot.Nri]{plot}} for details on plotting.
}

\author{
Lukas Lehnert
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[=plot.Nri]{plot}}, \code{\link{lm}}, \code{\link{glm}}, \code{\link{getNRI}}
}
\examples{
data(spectral_data)

## Calculate all possible combinations for WorldView-2-8
spec_WV <- spectral.resampling(spectral_data, "WorldView2-8",
                               response_function = FALSE)
nri_WV <- nri(spec_WV, recursive = TRUE)

glmnri <- glm.nri(nri_WV ~ Soil.moisture, preddata = spec_WV)
glmnri

plot(glmnri)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{multivariate}
