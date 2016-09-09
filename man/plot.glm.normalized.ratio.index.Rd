\name{plot.Nri}
\alias{plot.Nri}
\alias{plot,Nri-method}
\alias{plot,Nri,ANY-method}

\title{
Plot function for (g)lm.nri and cor.test.nri
}
\description{
Plot values in (generalised) linear modes and correlation tests from narrow band indices
}
\usage{
\S4method{plot}{Nri}(x, coefficient = "p.value", predictor = 2,                      
     xlab = "Wavelength band 1 (nm)",  
     ylab = "Wavelength band 2 (nm)", legend = TRUE,
     colspace = "hcl", col = c(10, 90, 60, 60, 10, 80),                
     digits = 2, range = "auto", constraint  = NULL,
     uppertriang = FALSE, ...)
}
\arguments{
  \item{x}{
Object to be plotted.
}
  \item{coefficient}{
Name or index of coefficient to plot.
}
\item{predictor}{
Name or index of term to plot.
}
\item{xlab}{
Label for x-axis.
}
\item{ylab}{
Label for y-axis.
}
\item{legend}{
Flag if legend is plotted. If \code{legend == "outer"} the legend is plotted in the outer margins of the figure. This is useful if both diagonals are used. 
}
\item{colspace}{
Either "hcl" or "rgb". Colour space to be used for the plots.
} 
\item{col}{
If colspace == "hcl", the vector is giving the minimum and maximum values of hue (element 1 & 2), chroma (element 3 & 4) and luminance (element 5 & 6). The optional element 7 is used as alpha value. See \code{\link{hcl}} for further explanation.
If colspace == "rgb", a vector of length >=2 giving the colours to be interpolated using \code{\link{colorRamp}}.
}
\item{digits}{
Precision of labels in legend.
}
\item{range}{
"auto" or a vector of length = 2 giving the range of values to be plotted.
}
\item{constraint}{
A character string giving a constraint which values should be plotted. See examples section.
}
\item{uppertriang}{
Flag if upper triangle is used for the plot. Note that if \code{TRUE} the current plot is used instead of starting a new plot
}
  \item{...}{
Further arguments passed to \code{plot.default}.
}
}
\details{
  See details in \code{\link{glm.nri}} and \code{\link{glm}}.
}
\value{
  An invisible vector with minimum and maximum values plotted.
}
\author{
Lukas Lehnert
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{nri}}, \code{\link{glm.nri}}, \code{\link{glm}}, \code{\link[=cor.test.nri]{cor.test}}, \code{\link[=t.test.nri]{t.test}}
}
\examples{
\dontrun{
data(spectral_data)

## Calculate all possible combinations for WorldView-2-8
spec_WV <- spectralResampling(spectral_data, "WorldView2-8",
                              response_function = FALSE)
nri_WV <- nri(spec_WV, recursive = TRUE)

## Fit generalised linear models between NRI-values and chlorophyll
glmnri <- glm.nri(nri_WV ~ chlorophyll, preddata = spec_WV)

## Plot p-values
plot(glmnri, range = c(0, 0.05))
## Plot t-values
plot(glmnri, coefficient = "t.value")
## Plot only t-values where p-values < 0.001
plot(glmnri, coefficient = "t.value", 
     constraint = "p.value < 0.001")

## Fit linear models between NRI-values and chlorophyll
lmnri <- lm.nri(nri_WV ~ chlorophyll, preddata = spec_WV)

## Plot r.squared
plot(lmnri)

## Example for EnMAP (Attention: Calculation time may be long!)
spec_EM <- spectralResampling(spectral_data, "EnMAP", 
                              response_function = FALSE)
mask(spec_EM) <- c(300, 550, 800, 2500)
nri_EM <- nri(spec_EM, recursive = TRUE)
glmnri <- glm.nri(nri_EM ~ chlorophyll, preddata = spec_EM)

## Plot T values in lower and p-values in upper diagonal
## of the plot
## Enlarge margins for legends
par(mar = c(5.1, 4.1, 4.1, 5))
plot(glmnri, coefficient = "t.value", legend = "outer")
plot(glmnri, coefficient = "p.value", uppertriang = TRUE)
lines(c(400,1705),c(400,1705))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}
