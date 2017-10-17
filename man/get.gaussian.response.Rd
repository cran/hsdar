\name{get.gaussian.response}
\alias{get.gaussian.response}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gaussian response function
}
\description{
Simulate Gaussian response function for band(s) of a (satellite) sensor. Each band is either defined by center and full-width-half-maximum values or by passing its upper and lower border.
}
\usage{
get.gaussian.response(fwhm)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fwhm}{
Object of class \code{data.frame} with three columns. See details and examples sections.
}
}
\details{
The characteristics of the sensor must be passed as a \code{data.frame} with three columns: first column is used as name for bands, second with lower bounds of channels and third column with upper bounds (5\% sensitivity). Alternatively, the \code{data.frame} may encompass band centre wavelength and full-width-half-maximum values of the sensor. Function will check the kind of data passed by partially matching the names of the data frame: If any column is named \code{"fwhm"} or \code{"center"}, it is assumed that data are band centre and full-width-half-maximum values. 
}
\value{
Data frame with response values for all bands covering the entire spectral range of sensor passed to the function.
}

\author{
Lukas Lehnert
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{get.sensor.characteristics}}, \code{\link{spectralResampling}}
}
\examples{
par(mfrow=c(1,2))
## Plot response function of RapidEye
plot(c(0,1)~c(330,1200), type = "n", xlab = "Wavelength [nm]", 
     ylab = "Spectral response")
data_RE <- get.gaussian.response(get.sensor.characteristics("RapidEye"))
xwl_response <- seq.int(attr(data_RE, "minwl"),
                        attr(data_RE, "maxwl"),
                        attr(data_RE, "stepsize"))
for (i in 1:ncol(data_RE))
  lines(xwl_response, data_RE[,i], col = i)
  
## Plot original response function
data_RE <- get.sensor.characteristics("RapidEye", TRUE)

plot(c(0,1)~c(330,1200), type = "n", xlab = "Wavelength [nm]", 
     ylab = "Spectral response")
xwl_response <- seq.int(attr(data_RE$response, "minwl"),
                        attr(data_RE$response, "maxwl"),
                        attr(data_RE$response, "stepsize"))
for (i in 1:nrow(data_RE$characteristics))
  lines(xwl_response, data_RE$response[,i], col = i)
  
## Simulate gaussian response for arbitrary sensor with 3 bands
sensor <- data.frame(Name = paste("Band_", c(1:3), sep = ""),
                     center = c(450, 570, 680),
                     fwhm = c(30, 40, 30))

## Plot response function
par(mfrow=c(1,1))
plot(c(0,1)~c(330,800), type = "n", xlab = "Wavelength [nm]", 
     ylab = "Spectral response")
data_as <- get.gaussian.response(sensor)
xwl_response <- seq.int(attr(data_as, "minwl"),
                        attr(data_as, "maxwl"),
                        attr(data_as, "stepsize"))
for (i in 1:3)
  lines(xwl_response, data_as[,i], col = i)
}
\keyword{utilities}
