\name{get.sensor.characteristics}
\alias{get.sensor.characteristics}
\alias{list.available.sensors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sensor characteristics
}
\description{
Get channel wavelength of satellite sensor
}
\usage{
get.sensor.characteristics(sensor, response_function = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sensor}{
Character or integer. Name or numerical abbreviation of sensor. See 'sensor="help"' or 'sensor=0' for an overview of available sensors.
}
  \item{response_function}{
If TRUE, the spectral response function is returned
}
}

\author{
Lukas Lehnert
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{spectralResampling}}
}
\examples{
## Return implemented sensors
get.sensor.characteristics(0)

## RapidEye
data_wv <- get.sensor.characteristics("RapidEye", TRUE)

## Plot response functions
plot(c(0,1)~c(330,1200), type = "n", xlab = "Wavelength [nm]", 
     ylab = "Spectral response")
xwl_response <- seq.int(attr(data_wv$response, "minwl"),
                        attr(data_wv$response, "maxwl"),
                        attr(data_wv$response, "stepsize"))
for (i in 1:nrow(data_wv$characteristics))
  lines(xwl_response, data_wv$response[,i], col = i)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
