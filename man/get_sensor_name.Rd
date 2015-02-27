\name{get.sensor.name}
\alias{get.sensor.name}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Satellite sensor name
}
\description{
Get satellite sensor name by integer value
}
\usage{
get.sensor.name(sensor)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sensor}{
Integer value to match against predefined satellite sensors.
}
}
\details{
  See \code{\link{get.sensor.characteristics}} to get overview on available satellite sensors.
}
\value{
Name of satellite sensor as character string.
}

\author{
Lukas Lehnert
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{get.sensor.characteristics}}
}
\examples{
get.sensor.name(1)
}
\keyword{utilities}
