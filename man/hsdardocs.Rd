\name{hsdardocs}
\alias{hsdardocs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Load additional documents
}
\description{
Access help documents and references for different methods.
}
\usage{
hsdardocs(doc)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{doc}{
Name of document to load. Currently, only \code{"References.pdf"} and 
\code{"Copyright"} are available
}

}
\author{
Lukas Lehnert
}


\examples{
\dontrun{
## Open references of hyperspectral vegetation indices (PDF-file)
hsdardocs("References.pdf")

## See copyrights of routines and data used in hsdar-package (ascii-file)
hsdardocs("Copyright")
}
}
\keyword{ utilities }
\keyword{ documentation }
