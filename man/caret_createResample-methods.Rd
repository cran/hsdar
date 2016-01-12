\name{caret::createResample-methods}
\docType{methods}
\alias{createResample-methods}
\alias{createResample,ANY-method}
\alias{createResample,.CaretHyperspectral-method}
\title{Methods for Function \code{createResample}}
\description{
Methods for function \code{createResample} in package \pkg{caret}
}
\section{Methods}{
\describe{

\item{\code{signature(y = ".CaretHyperspectral")}}{
  Wrapper method for \code{\link{createResample}}.
  \cr
  Note that \code{".CaretHyperspectral"} is a class union containing classes
  \Sexpr[echo=TRUE,results=rd]{
    paste(hsdar:::.getCaretCompatibleClasses(), collapse = ", ")
  }.
}
}}
\keyword{methods}
