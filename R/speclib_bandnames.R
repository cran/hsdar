bandnames <- function(x)
  return(attr(x, "bandnames"))


"bandnames<-" <- function(x, value)
{
  xx <- x
  attr(xx, "bandnames") <- value
  x <- xx
}