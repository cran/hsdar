idSpeclib <- function(x)
{
if (!is.speclib(x))
  stop("Class of x must be Speclib")
return(if (length(x@ID) == 0) c(1:nspectra(x)) else x@ID)
}

"idSpeclib<-" <- function(x, value)
{
  xx <- x
  xx@ID <- value
  x <- xx
}
