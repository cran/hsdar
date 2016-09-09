hsdar_parallel <- function()
{
  sort(c("transformSpeclib", "glm.nri"
         ))
}

.process_parallel <- function()
{
  process_parallel <- list(parallel = any(search() == "package:doMC"), dofun = NULL, donestedfun = NULL)
  if (process_parallel[[1]])
  {
    if (.getParallel())
    {   
      loadNamespace("foreach")
      process_parallel$dofun <- if (foreach::getDoParWorkers() > 1) foreach::`%dopar%` else  foreach::`%do%`
      process_parallel$donestedfun <- foreach::`%:%`
    } else {
      process_parallel[[1]] <- FALSE
    }
  }  
  return(process_parallel)
}

.getParallel <- function()
{
  res <- .Fortran("adminparallel",
                  flag    = as.integer(1),
                  process = as.integer(0),
                  PACKAGE = "hsdar"
                 )$process
  return(res == -1)
}
  
  
.restoreParallel <- function()
{
  res <- .Fortran("adminparallel",
                  flag    = as.integer(0),
                  process = as.integer(0),
                  PACKAGE = "hsdar"
                 )
}
