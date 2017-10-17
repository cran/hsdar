glm.nri <- function(formula, preddata=NULL, ...)
{  
  nri_response <- 0
  mc <- match.call()
  if (any(names(mc)=="family"))
  {
    fam <- mc[names(mc)=="family"]
    fam <- as.character(fam$family)
    names_table <- switch(fam,
                          binomial = c("Estimate", "Std.Error", "z.value", "p.value"),
                          poisson = c("Estimate", "Std.Error", "z.value", "p.value"),
                          c("Estimate", "Std.Error", "t.value", "p.value")
                   )

  } else {
    names_table <- c("Estimate", "Std.Error", "t.value", "p.value")
  }

  formula <- as.formula(formula)
  mc <- formula



  if (!is.null(preddata))
  {
    if (is.speclib(preddata))
      preddata <- SI(preddata)
    data <- preddata
#     attach(data)
    formula <- terms(formula, data=data)
  } else {
    formula <- terms(formula)
    data <- NULL
  }
  vars <- all.vars(formula)
  for (i in 1:length(vars))
  {
    x <- try(eval(parse(text = vars[i])),
             silent = TRUE
            )
    if (inherits(x, "try-error"))
      x <- eval(parse(text = vars[i]), data, environment(formula))
    if (class(x)=="Nri")
    {
      if (is.element(vars[i], attr(formula,"term.labels")))
      {
        nri_response <- 1
      } else {
        if (!is.element(vars[i],row.names(attr(formula,"factors"))))
          stop("Could not determine which variable contains nri-values. This may be caused by a function to be applied to nri-values")
        nri_response <- -1
      }
    }
  }

  if (nri_response==0)
    stop("Could not determine which variable contains nri-values")

  pp <- .process_parallel()
  
  if (nri_response==-1)
  {
    formula_apply <- update.formula(mc, response ~ .)
    response <- as.character(mc)[2]
    x <- eval(parse(text = response))
    response <- paste(response,"nri",sep="@")
    response <- eval(parse(text = response))

    mat <- match(all.vars(formula_apply)[-1], names(data))

    if (length(mat)==1)
    {
      predictors <- data.frame(a=data[,mat])
      names(predictors) <- names(data)[mat]
    } else {
      predictors <- data[,mat]
    }
    
    if (pp[[1]])
    {
      `%op%` <- pp[[2]]
      `%opnested%` <- pp[[3]]
      icol <- NULL
      irow <- NULL
      glm_data <- foreach::foreach(icol = c(1:(dim(response)[2]-1)), .combine = 'rbind') %opnested%
        foreach::foreach(irow = c((icol+1):dim(response)[1]), .combine = 'rbind') %op%
          {
            .glm_apply_lh(response[irow,icol,], formula_apply, predictors, ...)
          }
      glm_data <- rbind(matrix(glm_data[,1], nrow = 2), matrix(glm_data[,2], nrow = 2),
                        matrix(glm_data[,3], nrow = 2), matrix(glm_data[,4], nrow = 2))
      .restoreParallel()
    } else {
      glm_data <- apply(response, MARGIN = c(1, 2), FUN = .glm_apply_lh, formula_apply, predictors, ...)
    }
  } else {
    if (length(vars) > 2)
      stop("Models with more than 2 variables with nri values as predictors not supported, yet")
    predictor <- as.character(mc)[3]
    x <- eval(parse(text = predictor))
    predictor <- paste(predictor,"nri",sep="@")
    predictor <- eval(parse(text = predictor))

    formula_apply <- update.formula(mc, . ~ predictor)

    mat <- match(all.vars(formula_apply)[1], names(data))

    response <- data.frame(a=data[,mat])
    names(response) <- names(data)[mat]
 
    if (pp[[1]])
    {
      `%op%` <- pp[[2]]
      `%opnested%` <- pp[[3]]
      icol <- NULL
      irow <- NULL
      glm_data <- foreach::foreach(icol = c(1:(dim(predictor)[2]-1)), .combine = 'rbind') %opnested%
        foreach::foreach(irow = c((icol+1):dim(predictor)[1]), .combine = 'rbind') %op% {
          .glm_apply_rh(predictor[irow,icol,], formula_apply, response, ...)
        }
      glm_data <- rbind(matrix(glm_data[,1], nrow = 2), matrix(glm_data[,2], nrow = 2),
                        matrix(glm_data[,3], nrow = 2), matrix(glm_data[,4], nrow = 2))
      .restoreParallel()
    } else {
      glm_data <- apply(predictor, MARGIN = c(1, 2), FUN = .glm_apply_rh, formula_apply, response, ...)
    }
  }
  ncol = dim(glm_data)[1]/4

  nam <- NULL
  nam[[1]] <- c("(Intercept)", attr(formula,"term.labels"))
  nam[[2]] <- paste("B_",x$wavelength, sep="")
  nam[[3]] <- paste("B_",x$wavelength, sep="")

  final <- list(estimate = distMat3D(as.numeric(t(glm_data[1:ncol,])),
                                     length(x$wavelength),
                                     ncol),
                std.error = distMat3D(as.numeric(t(glm_data[c((ncol+1):(2*ncol)),])),
                                     length(x$wavelength),
                                     ncol),
                t.value = distMat3D(as.numeric(t(glm_data[c((2*ncol+1):(3*ncol)),])),
                                     length(x$wavelength),
                                     ncol),
                p.value = distMat3D(as.numeric(t(glm_data[c((3*ncol+1):(4*ncol)),])),
                                     length(x$wavelength),
                                     ncol)
           )
  names(final) <- names_table
  
  attr(final,"call") <- mc
  attr(final,"function") <- "glm"
  if (nri_response==-1)
  {
    attr(final,"is.predictor.nri") <- FALSE
    attr(final,"predictors") <- predictors
  } else {
    attr(final,"is.predictor.nri") <- TRUE
    attr(final,"response") <- response
  }
  x@multivariate <- final
  return(x)
}

.glm_apply_lh <- function(response, formula, predictors, ...)
{
  data <- cbind(data.frame(response=response), predictors)
  model <- glm(formula = formula, data = data, ...)
  return(summary(model)$coefficients[,c(1:4)])
}
.glm_apply_rh <- function(predictor, formula, response, ...)
{
  data <- data.frame(response=response[,1], predictor=predictor)
  model <- glm(response ~ predictor, data = data, ...)
  su <- summary(model)$coefficients
  if (nrow(su)<ncol(data))
    su <- rbind(su, rep.int(NA,4))
  return(su[,c(1:4)])
}

