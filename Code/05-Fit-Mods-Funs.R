# Function to fit model, time param estimation, catch errors, store results
#
# NOTE: I would really like if this function returned a "warning" column with
#   a T/F indicator of whether a warning was called, but I haven't had any luck
#   getting that to work. Setting silent.warnings=F results in time not getting
#   logged, and including a warning= argument doesn't enable us to alter rtn
fitModel <- function(modelFun, modelName, dfName, dfType, ...) {
  
  # Prepare output
  rtn <- data.frame("dataset" = dfName, "model" = modelName, 
                    "type" = dfType,
                    "var_sum" = NA_real_, "var_max" = NA_real_, 
                    rsq = NA_real_, ndiscoveries = NA_real_,
                    "fitError" = FALSE, "statError" = FALSE)
  
  # Run and time model, catch warnings
  fitStatus <- tryCatchLog::tryCatchLog(
    expr = {
      startTime <- Sys.time()
      fit <- modelFun(target ~ ., dfType = dfType, ...)
      endTime <- Sys.time()
      rtn$RT <- as.numeric(endTime - startTime)
    },
    error = function(e) { 
      "error"
    },
    include.full.call.stack = FALSE,
    include.compact.call.stack = FALSE,
    silent.warnings = TRUE,
    execution.context.msg = paste0(dfName, " w/ ", modelName, " fit"))
  
  # Store error info if fit error'd out, otherwise proceed to gathering fit
  #   statistics
  if (fitStatus == "error") {
    rtn$fitError <- TRUE
    statStatus <- "error"
  } else {
    
    # Sometimes model fit stats will throw warnings/errors, so we want to catch
    #   and report those too
    statStatus <- tryCatchLog::tryCatchLog(
      expr = {
        # Get stats
        rtn$var_sum <- sum(diag(vcov(fit)), na.rm = TRUE)
        rtn$var_max <- max(diag(vcov(fit)), na.rm = TRUE)
        s <- summary(fit)
        if(!is.null(s$r.squared)) {
          rtn$rsq <- s$r.squared
        } else {
          rtn$rsq <- 1-((fit$deviance)/fit$null.deviance)
        }
        
        rtn$ndiscoveries <- sum(s$coef[-1,4] < 0.05)
      },
      error = function(e) {
        "error"
      },
      include.full.call.stack = FALSE,
      include.compact.call.stack = FALSE,
      silent.warnings = TRUE,
      execution.context.msg = paste0(dfName, " w/ ", modelName, " statistics"))
  }
  
  # Store error status for statistic calculation
  # if(statStatus == "error") {
  #   rtn$statError <- TRUE
  # }
  
  rtn
}

lmglm <- function(formula, data, dfType = "continuous") { 
  X <- model.matrix(formula, data)[,-1]
  y <- model.response(model.frame(formula, data))
  X <- scale(X)
  
  if(dfType == "continuous")
    return(lm(scale(y)~ X))
  
  glm(y~ X, family = "binomial")
}


bundlR <- function(formula, data, dfType = "continuous") { 
  X <- model.matrix(formula, data)[,-1]
  X <- scale(X)
  y <- model.response(model.frame(formula, data))
  r <- cor(X)
  bundl_idx <- which(idx <- abs(r) == max(abs(r[r<1])), arr.ind = TRUE)[1,]
  bundl <- predict(prcomp(X[,bundl_idx]))[,1]
  X_bundle <- cbind(X[,-bundl_idx], bundl = bundl)
  
  if(dfType == "continuous")
    return(lm(scale(y) ~ X_bundle))
  
  glm(y ~ X_bundle, family = "binomial")
}
