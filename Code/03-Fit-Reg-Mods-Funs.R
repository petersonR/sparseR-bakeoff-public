# Function to fit model, time param estimation, catch errors, store results
#
# NOTE: I would really like if this function returned a "warning" column with
#   a T/F indicator of whether a warning was called, but I haven't had any luck
#   getting that to work. Setting silent.warnings=F results in time not getting
#   logged, and including a warning= argument doesn't enable us to alter rtn
fitModel <- function(modelFun, modelName, dfName, dfTest, ...) {
  
  # Prepare output
  rtn <- data.frame("dataset" = dfName, "model" = modelName, 
                    "Rsq_ES" = NA_real_, "Rsq_Tst" = NA_real_, 
                    "RMSE_Tst" = NA_real_, "RT" = NA_real_,
                    "fitError" = FALSE, "statError" = FALSE)
  
  # Run and time model, catch warnings
  fitStatus <- tryCatchLog::tryCatchLog(
    expr = {
      startTime <- Sys.time()
      fit <- modelFun(target ~ ., ...)
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
        rtn$Rsq_ES <- ifelse(rlang::env_name(environment(modelFun)) ==
                               "namespace:caret",
                             max(fit$results$Rsquared),
                             max(summary(fit$fit)$r.squared))
        rtn$Rsq_Tst <- yardstick::rsq_trad_vec(dfTest$target, 
                                          predict(fit, newdata = dfTest))
        rtn$RMSE_Tst <- yardstick::rmse_vec(dfTest$target, 
                                            predict(fit, newdata = dfTest))
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
  if(statStatus == "error") {
    rtn$statError <- TRUE
  }
  
  rtn
}
