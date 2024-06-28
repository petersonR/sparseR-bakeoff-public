fitModel <- function(modelFun, modelName, dfName, dfTest, ...) {
  
  # Prepare output
  rtn <- data.frame("dataset" = dfName, "model" = modelName, 
                    "roc_auc" = NA_real_,
                    "fitError" = FALSE, "statError" = FALSE)
  
  # Run and time model, catch warnings
  fitStatus <- tryCatchLog::tryCatchLog(
    expr = {
      startTime <- Sys.time()
      fit <- modelFun(...)
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
        if (rlang::env_name(environment(modelFun)) == "namespace:sparseR") {
          rtn$roc_auc <- 
            yardstick::roc_auc_vec(factor(dfTest$target), 
                                   predict(fit, newdata = dfTest, 
                                           type = "response"), 
                                   event_level = "second")
        } else if (modelName == "sv") { 
          rtn$roc_auc <- 
            yardstick::roc_auc_vec(factor(dfTest$target), 
                                   as.numeric(predict(fit, newdata = dfTest, 
                                           type = "raw")) - 1, 
                                   event_level = "second") 
        } else {
          rtn$roc_auc <- 
            yardstick::roc_auc_vec(factor(dfTest$target), 
                                   predict(fit, newdata = dfTest, 
                                           type = "prob")[, 2], 
                                   event_level = "second") 
        }
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