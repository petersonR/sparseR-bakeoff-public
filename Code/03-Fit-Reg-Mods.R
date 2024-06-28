library(foreach)
library(here)
library(ggplot2)
library(lattice)
source(here("Code/03-Fit-Reg-Mods-Funs.R"))

# Save info
savePath <- here("DataProcessed/03-Reg-Results.RDS")
logPath <- Sys.time() |> 
  as.character() |> 
  stringr::str_replace_all(c("-" = "", " " = "", ":" = "")) |> 
  paste0("DataProcessed/03-Reg-Results-Log-", ... = _, ".out") |> 
  here()

# Training method (caret only)

# Define a custom summary function for R-squared (traditional computation of r-squared; caret 
# does the pearson correlation squared by default which is not computable in certain settings)
customSummary <- function(data, lev = NULL, model = NULL) {
  # Calculate traditional R-squared
  rsq <- 1 - sum((data$obs - data$pred)^2) / sum((data$obs - mean(data$obs))^2)
  
  # Return a named vector as required by caret
  c(RMSE = sqrt(mean((data$pred - data$obs)^2)), Rsquared = rsq)
}

trainParams <- 
  caret::trainControl(method = "repeatedcv", number = 10, repeats = 1
                      # , summaryFunction = customSummary
                      )

# Load prepared data, reduce to only regression data sets
dfList <- readRDS(here("DataProcessed/02-Clean-Data-List.RDS"))
dfList <- dfList[order(tolower(names(dfList)))]
dataSummaryDF <- readRDS(here("DataProcessed/01-Data-Summary.RDS")) |> 
  dplyr::arrange(tolower(dataset))
dfListReg <- dfList[dataSummaryDF$endpoint_type == "continuous"]
nDataSets <- length(dfListReg)

# Parallelization params
cluster <- parallel::makeCluster(numCores, outfile = logPath)
doSNOW::registerDoSNOW(cluster)
parallel::clusterExport(cluster, 
                        c("dfListReg", "testTrainRatio", "trainParams"))

# Prepare progress bar
pb <- progress::progress_bar$new(total = nDataSets, 
                                 format = " [:bar] :percent eta: :eta", 
                                 width= 60)
progressLetter <- rep(LETTERS[1:10], 10)
progress <- function(n) {
  pb$tick(tokens = list(letter = progressLetter[n]))
} 
opts <- list(progress = progress)

# Prepare warning logger
writeLines("", logPath)

# Set seed
set.seed(seed)

# Limit number of data sets if needed
if (nDataSets <= length(dfListReg)) {
  i <- 1:nDataSets
} else {
  i <- 1:length(dfListReg)
}

# Train models, evaluate using test set
results <- foreach::foreach (
  i = i,
  .combine = dplyr::bind_rows, 
  .options.snow = opts,
  .packages = c("sparseR", "caret")) %dorng% {
    
    # Get data set and data set name
    df <- dfListReg[[i]]
    dfName <- names(dfListReg)[i]
    
    # Split into training/test datasets
    testIndex <- sample(1:nrow(df), floor(nrow(df)*testTrainRatio))
    dfTrain <- df[-testIndex,]
    dfTest <- df[testIndex,]
    
    # Fit models
    srl <- fitModel(sparseR::sparseR, "srl", dfName, dfTest, data = dfTrain, 
                    k = 1, poly = 2)
    lso <- fitModel(sparseR::sparseR, "lso", dfName, dfTest, data = dfTrain,
                    k = 0, poly = 1)
    rf <- fitModel(caret::train, "rf", dfName, dfTest, data = dfTrain, 
                   method = "rf", trControl = trainParams)
    sv <- fitModel(caret::train, "sv", dfName, dfTest, data = dfTrain, 
                   method = "svmLinear", trControl = trainParams)
    nn <- fitModel(caret::train, "nn", dfName, dfTest, data = dfTrain, 
                   method = "nnet", trControl = trainParams, linout = TRUE, 
                   trace = FALSE)
    xb <- fitModel(caret::train, "xb", dfName, dfTest, data = dfTrain, 
                   method = "xgbTree", trControl = trainParams, verbosity = 0)
    
    # Combine all info
    rtn <- dplyr::bind_rows(srl, lso, rf, sv, nn, xb) |> 
      dplyr::mutate(i = i, .before = dataset)
  
  }

# Make sure all clusters are stopped
parallel::stopCluster(cluster)
env <- foreach:::.foreachGlobals
rm(list=ls(name=env), pos=env)

# Save results and all info used to generate them
saveRDS(list("results" = results, "seed" = seed, 
             "testTrainRatio" = testTrainRatio, "trainParams" = trainParams, 
             "dfListReg" = dfListReg),
        file =  savePath)
