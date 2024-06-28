library(foreach)
library(here)
library(ggplot2)
library(lattice)
source(here("Code/04-Fit-Cat-Mods-Funs.R"))

# Save info
savePath <- here("DataProcessed/04-Cat-Results.RDS")
logPath <- Sys.time() |> 
  as.character() |> 
  stringr::str_replace_all(c("-" = "", " " = "", ":" = "")) |> 
  paste0("DataProcessed/04-Cat-Results-", ... = _, ".out") |> 
  here()

# Training method (caret only)
trainParams <- 
  caret::trainControl(method = "repeatedcv", number = 10, repeats = 1)

# Load prepared data, reduce to only regression data sets
dfList <- readRDS(here("DataProcessed/02-Clean-Data-List.RDS"))
dfList <- dfList[order(tolower(names(dfList)))]
dataSummaryDF <- readRDS(here("DataProcessed/01-Data-Summary.RDS")) |> 
  dplyr::arrange(tolower(dataset))
dfListCat <- dfList[dataSummaryDF$endpoint_type == "categorical"]
nDataSets <- length(dfListCat)

# Parallelization params
cluster <- parallel::makeCluster(numCores, outfile = logPath)
doSNOW::registerDoSNOW(cluster)
parallel::clusterExport(cluster, 
                        c("dfListCat", "testTrainRatio", "trainParams"))

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
if (nDataSets <= length(dfListCat)) {
  i <- 1:nDataSets
} else {
  i <- 1:length(dfListCat)
}

# Train models, evaluate using test set
results <- foreach::foreach (
  i = i,
  .combine = dplyr::bind_rows, 
  .options.snow = opts,
  .packages = c("sparseR", "caret")) %dorng% {
    
    # Get data set and data set name
    df <- dfListCat[[i]]
    dfName <- names(dfListCat)[i]
    
    # Split into training/test datasets
    testIndex <- sample(1:nrow(df), floor(nrow(df)*testTrainRatio))
    dfTrain <- df[-testIndex,]
    dfTest <- df[testIndex,]
    
    # Fit Models
    srl <- fitModel(sparseR::sparseR, "srl", dfName, dfTest, 
                    formula = target ~ ., data = dfTrain, k = 1, poly = 2, 
                    family = "binomial")
    lso <- fitModel(sparseR::sparseR, "lso", dfName, dfTest, 
                    formula = target ~ ., data = dfTrain, k = 0, poly = 1, 
                    family = "binomial")
    rf <- fitModel(caret::train, "rf", dfName, dfTest, 
                   form = factor(target) ~ ., data = dfTrain,
                   method = "rf", trControl = trainParams)
    sv <- fitModel(caret::train, "sv", dfName, dfTest, 
                   form = factor(target) ~ ., data = dfTrain,
                   method = "svmLinear", trControl = trainParams)
    nn <- fitModel(caret::train, "nn", dfName, dfTest,
                   x = dplyr::select(dfTrain, -target), y = factor(dfTrain$target),
                   method = "nnet", trControl = trainParams, linout = FALSE,
                   trace = FALSE)
    xb <- fitModel(caret::train, "xb", dfName, dfTest,
                   form = factor(target) ~ ., data = dfTrain, method = "xgbTree",
                   trControl = trainParams, verbosity = 0)
    # xb <- fitModel(caret::train, "xb", dfName, dfTest,
    #                form = factor(target) ~ ., data = dfTrain, method = "gbm",
    #                trControl = trainParams, verbose = FALSE)
    
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
             "dfListCat" = dfListCat),
        file =  savePath)
