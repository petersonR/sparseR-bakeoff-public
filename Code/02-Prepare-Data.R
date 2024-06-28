library(dplyr)
library(forcats)

# Read data from previous steps
dfSampleList <- 
  here::here("DataProcessed/00-Raw-Data-List.RDS") |> 
  readRDS()
metadataList <- 
  here::here("DataProcessed/01-Metadata-List.RDS") |> 
  readRDS()

# Make sure two data sets are in the same order (not actually necessary, but
#   useful for spot checking)
dfSampleList <- dfSampleList[order(names(dfSampleList))]
metadataList <- metadataList[order(names(metadataList))]
all(names(dfSampleList) == names(metadataList))

# Clean data sets
cleanDFList <- lapply(names(dfSampleList), function (name) {
  
  # Get data set and associated metadata
  dataset <- dfSampleList[[name]]
  metadata <- metadataList[[name]]
  
  # Get vectors of variable type, store indices of each type
  varTypes <- unlist(lapply(metadata$features, function(x) x$type))
  catVarIndex <- which(varTypes == "categorical")
  binVarNames <- which(varTypes == "binary")
  
  # Mutate variables according to type - combine small sample size categories
  #   and convert binary and categorical to factor
  dataset <- dataset |>
    dplyr::mutate(across(all_of(catVarIndex), function (col, prop_lump = 0.1) 
    { col <- forcats::fct_lump_prop(factor(col), prop_lump) })) |>
    dplyr::mutate(across(all_of(binVarNames), factor))
  
  # Make all binary responses factors with levels 0,1
  if (metadata$target$type != "continuous") {
    dataset <- dataset |> 
      mutate(target = factor(target))
    
    # Make sure all binary variables have 0,1 coding
    if (all(levels(dataset$target) == c("1", "2"))) {
      levels(dataset$target) <- c("0", "1")
    }
    
  }
  
  # Remove any predictor columns w/o variability
  dataset <- dataset |> 
    select_if(function(x) { length(unique(x)) > 1 })
  
  dataset
  
})

# Add names to cleaned list
names(cleanDFList) <- names(dfSampleList)

# Save clean data
here::here("DataProcessed/02-Clean-Data-List.RDS") |> 
  saveRDS(cleanDFList, file = _)
