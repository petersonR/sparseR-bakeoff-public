library(gh)
library(RCurl)
library(yaml)
library(here)

# Load data from previous step
dataList <- 
  here::here("DataProcessed/00-Raw-Data-List.RDS") |> 
  readRDS()
dataSummary <- 
  here::here("DataProcessed/00-Data-Summary.RDS") |> 
  readRDS()

# Get tree for latest commit on master branch of EpistasisLab/pmlb/datasets 
#   using GitHub API
latestCommit <- gh::gh("GET /repos/EpistasisLab/pmlb/commits/master")
treeSHA <- latestCommit$commit$tree$sha
tree <- paste0("GET /repos/EpistasisLab/pmlb/git/trees/", treeSHA) |> 
  gh::gh()
datasetTreeSHA <- tree$tree[[8]]$sha
datasetTree <- paste0("GET /repos/EpistasisLab/pmlb/git/trees/", 
                      datasetTreeSHA) |> 
  gh::gh()

# Convert tree into list of file names
datasetTreeList <- datasetTree$tree
datasetFilenames <- lapply(datasetTreeList, function(x) x$path) |>
  unlist()

# Reduce to only those in data frame list
datasetFilenames <- datasetFilenames[datasetFilenames %in% names(dataList)]

# Convert list of file names into URLs of metadata.yaml raw files on GitHub
datasetURLs <- 
  paste0("https://raw.githubusercontent.com/EpistasisLab/pmlb/master/datasets/",
         datasetFilenames,
         "/metadata.yaml")

# Get metadata.yaml files as strings - NOTE: this process sometimes fails, but
#   I'm not totally sure why. May be GH limiting requests. I have it set up
#   to process the URLs in batches, which seems to help but it still fails
#   sometimes
batches <- split(datasetURLs, as.integer((seq_along(datasetURLs) - 1) / 25))
metadataFiles <- unlist(lapply(batches, RCurl::getURL, timeout = 4))

# Convert to list of R objects
metadataList <- lapply(metadataFiles, yaml::yaml.load)
names(metadataList) <- datasetFilenames

# Create DF of info from metadata
metadataDF <- purrr::map(metadataList, .f = function(metadata) {
  
    # Get dataset info
    dataset <- metadata$dataset
    task <- metadata$task
    endpoint_type <- metadata$target$type
    
    # Feature info
    featureInfo <- metadata$features |> 
      bind_rows()
    n_features <- nrow(featureInfo)
    n_binary_features <- sum(featureInfo$type == "binary")
    n_categorical_features <- sum(featureInfo$type == "categorical")
    n_continuous_features <- sum(featureInfo$type == "continuous")
    
    # Return
    data.frame(dataset = dataset,
               n_features = n_features,
               n_binary_features = n_binary_features,
               n_categorical_features = n_categorical_features,
               n_continuous_features = n_continuous_features,
               endpoint_type = endpoint_type,
               task = task)
  }) |> 
  dplyr::bind_rows()

# Join info not found in metadata files (from pmlbr package)
dataSummary <- dataSummary |> 
  dplyr::select(dataset, n_instances, n_classes, imbalance) |> 
  dplyr::right_join(metadataDF, by = "dataset") |> 
  dplyr::select(dataset, n_instances, n_features, n_binary_features,
                n_categorical_features, n_continuous_features, endpoint_type,
                n_classes, imbalance, task)

# Save data
here::here("DataProcessed/01-Metadata-List.RDS") |> 
  saveRDS(metadataList, file = _)
here::here("DataProcessed/01-Data-Summary.RDS") |> 
  saveRDS(dataSummary, file = _)