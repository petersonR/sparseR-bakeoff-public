# Prep Environment -------------------------------------------------------------

# Libraries
library(here)
library(sparseR)
library(yardstick)
library(pmlbr)
library(dplyr)
library(doRNG)

# Model parameters
seed <- 2023
testTrainRatio <- .2
numCores <- 45 # Number of cores to use when fitting models

# Get data ---------------------------------------------------------------------

# Get data sets from PMLB, reduce
dataSummary <- pmlbr::summary_stats |>
  filter(!(endpoint_type == "categorical" & n_classes > 2)) |>
  filter(n_instances < 10000) |>
  filter(n_features <= 40) |>
  filter(n_features * n_instances < 100000)

# Load dataframes from GitHub - specifically from EpistasisLab/pmlb
dfSampleList <- lapply(dataSummary$dataset, pmlbr::fetch_data)
names(dfSampleList) <- dataSummary$dataset

# Save data
here::here("DataProcessed/00-Raw-Data-List.RDS") |> 
  saveRDS(dfSampleList, file = _)
here::here("DataProcessed/00-Data-Summary.RDS") |> 
  saveRDS(dataSummary, file = _)

# Prepare ----------------------------------------------------------------------
source(here("Code/01-Get-Metadata.R"))
source(here("Code/02-Prepare-Data.R"))

# Fit models -------------------------------------------------------------------
source(here("Code/03-Fit-Reg-Mods.R"))
source(here("Code/04-Fit-Cat-Mods.R"))
