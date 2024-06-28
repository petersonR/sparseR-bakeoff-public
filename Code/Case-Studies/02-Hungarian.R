# Load -------------------------------------------------------------------------

library(here)
library(tidyverse)
library(pmlbr)
library(kableExtra)
library(sparseR)

# Get hungarian data set
hungarianData <- fetch_data("hungarian")

# Get results for hungarian
results <-
  here("DataProcessed/04-Cat-Results.RDS") |> 
  readRDS() 

# Table ------------------------------------------------------------------------

hungarianTable <- results$results |> 
  filter(dataset == "hungarian") |> 
  select(model, roc_auc, RT) |> 
  mutate(model = toupper(model),
         roc_auc = round(roc_auc, 3),
         RT = round(RT, 1)) |> 
  kable(col.names = c("Model", "ROC AUC", "Runtime (s)")) |> 
  kable_minimal(full_width = FALSE)

# Prepare ----------------------------------------------------------------------

hungarianData <- hungarianData |> 
  tibble() |> 
  mutate(across(c(sex, cp, fbs, restecg, exang, slope, ca, thal, target), 
         ~ as.factor(.x)))

# Fit models -------------------------------------------------------------------

## SRL -----

# Fit model
srlFitTime <- system.time({
  set.seed(125)
  srlModel <- hungarianData |> 
    rename(`Odds of occurence` = target) |> 
    sparseR(`Odds of occurence` ~ ., data = _, k = 1, poly = 2)
})

# Set window
par(mfrow = c(1, 2))

# # Plot linear effect
# sparseR::effect_plot(srlModel, coef_name = "sex",
#                      resids = FALSE,
#                      main = "Linear relationship")

# Plot interaction effect
sparseR::effect_plot(srlModel, coef_name = "oldpeak", 
                     by = "cp", 
                     resids = FALSE,
                     main = "Interaction effect")

# Plot non-linear effect
sparseR::effect_plot(srlModel, coef_name = "thalach", 
                     resids = FALSE,
                     main = "Non-linear relationship")

# Reset window
par(mfrow = c(1, 1))
