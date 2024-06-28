# Load -------------------------------------------------------------------------

library(here)
library(tidyverse)
library(pmlbr)
library(kableExtra)
library(sparseR)

# Get 503_wind data set
windData <- fetch_data("503_wind")

# Get results for 503_wind
results <-
  here("DataProcessed/03-Reg-Results.RDS") |> 
  readRDS()
results <- results$results |> 
  filter(dataset == "503_wind")

# Prepare ----------------------------------------------------------------------

windData <- windData |> 
  tibble() |> 
  mutate(across(everything(), ~ as.numeric(.x)))

# Table ------------------------------------------------------------------------

five03WindTable <- results |> 
  select(model, Rsq_Tst, RMSE_Tst, RT) |> 
  mutate(model = toupper(model),
         Rsq_Tst = round(Rsq_Tst, 3),
         RMSE_Tst = round(RMSE_Tst, 2),
         RT = round(RT, 1)) |> 
  kable(col.names = c("Model", "Test R-squared", "Test RMSE", "Runtime (s)")) |> 
  kable_minimal(full_width = FALSE)

# Fit models -------------------------------------------------------------------

## SRL -----

# Fit model
srlFitTime <- system.time({
  set.seed(125)
  srlModel <- windData |> 
    rename(Outcome = target) |> 
    sparseR(Outcome ~ ., data = _, k = 1, poly = 2)
})

# Set window
par(mfrow = c(1, 3))

# Plot linear effect
sparseR::effect_plot(srlModel, coef_name = "BEL", 
                     resids = FALSE,
                     main = "Linear effect")

# Plot interaction effects
sparseR::effect_plot(srlModel, coef_name = "CLO", 
                     by = "VAL", 
                     resids = FALSE,
                     main = "Interaction effect")

# Plot non-linear effect
sparseR::effect_plot(srlModel, coef_name = "year", 
                     resids = FALSE,
                     main = "Non-linear effect")

# Reset window
par(mfrow = c(1, 1))