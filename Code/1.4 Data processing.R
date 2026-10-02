#### Start ####

# Imputation

rm(list = ls())
gc()

#### WD ####
setwd("G:/My Drive/R Projects/GGS_DE_commuting")

#### Library ####
library(haven)
library(dplyr)
library(mice)

#### Data ####
dt1 <- readRDS("Data/dt1c.rds") %>% 
  zap_labels()

# Exclude id from the imputation model
pred <- make.predictorMatrix(dt1)
pred[, "id"] <- 0   # id is not used to predict other variables
pred["id", ] <- 0   # nothing is used to predict id

meth <- make.method(dt1)
meth["id"] <- ""    # id is not imputed

dt_imp <- mice(dt1, m = 20, maxit = 5, seed = 1234,
               predictorMatrix = pred, method = meth)

#### save data ####
saveRDS(dt_imp, file = "Data/dt1d.rds")

#### End ####