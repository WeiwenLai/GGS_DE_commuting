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

dt_imp <- mice(dt1, m = 20)

#### save data ####
saveRDS(dt_imp, file = "Data/dt1d.rds")

#### End ####