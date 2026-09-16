#### Start ####

#### WD ####
setwd("G:/My Drive/R Projects/GGS_DE_commuting")

#### Library ####
library(tableone)
library(dplyr)

#### Data ####
dt99 <- readRDS("Data/dt99a.rds")

#### Create a descriptive table ####
t1 <- CreateTableOne(data = dt99, strata = "Female")

#### Output ####
t1_out <- print(t1, exact = "stage", 
                quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
t1_out %>% write.csv("Output/t1.csv")
  
#### End ####