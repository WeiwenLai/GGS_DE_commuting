#### Start ####

# presenting descriptive statistics

rm(list = ls())
gc()

#### WD ####
setwd("G:/My Drive/R Projects/GGS_DE_commuting")

#### Library ####
library(haven)
library(dplyr)
library(tableone)
library(ggplot2)
library(forcats)

#### Data ####
dt1 <- readRDS("Data/dt1b.rds") 

# Complete case
dt99a <- dt1 |> 
  na.omit()
  
#### desc table ####
t1 <- CreateTableOne(vars = c(
  "per21i2_w1", "per21i4_w1", "per21i5_w1",
  "comtime_w1",
  "job61i1_w1", "job61i2_w1", "job61i3_w1", "job61i4_w1", "job61i5_w1",
  "edu_w1", "emp_w1", "occ_w1", "ftj_w1", "wrhr_w1", "relstat_w1", "nchild_w1", "east_w1", "urban_w1",
  "comtime_w2",
  "job61i1_w2", "job61i2_w2", "job61i3_w2", "job61i4_w2", "job61i5_w2",
  "edu_w2", "emp_w2", "occ_w2", "ftj_w2", "wrhr_w2", "relstat_w2", "nchild_w2", "east_w2", "urban_w2",
  "per21i2_w2", "per21i4_w2", "per21i5_w2",
  "comtime_w3",
  "job61i1_w3", "job61i2_w3", "job61i3_w3", "job61i4_w3", "job61i5_w3",
  "edu_w3", "emp_w3", "occ_w3", "ftj_w3", "wrhr_w3", "relstat_w3", "nchild_w3", "east_w3", "urban_w3",
  "gender", "age", "mig"),
  data = dt99
)

print(t1, format = "p", digits =2)

# With missing values
dt99b <- dt1 |> 
  mutate(across(where(is.factor), ~ fct_na_value_to_level(.x, level = c("Missing"))))

t2 <- datasummary_skim(
  dt99b |> select("per21i2_w1", "per21i4_w1", "per21i5_w1",
                  "comtime_w1",
                  "job61i1_w1", "job61i2_w1", "job61i3_w1", "job61i4_w1", "job61i5_w1",
                  "edu_w1", "emp_w1", "occ_w1", "ftj_w1", "wrhr_w1", "relstat_w1", "nchild_w1", "east_w1", "urban_w1",
                  "comtime_w2",
                  "job61i1_w2", "job61i2_w2", "job61i3_w2", "job61i4_w2", "job61i5_w2",
                  "edu_w2", "emp_w2", "occ_w2", "ftj_w2", "wrhr_w2", "relstat_w2", "nchild_w2", "east_w2", "urban_w2",
                  "per21i2_w2", "per21i4_w2", "per21i5_w2",
                  "comtime_w3",
                  "job61i1_w3", "job61i2_w3", "job61i3_w3", "job61i4_w3", "job61i5_w3",
                  "edu_w3", "emp_w3", "occ_w3", "ftj_w3", "wrhr_w3", "relstat_w3", "nchild_w3", "east_w3", "urban_w3",
                  "gender", "age", "mig")
)

#### End ####
