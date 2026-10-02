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
library(modelsummary)
library(labelled)

#### Data ####
dt1 <- readRDS("Data/dt1c.rds") 

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
  "gender", "age", "mig", "gv"),
  data = dt99a
)

print(t1, format = "p", digits =2)

# With missing values
dt99b <- dt1 |> 
  mutate(across(where(is.factor), ~ fct_na_value_to_level(.x, level = c("Missing"))))

t2 <- datasummary_skim(
  dt99b |> select(
    # Time-constant
    age, 
    gender, 
    te_w1,
    mig,
    gv,
    # Wave 1
    per21i2_w1, per21i4_w1, per21i5_w1,
    job61i1_w1, job61i2_w1, job61i3_w1, job61i4_w1, job61i5_w1,
    comtime_w1,
    # Wave 2
    per21i2_w2, per21i4_w2, per21i5_w2,
    job61i1_w2, job61i2_w2, job61i3_w2, job61i4_w2, job61i5_w2,
    comtime_w2,
    emp_pr_w2, semp_w2, hs_wc_w2, ftj_w2, wrhr_w2, married_w2, nchild_w2, east_w2, urban_w2,
    # Wave 3
    per21i2_w3, per21i4_w3, per21i5_w3,
    job61i1_w3, job61i2_w3, job61i3_w3, job61i4_w3, job61i5_w3,
    comtime_w3,
    emp_pr_w3, semp_w3, hs_wc_w3, ftj_w3, wrhr_w3, married_w3, nchild_w3, east_w3, urban_w3
  ) |> 
    set_variable_labels(
      # Time-constant 
      age = "Age",
      gender = "Women", 
      mig = "Born outside Germany", 
      te_w1 = "College degree",
      gv = "Gener value",
      # Wave 1
      per21i2_w1 = "Depression: Felt depressed (W1)", 
      per21i4_w1 = "Depression: Felt fearful (W1)", 
      per21i5_w1 = "Depression: Felt sad (W1)",
      job61i1_w1 = "Work-family conflict: Too tired to do chores (W1)", 
      job61i2_w1 = "Work-family conflict: Difficult to fulfil family responsibilities (W1)", 
      job61i3_w1 = "Work-family conflict: Too tired to function at work (W1)", 
      job61i4_w1 = "Work-family conflict: Difficult to concentrate because of family responsibilities (W1)", 
      job61i5_w1 = "Work-family conflict: rivate conflicts impair job performance (W1)",
      comtime_w1 = "Commuting time in hours (W1)",
      # Wave 2
      per21i2_w2 = "Depression: Felt depressed (W2)", 
      per21i4_w2 = "Depression: Felt fearful (W2)", 
      per21i5_w2 = "Depression: Felt sad (W2)",
      job61i1_w2 = "Work-family conflict: Too tired to do chores (W2)", 
      job61i2_w2 = "Work-family conflict: Difficult to fulfil family responsibilities (W2)", 
      job61i3_w2 = "Work-family conflict: Too tired to function at work (W2)", 
      job61i4_w2 = "Work-family conflict: Difficult to concentrate because of family responsibilities (W2)", 
      job61i5_w2 = "Work-family conflict: rivate conflicts impair job performance (W2)",
      comtime_w2 = "Commuting time in hours (W2)",
      emp_pr_w2 = "Employed in the private sector (W2)",
      semp_w2 = "Self-employed (w2)", 
      hs_wc_w2 = "High-skilled, white-collar jobs (W2)", 
      ftj_w2 = "Working full time (W2)", 
      wrhr_w2 = "Weekly hours worked per week (W2)", 
      married_w2 = "Married (W2)", 
      nchild_w2 = "Number of co-resident children aged under 18 (W2)", 
      east_w2 = "East Germany (W2)", 
      urban_w2 = "Living in the city (W2)",
      # Wave 3
      per21i2_w3 = "Depression: Felt depressed (W3)", 
      per21i4_w3 = "Depression: Felt fearful (W3)", 
      per21i5_w3 = "Depression: Felt sad (W3)",
      job61i1_w3 = "Work-family conflict: Too tired to do chores (W3)", 
      job61i2_w3 = "Work-family conflict: Difficult to fulfil family responsibilities (W3)", 
      job61i3_w3 = "Work-family conflict: Too tired to function at work (W3)", 
      job61i4_w3 = "Work-family conflict: Difficult to concentrate because of family responsibilities (W3)", 
      job61i5_w3 = "Work-family conflict: rivate conflicts impair job performance (W3)",
      comtime_w3 = "Commuting time in hours (W3)",
      emp_pr_w3 = "Employed in the private sector (W3)",
      semp_w3 = "Self-employed (W3)", 
      hs_wc_w3 = "High-skilled, white-collar jobs (W3)", 
      ftj_w3 = "Working full time (W3)", 
      wrhr_w3 = "Weekly hours worked per week (W3)", 
      married_w3 = "Married (W3)", 
      nchild_w3 = "Number of co-resident children aged under 18 (W3)", 
      east_w3 = "East Germany (W3)", 
      urban_w3 = "Living in the city (W3)"
    ),
  fmt = 3,
  output = "Output/descriptive20260928.xlsx"
)



#### End ####
