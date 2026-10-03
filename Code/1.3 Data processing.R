#### Start ####

# Objective:
# 1) Turning categorical vars into binary coding for fitting SEM
# 2) Also to calculate missing data, appended to the attrition table

rm(list = ls())
gc()

#### WD ####
setwd("G:/My Drive/R Projects/GGS_DE_commuting")

#### Library ####
library(haven)
library(dplyr)

#### Data ####
dt1 <- readRDS("Data/dt1b.rds")

#### edu (ref: upper secondary) ####
# pse -> Post-secondary
# te -> Tertiary
dt2 <- dt1 |> 
  mutate(across(starts_with("edu"), ~ case_when(.x == "Post-secondary" ~ 1,
                                                !is.na(.x) ~ 0),
                .names = "{sub('edu_', 'pse_', .col)}")) |> 
  mutate(across(starts_with("edu"), ~ case_when(.x == "Tertiary" ~ 1,
                                                !is.na(.x) ~ 0),
                .names = "{sub('edu_', 'te_', .col)}"))
  
#### Occupation (ref: Low-skilled blue-collar) ####
# hs_bc -> High-skilled blue-collar
# ls_wc -> Low-skilled white-collar
# hs_wc -> High-skilled white-collar
dt3 <- dt2 |> 
  mutate(across(starts_with("occ_"), ~ case_when(.x == "High-skilled blue-collar" ~ 1,
                                                !is.na(.x) ~ 0),
                .names = "{sub('occ_', 'hs_bc_', .col)}")) |> 
  mutate(across(starts_with("occ_"), ~ case_when(.x == "Low-skilled white-collar" ~ 1,
                                                 !is.na(.x) ~ 0),
                .names = "{sub('occ_', 'ls_wc_', .col)}")) |> 
  mutate(across(starts_with("occ_"), ~ case_when(.x == "High-skilled white-collar" ~ 1,
                                                 !is.na(.x) ~ 0),
                .names = "{sub('occ_', 'hs_wc_', .col)}"))
   
#### employment (ref: Employed, public sector) #### 
# emp_pr -> private employment
# semp -> self-employed
dt4 <- dt3 |> 
  mutate(across(starts_with("emp_"), ~ case_when(.x == "Employed, private sector" ~ 1,
                                                 !is.na(.x) ~ 0),
                .names = "{sub('emp_', 'emp_pr_', .col)}")) |> 
  mutate(across(starts_with("emp_"), ~ case_when(.x == "Self-employed" ~ 1,
                                                 !is.na(.x) ~ 0),
                .names = "{sub('emp_', 'semp_', .col)}"))

#### Relationship status (ref: single & cohabiting) ####
# married -> married
dt5 <- dt4 |> 
  mutate(across(starts_with("relstat_"), ~ case_when(.x == "Married" ~ 1,
                                                     !is.na(.x) ~ 0),
                .names = "{sub('relstat_', 'married_', .col)}"))

#### Other binary variables ####
dt6 <- dt5 |> 
  mutate(across(c("gender", "mig"), ~ as.numeric(.x) - 1)) |> 
  mutate(across(starts_with("ftj_"), ~ as.numeric(.x) - 1)) |> 
  mutate(across(starts_with("east_"), ~ as.numeric(.x) - 1)) |> 
  mutate(across(starts_with("urban"), ~ as.numeric(.x) - 1))

#### Keep the selected vars ####

#### Missing data pattern ####
# Missing on time-constant vars
m_tcv <- rowSums(is.na(dt6[, c("age", "gender", "mig", "gv", "te_w1")])) > 0
# Wave 1
# Commuting time
m_w1_commtime <- is.na(dt6$comtime_w1)
# Depression scale
m_w1_dep <- rowSums(is.na(dt6[, c(paste0("per21i", c(2, 4, 5), "_w1"))])) > 0
# Work-family conflicts
m_w1_wfc <- rowSums(is.na(dt6[, c(paste0("job61i", 1:5, "_w1"))])) > 0
# Other covariates
m_w1_other <- rowSums(is.na(dt6[, c("emp_pr_w1", "semp_w1", "hs_wc_w1", 
                                    "ftj_w1", "wrhr_w1", "married_w1", 
                                    "nchild_w1", "east_w1", "urban_w1")])) > 0
# Missing on any time-varying covariates
m_w1_tvc <- rowSums(is.na(dt6[, c("comtime_w1",
                                  paste0("per21i", c(2, 4, 5), "_w1"),
                                  paste0("job61i", 1:5, "_w1"),
                                  "emp_pr_w1", "semp_w1", "hs_wc_w1", 
                                  "ftj_w1", "wrhr_w1", "married_w1", 
                                  "nchild_w1", "east_w1", "urban_w1")])) > 0
                    
# Wave 2
# Commuting time
m_w2_commtime <- is.na(dt6$comtime_w2)
# Depression scale
m_w2_dep <- rowSums(is.na(dt6[, c(paste0("per21i", c(2, 4, 5), "_w2"))])) > 0
# Work-family conflicts
m_w2_wfc <- rowSums(is.na(dt6[, c(paste0("job61i", 1:5, "_w2"))])) > 0
# Other covariates
m_w2_other <- rowSums(is.na(dt6[, c("emp_pr_w2", "semp_w2", "hs_wc_w2", 
                                    "ftj_w2", "wrhr_w2", "married_w2", 
                                    "nchild_w2", "east_w2", "urban_w2")])) > 0
# Missing on any time-varying covariates
m_w2_tvc <- rowSums(is.na(dt6[, c("comtime_w2",
                                  paste0("per21i", c(2, 4, 5), "_w2"),
                                  paste0("job61i", 1:5, "_w2"),
                                  "emp_pr_w2", "semp_w2", "hs_wc_w2", 
                                  "ftj_w2", "wrhr_w2", "married_w2", 
                                  "nchild_w2", "east_w2", "urban_w2")])) > 0

# Wave 3
# Commuting time
m_w3_commtime <- is.na(dt6$comtime_w3)
# Depression scale
m_w3_dep <- rowSums(is.na(dt6[, c(paste0("per21i", c(2, 4, 5), "_w3"))])) > 0
# Work-family conflicts
m_w3_wfc <- rowSums(is.na(dt6[, c(paste0("job61i", 1:5, "_w3"))])) > 0
# Other covariates
m_w3_other <- rowSums(is.na(dt6[, c("emp_pr_w3", "semp_w3", "hs_wc_w3", 
                                    "ftj_w3", "wrhr_w3", "married_w3", 
                                    "nchild_w3", "east_w3", "urban_w3")])) > 0
# Missing on any time-varying covariates
m_w3_tvc <- rowSums(is.na(dt6[, c("comtime_w3",
                                  paste0("per21i", c(2, 4, 5), "_w3"),
                                  paste0("job61i", 1:5, "_w3"),
                                  "emp_pr_w3", "semp_w3", "hs_wc_w3", 
                                  "ftj_w3", "wrhr_w3", "married_w3", 
                                  "nchild_w3", "east_w3", "urban_w3")])) > 0

# modify the original attrition table
att_misdat <- readRDS("Data/att_tab.rds") |>
  rbind(
    data.frame(
      wave1 = c(sum(m_tcv), sum(m_w1_commtime), sum(m_w1_dep), sum(m_w1_wfc), sum(m_w1_other), sum(m_w1_tvc)),
      wave2 = c("", sum(m_w2_commtime), sum(m_w2_dep), sum(m_w2_wfc), sum(m_w2_other), sum(m_w2_tvc)),
      wave3 = c("", sum(m_w3_commtime), sum(m_w3_dep), sum(m_w3_wfc), sum(m_w3_other), sum(m_w3_tvc)),
      row.names = c("Missing on time-constant covariates",
                    "Missing on commuting time",
                    "Missing on the depression scale",
                    "Missing on the work-family conflict scale",
                    "Missing on other time-varying covariates",
                    "Missing on any time-varying covariates")
    )
  ) 
  


#### save data ####
varlist <- c("id", "age", "gender", "mig", "gv",
             paste0("comtime_w", 1:3),
             paste0("per21i2_w", 1:3),
             paste0("per21i4_w", 1:3),
             paste0("per21i5_w", 1:3),
             paste0("job61i1_w", 1:3),
             paste0("job61i2_w", 1:3),
             paste0("job61i3_w", 1:3),
             paste0("job61i4_w", 1:3),
             paste0("job61i5_w", 1:3),
             paste0("te_w", 1:3),
             paste0("emp_pr_w", 1:3),
             paste0("semp_w", 1:3),
             paste0("hs_wc_w", 1:3),
             paste0("ftj_w", 1:3),
             paste0("wrhr_w", 1:3),
             paste0("married_w", 1:3),
             paste0("nchild_w", 1:3),
             paste0("east_w", 1:3),
             paste0("urban_w", 1:3),
             paste0("stattrxrdesign_w", 1:3)
             )
dt7 <- dt6[varlist]

saveRDS(dt7, file = "Data/dt1c.rds")

#### End ####
