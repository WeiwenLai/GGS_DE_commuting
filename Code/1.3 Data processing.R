#### Start ####

# Turning categorical vars into binary coding for fitting SEM

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

#### Relationship status (ref: single) ####
# coh -> cohabiting
# married -> married
dt5 <- dt4 |> 
  mutate(across(starts_with("relstat_"), ~ case_when(.x == "Cohabiting" ~ 1,
                                                 !is.na(.x) ~ 0),
                .names = "{sub('relstat_', 'coh_', .col)}")) |> 
  mutate(across(starts_with("relstat_"), ~ case_when(.x == "Married" ~ 1,
                                                     !is.na(.x) ~ 0),
                .names = "{sub('relstat_', 'married_', .col)}"))

#### Other binary variables ####
dt6 <- dt5 |> 
  mutate(across(c("gender", "mig"), ~ as.numeric(.x) - 1)) |> 
  mutate(across(starts_with("ftj_"), ~ as.numeric(.x) - 1)) |> 
  mutate(across(starts_with("east_"), ~ as.numeric(.x) - 1)) |> 
  mutate(across(starts_with("urban"), ~ as.numeric(.x) - 1))

#### save data ####
varlist <- c("age", "gender", "mig",
             paste0("comtime_w", 1:3),
             paste0("per21i2_w", 1:3),
             paste0("per21i4_w", 1:3),
             paste0("per21i5_w", 1:3),
             paste0("job61i1_w", 1:3),
             paste0("job61i2_w", 1:3),
             paste0("job61i3_w", 1:3),
             paste0("job61i4_w", 1:3),
             paste0("job61i5_w", 1:3),
             paste0("pse_w", 1:3),
             paste0("te_w", 1:3),
             paste0("emp_pr_w", 1:3),
             paste0("semp_w", 1:3),
             paste0("hs_bc_w", 1:3),
             paste0("ls_wc_w", 1:3),
             paste0("hs_wc_w", 1:3),
             paste0("ftj_w", 1:3),
             paste0("wrhr_w", 1:3),
             paste0("coh_w", 1:3),
             paste0("married_w", 1:3),
             paste0("nchild_w", 1:3),
             paste0("east_w", 1:3),
             paste0("urban_w", 1:3)
             )
dt7 <- dt6[varlist]

saveRDS(dt7, file = "Data/dt1c.rds")

#### End ####
