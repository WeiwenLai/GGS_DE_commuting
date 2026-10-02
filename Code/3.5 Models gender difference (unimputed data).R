#### Start ####

# based on imputated datasets

rm(list = ls())
gc()

#### WD ####
setwd("G:/My Drive/R Projects/GGS_DE_commuting")

#### Library ####
library(haven)
library(dplyr)
library(lavaan.mi)
library(semTools)

#### Data ####
dt1 <- readRDS("Data/dt1d.rds") 
dt99 <- dt1 |>
  na.omit()

#### Unconstrained models ####
m1_s <- "
  
  # Measurement models: Depression
  
  dep_w1 =~ NA*per21i2_w1 + a*per21i2_w1 + b*per21i4_w1 + c*per21i5_w1
  dep_w2 =~ NA*per21i2_w2 + a*per21i2_w2 + b*per21i4_w2 + c*per21i5_w2
  dep_w3 =~ NA*per21i2_w3 + a*per21i2_w3 + b*per21i4_w3 + c*per21i5_w3
  
  dep_w1 ~ 1
  dep_w2 ~ 1
  dep_w3 ~ 1
  
  per21i2_w1 ~~ per21i2_w2 + per21i2_w3
  per21i4_w1 ~~ per21i4_w2 + per21i4_w3
  per21i5_w1 ~~ per21i5_w2 + per21i5_w3
  
  per21i2_w2 ~~  per21i2_w3
  per21i4_w2 ~~  per21i4_w3
  per21i5_w2 ~~  per21i5_w3

  per21i2_w1 ~ ia*1
  per21i4_w1 ~ ib*1
  per21i5_w1 ~ ic*1
  
  per21i2_w2 ~ ia*1
  per21i4_w2 ~ ib*1
  per21i5_w2 ~ ic*1
  
  per21i2_w3 ~ ia*1
  per21i4_w3 ~ ib*1
  per21i5_w3 ~ ic*1
  
  a + b + c == 3
  ia + ib + ic == 0
  
  # Measurement models: work-family conflict

  wfc_w1 =~ NA*job61i1_w1 + d*job61i1_w1 + e*job61i2_w1 + f*job61i3_w1 + g*job61i4_w1 + h*job61i5_w1
  wfc_w2 =~ NA*job61i1_w2 + d*job61i1_w2 + e*job61i2_w2 + f*job61i3_w2 + g*job61i4_w2 + h*job61i5_w2
  wfc_w3 =~ NA*job61i1_w3 + d*job61i1_w3 + e*job61i2_w3 + f*job61i3_w3 + g*job61i4_w3 + h*job61i5_w3

  job61i1_w1 ~~ job61i1_w2 + job61i1_w3
  job61i2_w1 ~~ job61i2_w2 + job61i2_w3
  job61i3_w1 ~~ job61i3_w2 + job61i3_w3
  job61i4_w1 ~~ job61i4_w2 + job61i4_w3
  job61i5_w1 ~~ job61i5_w2 + job61i5_w3

  job61i1_w2 ~~ job61i1_w3
  job61i2_w2 ~~ job61i2_w3
  job61i3_w2 ~~ job61i3_w3
  job61i4_w2 ~~ job61i4_w3
  job61i5_w2 ~~ job61i5_w3
  
  wfc_w1 ~ 1
  wfc_w2 ~ 1
  wfc_w3 ~ 1

  job61i1_w1 ~ id*1
  job61i2_w1 ~ ie*1
  job61i3_w1 ~ ifn*1
  job61i4_w1 ~ ig*1
  job61i5_w1 ~ ih*1

  job61i1_w2 ~ id*1
  job61i2_w2 ~ ie*1
  job61i3_w2 ~ ifn*1
  job61i4_w2 ~ ig*1
  job61i5_w2 ~ ih*1

  job61i1_w3 ~ id*1
  job61i2_w3 ~ ie*1
  job61i3_w3 ~ ifn*1
  job61i4_w3 ~ ig*1
  job61i5_w3 ~ ih*1

  d + e + f + g + h == 5
  id + ie + ifn + ig + ih == 0
  
  # Structure model

  # Time-varying covariates: T1 -> T2
  comtime_w2 ~ comtime_w1 + wfc_w1 + age + mig + gv + te_w1 +
    emp_pr_w2 + semp_w2 + hs_wc_w2 +
    ftj_w2 + wrhr_w2 + married_w2 + nchild_w2 + east_w2 + urban_w2
  wfc_w2 ~ c(la1, la2)*comtime_w1 + wfc_w1 + dep_w1 + age + mig + gv + te_w1 +
    emp_pr_w2 + semp_w2 + hs_wc_w2 +
    ftj_w2 + wrhr_w2 + married_w2 + nchild_w2 + east_w2 + urban_w2
  dep_w2 ~ wfc_w1 + dep_w1 + age + mig + gv + te_w1 +
    emp_pr_w2 + semp_w2 + hs_wc_w2 +
    ftj_w2 + wrhr_w2 + married_w2 + nchild_w2 + east_w2 + urban_w2
  
  # Time-varying covariates: T2 -> T3
  comtime_w3 ~ comtime_w2 + wfc_w2 + age + mig + gv + te_w1 +
    emp_pr_w3 + semp_w3 + hs_wc_w3 +
    ftj_w3 + wrhr_w3 + married_w3 + nchild_w3 + east_w3 + urban_w3
  wfc_w3 ~ comtime_w2 + wfc_w2 + dep_w2 + age + mig + gv + te_w1 +
    emp_pr_w3 + semp_w3 + hs_wc_w3 +
    ftj_w3 + wrhr_w3 + married_w3 + nchild_w3 + east_w3 + urban_w3
  dep_w3 ~ c(lb1, lb2)*wfc_w2 + dep_w2 + age + mig + gv + te_w1 +
    emp_pr_w3 + semp_w3 + hs_wc_w3 +
    ftj_w3 + wrhr_w3 + married_w3 + nchild_w3 + east_w3 + urban_w3 + 
    c(lc1, lc2)*comtime_w1
    
  # Within-wave covariance
  # T1
  comtime_w1 ~~ wfc_w1 + dep_w1
  wfc_w1     ~~ dep_w1
  # T2
  comtime_w2 ~~ wfc_w2 + dep_w2
  wfc_w2     ~~ dep_w2
  # T3
  comtime_w3 ~~ wfc_w3 + dep_w3
  wfc_w3     ~~ dep_w3
  
  # direct effect
  dir1 := lc1
  dir2 := lc2
  # Indirect effect
  ind1 := la1 * lb1
  ind2 := la2 * lb2
  # Total effect
  total1 := ind1 + lc1
  total2 := ind2 + lc2
  
  # GEnder difference
  diff_dir := dir1 - dir2
  diff_ind := ind1 - ind2
  diff_total := total1 - total2
"
m1 <- sem.mi(m1_s, data = dt99, group = "gender", se.def = "mc", parallel = "snow", ncpus = 6)

#### Unconstrained models ####
m2_s <- "
  
  # Measurement models: Depression
  
  dep_w1 =~ NA*per21i2_w1 + a*per21i2_w1 + b*per21i4_w1 + c*per21i5_w1
  dep_w2 =~ NA*per21i2_w2 + a*per21i2_w2 + b*per21i4_w2 + c*per21i5_w2
  dep_w3 =~ NA*per21i2_w3 + a*per21i2_w3 + b*per21i4_w3 + c*per21i5_w3
  
  dep_w1 ~ 1
  dep_w2 ~ 1
  dep_w3 ~ 1
  
  per21i2_w1 ~~ per21i2_w2 + per21i2_w3
  per21i4_w1 ~~ per21i4_w2 + per21i4_w3
  per21i5_w1 ~~ per21i5_w2 + per21i5_w3
  
  per21i2_w2 ~~  per21i2_w3
  per21i4_w2 ~~  per21i4_w3
  per21i5_w2 ~~  per21i5_w3

  per21i2_w1 ~ ia*1
  per21i4_w1 ~ ib*1
  per21i5_w1 ~ ic*1
  
  per21i2_w2 ~ ia*1
  per21i4_w2 ~ ib*1
  per21i5_w2 ~ ic*1
  
  per21i2_w3 ~ ia*1
  per21i4_w3 ~ ib*1
  per21i5_w3 ~ ic*1
  
  a + b + c == 3
  ia + ib + ic == 0
  
  # Measurement models: work-family conflict

  wfc_w1 =~ NA*job61i1_w1 + d*job61i1_w1 + e*job61i2_w1 + f*job61i3_w1 + g*job61i4_w1 + h*job61i5_w1
  wfc_w2 =~ NA*job61i1_w2 + d*job61i1_w2 + e*job61i2_w2 + f*job61i3_w2 + g*job61i4_w2 + h*job61i5_w2
  wfc_w3 =~ NA*job61i1_w3 + d*job61i1_w3 + e*job61i2_w3 + f*job61i3_w3 + g*job61i4_w3 + h*job61i5_w3

  job61i1_w1 ~~ job61i1_w2 + job61i1_w3
  job61i2_w1 ~~ job61i2_w2 + job61i2_w3
  job61i3_w1 ~~ job61i3_w2 + job61i3_w3
  job61i4_w1 ~~ job61i4_w2 + job61i4_w3
  job61i5_w1 ~~ job61i5_w2 + job61i5_w3

  job61i1_w2 ~~ job61i1_w3
  job61i2_w2 ~~ job61i2_w3
  job61i3_w2 ~~ job61i3_w3
  job61i4_w2 ~~ job61i4_w3
  job61i5_w2 ~~ job61i5_w3
  
  wfc_w1 ~ 1
  wfc_w2 ~ 1
  wfc_w3 ~ 1

  job61i1_w1 ~ id*1
  job61i2_w1 ~ ie*1
  job61i3_w1 ~ ifn*1
  job61i4_w1 ~ ig*1
  job61i5_w1 ~ ih*1

  job61i1_w2 ~ id*1
  job61i2_w2 ~ ie*1
  job61i3_w2 ~ ifn*1
  job61i4_w2 ~ ig*1
  job61i5_w2 ~ ih*1

  job61i1_w3 ~ id*1
  job61i2_w3 ~ ie*1
  job61i3_w3 ~ ifn*1
  job61i4_w3 ~ ig*1
  job61i5_w3 ~ ih*1

  d + e + f + g + h == 5
  id + ie + ifn + ig + ih == 0
  
  # Structure model

  # Time-varying covariates: T1 -> T2
  comtime_w2 ~ comtime_w1 + wfc_w1 + age + mig + gv + te_w1 +
    emp_pr_w2 + semp_w2 + hs_wc_w2 +
    ftj_w2 + wrhr_w2 + married_w2 + nchild_w2 + east_w2 + urban_w2
  wfc_w2 ~ c(la, la)*comtime_w1 + wfc_w1 + dep_w1 + age + mig + gv + te_w1 +
    emp_pr_w2 + semp_w2 + hs_wc_w2 +
    ftj_w2 + wrhr_w2 + married_w2 + nchild_w2 + east_w2 + urban_w2
  dep_w2 ~ wfc_w1 + dep_w1 + age + mig + gv + te_w1 +
    emp_pr_w2 + semp_w2 + hs_wc_w2 +
    ftj_w2 + wrhr_w2 + married_w2 + nchild_w2 + east_w2 + urban_w2
  
  # Time-varying covariates: T2 -> T3
  comtime_w3 ~ comtime_w2 + wfc_w2 + age + mig + gv + te_w1 +
    emp_pr_w3 + semp_w3 + hs_wc_w3 +
    ftj_w3 + wrhr_w3 + married_w3 + nchild_w3 + east_w3 + urban_w3
  wfc_w3 ~ comtime_w2 + wfc_w2 + dep_w2 + age + mig + gv + te_w1 +
    emp_pr_w3 + semp_w3 + hs_wc_w3 +
    ftj_w3 + wrhr_w3 + married_w3 + nchild_w3 + east_w3 + urban_w3
  dep_w3 ~ c(lb, lb)*wfc_w2 + dep_w2 + age + mig + gv + te_w1 +
    emp_pr_w3 + semp_w3 + hs_wc_w3 +
    ftj_w3 + wrhr_w3 + married_w3 + nchild_w3 + east_w3 + urban_w3 +
    c(lc, lc)*comtime_w1
    
  # Within-wave covariance
  # T1
  comtime_w1 ~~ wfc_w1 + dep_w1
  wfc_w1     ~~ dep_w1
  # T2
  comtime_w2 ~~ wfc_w2 + dep_w2
  wfc_w2     ~~ dep_w2
  # T3
  comtime_w3 ~~ wfc_w3 + dep_w3
  wfc_w3     ~~ dep_w3
"
m2 <- sem.mi(m2_s, data = dt99, group = "gender", se.def = "mc", parallel = "snow", ncpus = 6)

# Compare models
lavTestLRT.mi(m1, m2)

# Save model results
saveRDS(c(m1, m2), file = "Output/model_gender_difference.rds")


#### End ####