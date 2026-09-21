#### Start ####

# Idea: testing the measurement invariance across measurement occasion
# Short conclusion: both latent variables meet scalar invariance

#### WD ####
setwd("G:/My Drive/R Projects/GGS_DE_commuting")

#### Library ####
library(dplyr)
library(haven)
library(lavaan)
library(semTools)

#### Data ####
dt1 <- readRDS("Data/dt1b.rds")
dt99 <- dt1

#### CFA of depression ####
##### Configural #####
m1 <- 
  "
  dep_w1 =~ per21i2_w1 + per21i4_w1 + per21i5_w1
  dep_w2 =~ per21i2_w2 + per21i4_w2 + per21i5_w2
  dep_w3 =~ per21i2_w3 + per21i4_w3 + per21i5_w3
  
  per21i2_w1 ~~ per21i2_w2 + per21i2_w3
  per21i4_w1 ~~ per21i4_w2 + per21i4_w3
  per21i5_w1 ~~ per21i5_w2 + per21i5_w3
  
  per21i2_w2 ~~  per21i2_w3
  per21i4_w2 ~~  per21i4_w3
  per21i5_w2 ~~  per21i5_w3
  
  dep_w1 ~ 0
  dep_w2 ~ 0
  dep_w3 ~ 0
  
  per21i2_w1 ~ 1
  per21i4_w1 ~ 1
  per21i5_w1 ~ 1
  
  per21i2_w2 ~ 1
  per21i4_w2 ~ 1
  per21i5_w2 ~ 1
  
  per21i2_w3 ~ 1
  per21i4_w3 ~ 1
  per21i5_w3 ~ 1
"
config_dep <- cfa(m1, data = dt99, missing = "FIML")
summary(config_dep, standardized = TRUE)

##### Metric #####
m2 <- 
  "
  dep_w1 =~ a*per21i2_w1 + b*per21i4_w1 + c*per21i5_w1
  dep_w2 =~ a*per21i2_w2 + b*per21i4_w2 + c*per21i5_w2
  dep_w3 =~ a*per21i2_w3 + b*per21i4_w3 + c*per21i5_w3
  
  per21i2_w1 ~~ per21i2_w2 + per21i2_w3
  per21i4_w1 ~~ per21i4_w2 + per21i4_w3
  per21i5_w1 ~~ per21i5_w2 + per21i5_w3
  
  per21i2_w2 ~~  per21i2_w3
  per21i4_w2 ~~  per21i4_w3
  per21i5_w2 ~~  per21i5_w3
  
  dep_w1 ~ 0
  dep_w2 ~ 0
  dep_w3 ~ 0
  
  per21i2_w1 ~ 1
  per21i4_w1 ~ 1
  per21i5_w1 ~ 1
  
  per21i2_w2 ~ 1
  per21i4_w2 ~ 1
  per21i5_w2 ~ 1
  
  per21i2_w3 ~ 1
  per21i4_w3 ~ 1
  per21i5_w3 ~ 1
"
metric_dep <- cfa(m2, data = dt99, missing = "FIML")
summary(metric_dep, standardized = TRUE)

##### Scalar #####
m3 <- 
  "
  dep_w1 =~ a*per21i2_w1 + b*per21i4_w1 + c*per21i5_w1
  dep_w2 =~ a*per21i2_w2 + b*per21i4_w2 + c*per21i5_w2
  dep_w3 =~ a*per21i2_w3 + b*per21i4_w3 + c*per21i5_w3
  
  per21i2_w1 ~~ per21i2_w2 + per21i2_w3
  per21i4_w1 ~~ per21i4_w2 + per21i4_w3
  per21i5_w1 ~~ per21i5_w2 + per21i5_w3
  
  per21i2_w2 ~~  per21i2_w3
  per21i4_w2 ~~  per21i4_w3
  per21i5_w2 ~~  per21i5_w3
  
  dep_w1 ~ 0
  dep_w2 ~ 1
  dep_w3 ~ 1
  
  per21i2_w1 ~ ia*1
  per21i4_w1 ~ ib*1
  per21i5_w1 ~ ic*1
  
  per21i2_w2 ~ ia*1
  per21i4_w2 ~ ib*1
  per21i5_w2 ~ ic*1
  
  per21i2_w3 ~ ia*1
  per21i4_w3 ~ ib*1
  per21i5_w3 ~ ic*1
"
scalar_dep <- cfa(m3, data = dt99, missing = "FIML")
summary(scalar_dep, standardized = TRUE)

##### compare models #####
mfit_dep <- compareFit(config_dep, metric_dep, scalar_dep)

#### CFA of work-family conflict ####
##### Configural #####
m4 <- 
  "
  wfc_w1 =~ job61i1_w1 + job61i2_w1 + job61i3_w1 + job61i4_w1 + job61i5_w1
  wfc_w2 =~ job61i1_w2 + job61i2_w2 + job61i3_w2 + job61i4_w2 + job61i5_w2
  wfc_w3 =~ job61i1_w3 + job61i2_w3 + job61i3_w3 + job61i4_w3 + job61i5_w3
  
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
  
  wfc_w1 ~ 0
  wfc_w2 ~ 0
  wfc_w3 ~ 0
  
  job61i1_w1 ~ 1
  job61i2_w1 ~ 1
  job61i3_w1 ~ 1
  job61i4_w1 ~ 1
  job61i5_w1 ~ 1
  
  job61i1_w2 ~ 1
  job61i2_w2 ~ 1
  job61i3_w2 ~ 1
  job61i4_w2 ~ 1
  job61i5_w2 ~ 1
  
  job61i1_w3 ~ 1
  job61i2_w3 ~ 1
  job61i3_w3 ~ 1
  job61i4_w3 ~ 1
  job61i5_w3 ~ 1
"
config_wfc <- cfa(m4, data = dt99, missing = "FIML")
summary(config_wfc, standardized = TRUE)

##### Metric #####
m5 <- 
  "
  wfc_w1 =~ a*job61i1_w1 + b*job61i2_w1 + c*job61i3_w1 + d*job61i4_w1 + e*job61i5_w1
  wfc_w2 =~ a*job61i1_w2 + b*job61i2_w2 + c*job61i3_w2 + d*job61i4_w2 + e*job61i5_w2
  wfc_w3 =~ a*job61i1_w3 + b*job61i2_w3 + c*job61i3_w3 + d*job61i4_w3 + e*job61i5_w3
  
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
  
  wfc_w1 ~ 0
  wfc_w2 ~ 0
  wfc_w3 ~ 0
  
  job61i1_w1 ~ 1
  job61i2_w1 ~ 1
  job61i3_w1 ~ 1
  job61i4_w1 ~ 1
  job61i5_w1 ~ 1
  
  job61i1_w2 ~ 1
  job61i2_w2 ~ 1
  job61i3_w2 ~ 1
  job61i4_w2 ~ 1
  job61i5_w2 ~ 1
  
  job61i1_w3 ~ 1
  job61i2_w3 ~ 1
  job61i3_w3 ~ 1
  job61i4_w3 ~ 1
  job61i5_w3 ~ 1
"
metric_wfc <- cfa(m5, data = dt99, missing = "FIML")
summary(metric_wfc, standardized = TRUE)

##### Scalar #####
m6 <- 
  "
  wfc_w1 =~ a*job61i1_w1 + b*job61i2_w1 + c*job61i3_w1 + d*job61i4_w1 + e*job61i5_w1
  wfc_w2 =~ a*job61i1_w2 + b*job61i2_w2 + c*job61i3_w2 + d*job61i4_w2 + e*job61i5_w2
  wfc_w3 =~ a*job61i1_w3 + b*job61i2_w3 + c*job61i3_w3 + d*job61i4_w3 + e*job61i5_w3
  
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
  
  wfc_w1 ~ 0
  wfc_w2 ~ 0
  wfc_w3 ~ 0
  
  job61i1_w1 ~ ia*1
  job61i2_w1 ~ ib*1
  job61i3_w1 ~ ic*1
  job61i4_w1 ~ id*1
  job61i5_w1 ~ ie*1
  
  job61i1_w2 ~ ia*1
  job61i2_w2 ~ ib*1
  job61i3_w2 ~ ic*1
  job61i4_w2 ~ id*1
  job61i5_w2 ~ ie*1
  
  job61i1_w3 ~ ia*1
  job61i2_w3 ~ ib*1
  job61i3_w3 ~ ic*1
  job61i4_w3 ~ id*1
  job61i5_w3 ~ ie*1
"
scalar_wfc <- cfa(m6, data = dt99, missing = "FIML")
summary(scalar_wfc, standardized = TRUE)

##### compare models #####
mfit_wfc <- compareFit(config_wfc, metric_wfc, scalar_wfc)
summary(mfit_wfc)

#### Effect coding approach for both latent variables ####
##### depression #####
m7 <- 
  "
  dep_w1 =~ NA*per21i2_w1 + a*per21i2_w1 + b*per21i4_w1 + c*per21i5_w1
  dep_w2 =~ NA*per21i2_w2 + a*per21i2_w2 + b*per21i4_w2 + c*per21i5_w2
  dep_w3 =~ NA*per21i2_w3 + a*per21i2_w3 + b*per21i4_w3 + c*per21i5_w3
  
  per21i2_w1 ~~ per21i2_w2 + per21i2_w3
  per21i4_w1 ~~ per21i4_w2 + per21i4_w3
  per21i5_w1 ~~ per21i5_w2 + per21i5_w3
  
  per21i2_w2 ~~  per21i2_w3
  per21i4_w2 ~~  per21i4_w3
  per21i5_w2 ~~  per21i5_w3
  
  dep_w1 ~ 1
  dep_w2 ~ 1
  dep_w3 ~ 1
  
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
"
scalar_dep_ec <- cfa(m7, data = dt99, missing = "FIML")
summary(scalar_dep, standardized = TRUE)

##### compare models #####
dep_compare <- compareFit(scalar_dep, scalar_dep_ec)
summary(dep_compare)

#### Work-life conflict ####
m8 <- 
  "
  wfc_w1 =~ NA*job61i1_w1 + a*job61i1_w1 + b*job61i2_w1 + c*job61i3_w1 + d*job61i4_w1 + e*job61i5_w1
  wfc_w2 =~ NA*job61i1_w2 + a*job61i1_w2 + b*job61i2_w2 + c*job61i3_w2 + d*job61i4_w2 + e*job61i5_w2
  wfc_w3 =~ NA*job61i1_w3 + a*job61i1_w3 + b*job61i2_w3 + c*job61i3_w3 + d*job61i4_w3 + e*job61i5_w3

  a + b + c + d + e == 5

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

  job61i1_w1 ~ ia*1
  job61i2_w1 ~ ib*1
  job61i3_w1 ~ ic*1
  job61i4_w1 ~ id*1
  job61i5_w1 ~ ie*1

  job61i1_w2 ~ ia*1
  job61i2_w2 ~ ib*1
  job61i3_w2 ~ ic*1
  job61i4_w2 ~ id*1
  job61i5_w2 ~ ie*1

  job61i1_w3 ~ ia*1
  job61i2_w3 ~ ib*1
  job61i3_w3 ~ ic*1
  job61i4_w3 ~ id*1
  job61i5_w3 ~ ie*1

  ia + ib + ic + id + ie == 0

  wfc_w1 ~ 1
  wfc_w2 ~ 1
  wfc_w3 ~ 1
  "

scalar_wfc_ec <- cfa(m8, data = dt99)
summary(scalar_wfc_ec, fit.measures = TRUE, standardized = TRUE)

##### compare models #####
wfc_compare <- compareFit(scalar_wfc, scalar_wfc_ec)
summary(wfc_compare)

#### End ####
