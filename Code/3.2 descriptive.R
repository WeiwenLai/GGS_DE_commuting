#### Start ####

# Present the correlations between latent variables

rm(list = ls())
gc()

#### WD ####
setwd("G:/My Drive/R Projects/GGS_DE_commuting")

#### Library ####
library(haven)
library(lavaan)

#### Data ####
dt1 <- readRDS("Data/dt1b.rds") 
dt99 <- dt1

#### SEM (baseline models: with measurement models only) ####
m1_s <- 
  "
  # Depression
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
  
  # Work-family conflift
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
  
  # Commuting time
  comtime_w1 ~ 1
  comtime_w2 ~ 1
  comtime_w3 ~ 1
  comtime_w1 ~~ comtime_w1
  comtime_w2 ~~ comtime_w2
  comtime_w3 ~~ comtime_w3

  # covariances among the work variables
  comtime_w1 ~~ comtime_w2 + comtime_w3
  comtime_w2 ~~ comtime_w3

  # covariances of work with each latent factor
  dep_w1 ~~ comtime_w1 + comtime_w2 + comtime_w3
  dep_w2 ~~ comtime_w1 + comtime_w2 + comtime_w3
  dep_w3 ~~ comtime_w1 + comtime_w2 + comtime_w3
  wfc_w1 ~~ comtime_w1 + comtime_w2 + comtime_w3
  wfc_w2 ~~ comtime_w1 + comtime_w2 + comtime_w3
  wfc_w3 ~~ comtime_w1 + comtime_w2 + comtime_w3
"
m1 <- cfa(m1_s, data = dt99, missing = "FIML")
summary(m1, standardized = TRUE)

cortab <- round(lavInspect(m1, what = "cor.all"), 2)
cortab[c("dep_w1","dep_w2","dep_w3","wfc_w1","wfc_w2","wfc_w3", "comtime_w1","comtime_w2","comtime_w3"),
       c("dep_w1","dep_w2","dep_w3","wfc_w1","wfc_w2","wfc_w3", "comtime_w1","comtime_w2","comtime_w3")]

#### End ####
