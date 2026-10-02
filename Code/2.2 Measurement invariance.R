#### Start ####

# Idea: testing the longitudinal invariance by gender
# Short observation: yes, measurement invariance supported across gender and measurement occasion

#### WD ####
setwd("G:/My Drive/R Projects/GGS_DE_commuting")

#### Library ####
library(dplyr)
library(haven)
library(lavaan)
library(semTools)

#### Data ####
dt1 <- readRDS("Data/dt1b.rds")

dt99 <- dt1 |> 
  filter(!is.na(gender))

#### CFA ####
cfa_conf <- "
  dep_w1 =~ per21i2_w1 + per21i4_w1 + per21i5_w1
  dep_w2 =~ per21i2_w2 + per21i4_w2 + per21i5_w2
  dep_w3 =~ per21i2_w3 + per21i4_w3 + per21i5_w3
"
longFacNames <- list(dep = c("dep_w1", "dep_w2", "dep_w3"))

# Longitudinal metric + scalar, free across gender
fit_long <- measEq.syntax(cfa_conf, data = dt99, group = "gender",
                          ID.fac = "std.lv",
                          longFacNames = longFacNames,
                          long.equal = c("loadings", "intercepts"),
                          return.fit = TRUE,
                          missing = "FIML")

# Add metric invariance across gender
fit_long_grpM <- measEq.syntax(cfa_conf, data = dt99, group = "gender",
                               ID.fac = "std.lv",
                               longFacNames = longFacNames,
                               long.equal = c("loadings", "intercepts"),
                               group.equal = "loadings",
                               return.fit = TRUE,
                               missing = "FIML")

mfit_dep <- compareFit(fit_long, fit_long_grpM)
summary(mfit_dep)

#### CFA: work-family conflict ####
cfa_conf_wfc <- "
  wfc_w1 =~ job61i1_w1 + job61i2_w1 + job61i3_w1 + job61i4_w1 + job61i5_w1
  wfc_w2 =~ job61i1_w2 + job61i2_w2 + job61i3_w2 + job61i4_w2 + job61i5_w2
  wfc_w3 =~ job61i1_w3 + job61i2_w3 + job61i3_w3 + job61i4_w3 + job61i5_w3
"
longFacNames_wfc <- list(wfc = c("wfc_w1", "wfc_w2", "wfc_w3"))

# Longitudinal metric + scalar, free across gender
fit_long_wfc <- measEq.syntax(cfa_conf_wfc, data = dt99, group = "gender",
                              ID.fac = "std.lv",
                              longFacNames = longFacNames_wfc,
                              long.equal = c("loadings", "intercepts"),
                              return.fit = TRUE,
                              missing = "FIML")

# Add metric invariance across gender
fit_long_grpM_wfc <- measEq.syntax(cfa_conf_wfc, data = dt99, group = "gender",
                                   ID.fac = "std.lv",
                                   longFacNames = longFacNames_wfc,
                                   long.equal = c("loadings", "intercepts"),
                                   group.equal = "loadings",
                                   return.fit = TRUE,
                                   missing = "FIML")

mfit_wfc <- compareFit(fit_long_wfc, fit_long_grpM_wfc)
summary(mfit_wfc)

#### End ####
