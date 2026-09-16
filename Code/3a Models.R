#### Start ####

# A step further based on the script (3 Models) by including a measurement model

#### WD ####
setwd("G:/My Drive/R Projects/GGS_DE_commuting")

#### Library ####
library(lavaan)
library(dplyr)

#### Data ####
dt99 <- readRDS("Data/dt99a.rds") %>% 
  select(!c(dep, wlb))

#### Model specification ####
model <- "
  # Measurement models
  dep =~ wel11a + wel11b + wel11c + wel11d + wel11e
  wlb =~ wrk15a + wrk15b + wrk15c + wrk15d
  
  # Regression
  dep ~ c(cg1, cg2)*commute10 + wrkhr + Fulltime + Skilled_worker + Managers_n_professionals +
          Public_sector + Permanent_contract + child04 + child514 + age + 
          Post_secondary + College + LAT + Cohabiting + Married
  wlb ~ c(ag1, ag2)*commute10 + wrkhr + Fulltime + Skilled_worker + Managers_n_professionals +
          Public_sector + Permanent_contract + child04 + child514 + age + 
          Post_secondary + College + LAT + Cohabiting + Married
  dep ~ c(bg1,bg2)*wlb + wrkhr + Fulltime + Skilled_worker + Managers_n_professionals +
          Public_sector + Permanent_contract + child04 + child514 + age + 
          Post_secondary + College + LAT + Cohabiting + Married
  
  # indirect effect (a*b)           
  abg1 := ag1*bg1 
  abg2 := ag2*bg2

  # total effect
  totalg1 := cg1 + (ag1*bg1)
  totalg2 := cg2 + (ag2*bg2)
"
# Multi group comparison
fit1 <- sem(model, data = dt99, group = "Female",
            se = "boot", bootstrap = 500, meanstructure = TRUE)
summary(fit1, remove.unused = FALSE)
# Direct effect by gender
lavTestWald(fit1, constraints = "cg1==cg2")
# Indirect effect by gender
lavTestWald(fit1, constraints = "abg1==abg2")
# Total effect by gender
lavTestWald(fit1, constraints = "totalg1==totalg2")

#### End ####