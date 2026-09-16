#### Start ####

#### WD ####
setwd("G:/My Drive/R Projects/GGS_DE_commuting")

#### Library ####
library(lavaan)

#### Data ####
dt99 <- readRDS("Data/dt99a.rds")

#### Model specification ####
model <- "
  dep ~ c(cg1, cg2)*commute10 + wrkhr + Fulltime + Skilled_worker + Managers_n_professionals +
          Public_sector + Permanent_contract + child04 + child514 + age + 
          Post_secondary + College + LAT + Cohabiting + Married
  wlb ~ c(ag1, ag2)*commute10 + wrkhr + Fulltime + Skilled_worker + Managers_n_professionals +
          Public_sector + Permanent_contract + child04 + child514 + age + 
          Post_secondary + College + LAT + Cohabiting + Married
  dep ~ c(bg1,bg2)*wlb + wrkhr + Fulltime + Skilled_worker + Managers_n_professionals +
          Public_sector + Permanent_contract + child04 + child514 + age + 
          Post_secondary + College + LAT + Cohabiting + Married
  
  # direct effect
  direff1 := cg1
  direff2 := cg2
  
  # indirect effect (a*b)           
  indeff1 := ag1*bg1 
  indeff2 := ag2*bg2

  # total effect
  toleff1 := cg1 + (ag1*bg1)
  toleff2 := cg2 + (ag2*bg2)
"


# Multigroup comparison
fit1 <- sem(model, data = dt99, group = "Female",
            se = "boot", bootstrap = 500, meanstructure = TRUE)
summary(fit1)


# Direct effect by gender
lavTestWald(fit1, constraints = "cg1==cg2")
# Indirect effect by gender
lavTestWald(fit1, constraints = "abg1==abg2")
# Total effect by gender
lavTestWald(fit1, constraints = "totalg1==totalg2")

#### End ####