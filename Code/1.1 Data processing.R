#### Start ####

# Main idea: keep employed individuals surveyed all the first three waves

rm(list = ls())
gc()

#### WD ####
setwd("G:/My Drive/R Projects/GGS_DE_commuting")

#### Library ####
library(haven)
library(dplyr)
library(zoo)

#### Data ####
##### Wave 1 ####
w1_r <- read_dta("G:/My Drive/Data/Freda/FReDA v6.0.0/Data/Stata/FREDAanchor1.dta")|> 
  select(id,   # ID
         age_reg, # Age 
         sex_reg, # Gender
         mig10, # Born in Germany
         isced11 # Education
         )
w1_a <- read_dta("G:/My Drive/Data/Freda/FReDA v6.0.0/Data/Stata/FREDAanchor2.dta")|> 
  select(id,
         east, # living in the east
         degurba, # degree of urbanization
         relstat, # relationship status
         sd55, # current activity status (employed, self employed)
         job42, # paid job last week
         job40, # full-time or part time
         job56, # weekly hours worked
         job16h, # commuting time in hours 
         job16m, # commuting time in minutes
         job61i1, # Work-family conflict scale
         job61i2,
         job61i3, 
         job61i4, 
         job61i5,
         flag35, # Missing data on children
         nkids, # Number of kids born (including biological, step)
         nkidslivu18, # Number of cohabiting children aged under 18
         per21i1, # Depression scale
         per21i2,
         per21i3,
         per21i4,
         per21i5
  )
w1_b <- read_dta("G:/My Drive/Data/Freda/FReDA v6.0.0/Data/Stata/FREDAanchor3.dta") |> 
  select(id,
         job42, # in paid work
         sd55, # current activity status (employed, self employed)
         job45, # Type of contract
         job46, # Sector
         job43i1, # Occupational classification
         job26, # working from home
         isco08 # Occupational codes
    )

# Merge
w1 <- w1_r |> 
  merge(w1_a, by = "id") |> 
  merge(w1_b, by = "id") |> 
  rename(job42_a = job42.x,
         job42_b = job42.y,
         sd55_a = sd55.x,
         sd55_b = sd55.y) |> 
  rename_with(~ paste0(.x, "_w1"), .cols = -c(id, age_reg, sex_reg, mig10))
  
# Select study population
w1_sp <- w1 |> 
  # 1) age 49 or younger
  filter(age_reg %in% 18:49) |> 
  # Wave a, in paid work (so that they can answer questions about commuting and work-family conflict)
  filter(job42_a_w1 %in% c(1, 4)) %>% 
  # Wave b, in paid work (so that they can answer full time/occupation classification/codes
  filter(job42_b_w1 %in% c(1, 4)) %>% 
  # Wave b, keep employed and self-employed (to ensure a comparable sample with later waves)
  filter(sd55_b_w1 %in% c(-9, -2, 2:3))

# A note about the first wave: as long as people indicated they were active in the paid work, they ...
# ... were asked for working full time (job40, wave a) and occupation codes (isco08, wave b)
# sd55 (activity status is asked to everyone)

##### Wave 2 #####
w2_b <- read_dta("G:/My Drive/Data/Freda/FReDA v6.0.0/Data/Stata/FREDAanchor5.dta") |> 
  select(id,
         isced11,
         degurba,
         east,
         relstat, 
         lfstat,
         job16h,
         job16m,
         job42,
         job61i1,
         job61i2,
         job61i3, 
         job61i4, 
         job61i5,
         flag35, 
         nkids, 
         nkidslivu18,
         job40,
         job45,
         job56,
         job46,
         job43i1,
         per21i2,
         per21i4,
         per21i5,
         job26,
         isco08,
         job66
  ) |> 
  rename_with(~ paste0(.x, "_w2"), .cols = -id)

# Study population in wave 2
w2_sp <- w2_b |> 
  # 1) employed (so that they can answer question related to work-family conflicts and other job characteristics)
  filter(job42_w2 %in% c(1, 4)) %>% 
  # 2) keep: a) missing (for later imputation); b) different working situations; c) other situations
  filter(lfstat_w2 %in% c(-7, 7:11))

# Respondents interviewed both in wave 1 and 2
sp_w12 <- w1_sp |> 
  merge(w2_sp, by = "id")

#### Wave 3 ####
w3_b <- read_dta("G:/My Drive/Data/Freda/FReDA v6.0.0/Data/Stata/FREDAanchor7.dta") %>% 
  select(id,
         east,
         degurba,
         isced11,
         relstat,
         lfstat,
         job16h,
         job16m,
         job42,
         job61i1,
         job61i2,
         job61i3, 
         job61i4, 
         job61i5,
         nkids, 
         nkidslivu18,
         job42, 
         job40,
         job56,
         job46,
         job43i1,
         per21i2,
         per21i4,
         per21i5,
         job26,
         isco08,
         job66) |> 
  rename_with(~ paste0(.x, "_w3"), .cols = -id)

# Study population in wave 3
w3_sp <- w3_b |>
  # 1) employed (so that they can answer question related to work-family conflicts and other job characteristics)
  filter(job42_w3 %in% c(1, 4)) %>% 
  # 2) keep: a) missing (for later imputation); b) different working situations; c) other situations
  filter(lfstat_w3 %in% c(-7, 7:11))

# Respondents interviewed both in waves 1, 2, and 3
sp_w123 <- sp_w12 |> 
  merge(w3_sp, by = "id")

#### Save data ####
saveRDS(sp_w123, file = "Data/dt11.rds")

#### End ####
