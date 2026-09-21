#### Start ####

# Objective: data management
# 1) construct variables
# 2) deal with missing values

rm(list = ls())
gc()

#### WD ####
setwd("G:/My Drive/R Projects/GGS_DE_commuting")

#### Library ####
library(haven)
library(dplyr)
library(zoo)

#### Data ####
dt1 <- readRDS("Data/dt1a.rds")

#### Commuting time #### 
# Convert them into 10 minutes
dt2 <- dt1 %>% 
  # Why dropping them: -8 -> the answer option "does not apply"; -2 -> no answer
  # I just dont know what them mean
  filter(!job16h_w1 %in% c(-8, -2) &
           !job16h_w2 %in% c(-8, -2) &
           !job16h_w3 %in% c(-8, -2)) %>% 
  mutate(comtime_w1 = job16h_w1 * 60 + job16m_w1,
         comtime_w2 = job16h_w2 * 60 + job16m_w2,
         comtime_w3 = job16h_w3 * 60 + job16m_w3) %>% 
  mutate(comtime_w1 = case_when(job16h_w1 == -9 | job16m_w1 == -9 ~ NA,
                                TRUE ~ comtime_w1),
         comtime_w2 = case_when(job16h_w2 == -9 | job16m_w2 == -9 ~ NA,
                                TRUE ~ comtime_w2),
         comtime_w3 = case_when(job16h_w3 == -9 | job16m_w3 %in% c(-9, -6)  ~ NA,
                                TRUE ~ comtime_w3)) %>% 
  mutate_at(vars(comtime_w1, comtime_w2, comtime_w3), ~ case_when(.x > 180 ~ 180,
                                                                  TRUE ~ .x)) %>% 
  mutate_at(vars(comtime_w1, comtime_w2, comtime_w3), ~ .x/10)  
  

#### Depression ####
dt3 <- dt2 %>% 
  mutate(across(paste0("per21i2_w", 1:3), ~ case_when(.x < 0 ~ NA, 
                                                      TRUE ~ .x))) %>% 
  mutate(across(paste0("per21i4_w", 1:3), ~ case_when(.x < 0 ~ NA, 
                                                      TRUE ~ .x))) %>% 
  mutate(across(paste0("per21i5_w", 1:3), ~ case_when(.x < 0 ~ NA, 
                                                      TRUE ~ .x)))

#### Work-family conflict ####
# Flip the scale to make a higher value indicate greater conflict
dt4 <- dt3 %>% 
  mutate(across(starts_with("job61i"), ~ case_when(.x < 0 ~ NA,
                                                   TRUE ~ .x))) %>% 
  mutate(across(starts_with("job61i"), ~ 5 - .x))

#### Other variables ####
# time-invariant: age, gender, mig
dt5 <- dt4 %>% 
  mutate(
    age = age_reg,
    gender = case_when(sex_reg %in% 1:2 ~ sex_reg) |> factor(labels = c("Men", "Women")),
    mig = case_when(mig10  %in% 1:2 ~ mig10 ) |> factor(labels = c("No", "Yes"))
  )
  
# time-varying
  # Education: edu_w1, edu_w2, edu_w3
  # Occupation: occ_w1, occ_w2, occ_w3
  # Full-time: ftj_w1, ftj_w2, ftj_w3
  # Hours worked: wrhr_w1, wrhr_w2, wrhr_w3
  # Employment status: emp_w1, emp_w2, emp_w3
  # Partnership status: relstat_w1, relstat_w2, relstat_w3 
  # number of co-resident children aged 18 or younger: nchild_w1, nchild_w2, nchild_w3
  # Living in the east: east_w1, east_w2, east_w3
  # Degree of urbanization: east_w1, east_w2, east_w3
dt6 <- dt5 %>% 
  mutate(
    across(paste0("isced11_w", 1:3), ~ case_when(.x %in% 1:3 ~ 1,
                                                 .x == 4 ~ 2,
                                                 .x %in% 6:8 ~ 3) |> 
             factor(labels = c("Upper secondary",
                               "Post-secondary",
                               "Tertiary")),
           .names = "{sub('isced11', 'edu', .col)}")
  ) %>% 
  mutate(across(starts_with("isco08"), ~ case_when(.x > 0 ~ .x,
                                                   TRUE ~ NA))) %>% 
  mutate(occ_w1 = isco08_w1, # Nothing complicated for the first wave
         # Second wave
         occ_w2 = case_when(job66_w2 == 1 ~ occ_w1,      # Those having the same job as the previous wave
                            job66_w2 == 2 ~ isco08_w2),  # Those with a different job
         # Third wave
         occ_w3 = case_when(job66_w3 == 1 ~ occ_w2,      # Those having the same job as the previous wave
                            job66_w3 == 2 ~ isco08_w3)   # Those with a different job
  ) %>% 
  mutate(across(starts_with("occ_w"), ~ case_when(.x > 1 & .x < 4000 ~ 1,
                                                  .x >= 4000 & .x < 6000 ~ 2,
                                                  .x >= 6000 & .x < 8000 ~ 3,
                                                  .x >= 8000 & .x < 10000 ~ 4) |>
                  factor(labels = c("High-skilled white-collar",
                                    "Low-skilled white-collar",
                                    "High-skilled blue-collar",
                                    "Low-skilled blue-collar")))) %>% 
  mutate(across(starts_with("job40"), ~ case_when(.x == 1 ~ 2,
                                                  .x %in% 2:3 ~ 1) |>
                  factor(labels = c("No", "Yes")),
                .names = "{sub('job40', 'ftj', .col)}")) %>% 
  mutate(across(starts_with("job56"), ~ case_when(.x >= 0 ~ .x,
                                                  TRUE ~ NA),
                .names = "{sub('job56', 'wrhr', .col)}")) %>% 
  # updating job46 for wave 2 and 3
  mutate(job46_w2 = case_when(job66_w2 == 1 ~ job46_w1,
                              TRUE ~ job46_w2),
         job46_w3 = case_when(job66_w3 == 1 ~ job46_w2,
                              TRUE ~ job46_w3)) %>% 
  mutate(emp_w1 = case_when(sd55_b_w1 == 2 & job46_w1 == 2 ~ 1,
                            sd55_b_w1 == 2 & job46_w1 %in% c(1, 3) ~ 2,
                            sd55_b_w1 == 3 ~ 3),
         emp_w2 = case_when(lfstat_w2 %in% 7:9 & job46_w2 == 2 ~ 1,
                            lfstat_w2 %in% 7:9 & job46_w2 %in% c(1, 3) ~ 2,
                            lfstat_w2 == 10 ~ 3),
         emp_w3 = case_when(lfstat_w3 %in% 7:9 & job46_w3 == 2 ~ 1,
                            lfstat_w3 %in% 7:9 & job46_w3 %in% c(1, 3) ~ 2,
                            lfstat_w3 == 10 ~ 3)) %>% 
  mutate(across(starts_with("emp"), ~ factor(.x, labels = c("Employed, public sector",
                                                            "Employed, private sector",
                                                            "Self-employed")))) %>% 
  mutate(across(starts_with("relstat"), ~ case_when(.x %in% c(3, 8, 11) ~ 2,
                                                    .x %in% c(4, 5) ~ 3,
                                                    .x >= 1 ~ 1) |> 
                  factor(labels = c("Single", "Cohabiting", "Married")))) %>% 
  mutate(nchild_w1 = case_when(nkidslivu18_w1 >= 0 ~ nkidslivu18_w1,
                               TRUE ~ NA),
         nchild_w2 = case_when(nkidslivu18_w2 >= 0 ~ nkidslivu18_w2,
                               TRUE ~ NA),
         nchild_w3 = case_when(nkidslivu18_w3 >= 0 ~ nkidslivu18_w3,
                               TRUE ~ NA)) %>% 
  mutate(across(starts_with("east"), ~ case_when(.x %in% 0:1 ~ .x,
                                                 TRUE ~ NA) |>
                  factor(labels = c("No", "Yes")))) %>% 
  mutate(across(starts_with("degurba"), ~ case_when(.x == 3 ~ 1,
                                                    .x %in% 1:2 ~ 0) |>
                  factor(labels = c("No", "Yes")),
                .names = "{sub('degurba', 'urban', .col)}"))

#### Save data ####
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
             paste0("edu_w", 1:3),
             paste0("emp_w", 1:3),
             paste0("occ_w", 1:3),
             paste0("ftj_w", 1:3),
             paste0("wrhr_w", 1:3),
             paste0("relstat_w", 1:3),
             paste0("nchild_w", 1:3),
             paste0("east_w", 1:3),
             paste0("urban_w", 1:3)
             )
dt12 <- saveRDS(dt6[varlist], file = "Data/dt1b.rds")

#### End ####
