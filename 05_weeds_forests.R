#########################
##
## Date of creation: Aug 12 2018
## Date last modified: Sept 26 2018
##                     April 12 2019
##                     April 30 2019 (go through and clean up)
##                     June 19 2019 (clean and check for git publication)
## Author: Gina
## Purpose: Use gbm package to get variable importance
##          
## Inputs: data-tidy-PIVI
##
## Outputs: stats_gbm.csv
## NOTE: Just use density data, bc that's the only one w/enough points. Use PICV. 
## 
#########################

rm(list = ls())
library(tidyverse)
library(gbm) #--does boosted regression trees, Elia et al. 2008
library(caret)
library(here)
setwd(here())


# How many biomass points WOULD we have? ----------------------------------

wow <-  read_csv("data-tidy-PIVI.csv") %>%
  select(-denLRR) %>%
  rename(bio = bioLRR) %>%
  
  mutate(
    # replace 'IC' in sys_tillage w/NA
    sys_tillage = ifelse(sys_tillage == "IC", NA, sys_tillage),
    sys_weedmgmt2 = ifelse(sys_weedmgmt2 == "IC", NA, sys_weedmgmt2)) %>%
  
  mutate_if(is.character, as.factor) %>%
  
  # What are the things we want to investigate? WE have 9 predictors.
  select(sys_tillage, weedmsmt_unit, peren_divYN, fallow_simpYN, sys_weedmgmt2,
         climclasslat2, species_rat, PICV_diff, mono_YN, bio) %>%
  filter(!is.na(bio)) %>%
  na.omit()


# Only do it on density data ----------------------------------------------

wow <-  read_csv("data-tidy-PIVI.csv") %>%
  select(-bioLRR) %>%
  rename(den = denLRR) %>%
  
  mutate(
    # replace 'IC' in sys_tillage w/NA
    sys_tillage = ifelse(sys_tillage == "IC", NA, sys_tillage),
    sys_weedmgmt2 = ifelse(sys_weedmgmt2 == "IC", NA, sys_weedmgmt2)) %>%
    
  mutate_if(is.character, as.factor) %>%
  
  # What are the things we want to investigate? WE have 9 predictors.
  select(sys_tillage, weedmsmt_unit, peren_divYN, fallow_simpYN, sys_weedmgmt2,
         climclasslat2, species_rat, 
         #PISD_diff, 
         PICV_diff, 
         mono_YN, den) %>%
  filter(!is.na(den)) %>%
  na.omit()



# Tune the model ----------------------------------------------------------
# Make grid of interaction.depth, n.trees, and shrinkages

gbmGrid <- expand.grid(interaction.depth = seq(1, 7, by = 2),
                       n.trees = seq(100, 500, by = 50),
                       shrinkage = c(0.001, 0.01, 0.1),
                       n.minobsinnode = 10)

set.seed(951983)

# Train takes matrices, it doesn't like the formula interface
wowY <- wow$den
wowX = wow[,1:9]

gbmTune <- caret::train(den ~., 
                 data = wow,
                 method = "gbm",
                 tuneGrid = gbmGrid,
                 verbose = FALSE)


plot(gbmTune)


# Fit model using best tuning parameters ----------------------------------

# Run a boosted tree using gbm package (page 217 of Applied Pred Modelling)
# Use the tuning parameters that optimized our RMSE (depth of 3, 200 trees, shrinkage of 0.01)
# Use a gaussian distribution because our response variable is continuous

boost_den <- gbm(den~., data = wow, distribution = "gaussian",
                 n.trees = 200, interaction.depth = 3, 
                 shrinkage = 0.01)

summary(boost_den)
gbmres <- summary(boost_den) %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  select(-rowname) %>%
  rename("rel_inf" = rel.inf)

write_csv(gbmres, "stats_gbm.csv")


