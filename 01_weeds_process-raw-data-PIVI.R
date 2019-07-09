#########################
##
## Date of creation: Dec 10 2018
## Date last modified: Jan 9 2018 (number of points updated w/removal of Barberi study)
##                     March 1 2019 (fixed the one Ma that should be M)
##                     April 30 2019 (made it read in data-raw-PIVI)
##                     June 19 2019 (cleaned and checked for publishing to git)
##
## Author: Gina Nichols (virginia.nichols@gmail.com)
##
## Purpose: 1. Average weed responses over variables not of interest
##          2. Change response values of 0 to NA
##          3. Calculate ln of (div/simp) response ratio (biomass and density)
##          4. Deal with different time reporting
##          5. Make desired modifier categories
##          6. Order/select columns, write to csv
##
## Inputs: data-raw-PIVI
##
## Outputs: data-tidy-PIVI
##
## NOTES: 
##
#########################


##### Clear environment and load packages #####
rm(list=ls())
library(tidyverse) #--gets all the things
library(lubridate) #--for dates
library(readxl) #--used to read Excel files
library(here) #--for using relative paths
setwd(here())

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Read in raw data (918 comps)--------------------------------------------------------
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

d0 <- read_csv("data-raw-PIVI.csv") %>%
  group_by(study_no, reference) %>%
  mutate(nobs = n()) %>%
  select(nobs, everything())

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 1. Pool points where appropriate (594 comps)------------------------------------------------------
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


d1 <- d0 %>%
  
  group_by(study_no, reference, lat, long, 
           study_age, # only want to pool within years, not across years
           data_type, reps, checkcrop,
           cropseq_simp, rotlength_simp, speciesno_simp, 
           cropseq_div, rotlength_div, speciesno_div, 
           sys_tillage, sys_weedmgmt, weedmsmt_unit, peren_divYN, fallow_simpYN, 
           PISD_diff, PICV_diff) %>%
  
  summarise(nobs = mean(nobs), #--so we know where we pooled
            bio_simp = mean(bio_simp, na.rm = T),
            bio_div = mean(bio_div, na.rm = T),
            
            den_simp = mean(den_simp, na.rm = T),
            den_div = mean(den_div, na.rm = T)) %>%
  
  ungroup() %>%
  
  # Count number of obs in each study after pooling
  group_by(study_no, reference) %>%
  mutate(newnobs = n()) %>%
  select(newnobs, nobs, everything())


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 2. Change 0s to NAs (592 comps)--------------------------------------------------------
# NOTE: The response ratio will become NA
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


d2 <- d1 %>%
  
  mutate(bio_simp = ifelse(bio_simp == 0, NA, bio_simp),
         bio_div = ifelse(bio_div == 0, NA, bio_div),
         
         den_simp = ifelse(den_simp == 0, NA, den_simp),
         den_div = ifelse(den_div == 0, NA, den_div))
  
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 3. Calculate log of response ratio (583 comps)---------------------------------------------------
# NOTE: Elminated 9 comparisons bc of 0s
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
d3 <- d2 %>%
  
  # Calculate log of ratio
  mutate(bioLRR = log(bio_div / bio_simp),
         denLRR = log(den_div / den_simp)) %>%
  
  # Get rid of raw values
  select(-bio_simp, -bio_div,
         -den_simp, -den_div) %>%
  
  filter( !(is.na(bioLRR) & is.na(denLRR))) %>%
  
  # Find max study age w/in group
  # Group by the same things we pooled by EXCEPT study_age
  group_by(study_no, reference, lat, long, 
           #study_age, #-want max study age across poolings
           data_type, reps, checkcrop,
           cropseq_simp, rotlength_simp, speciesno_simp, 
           cropseq_div, rotlength_div, speciesno_div, 
           sys_tillage, sys_weedmgmt, weedmsmt_unit, peren_divYN, fallow_simpYN, 
           PISD_diff, PICV_diff) %>%
  
  mutate(maxstudyage = max(study_age, na.rm = T)) %>%
  
  ungroup()
  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 4. Deal with different time reporting -----------------------------------
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# LAST and AVG data
#~~~~~~~~~~~~~~~~~~~~~~~~

# Keep portion of data set that is NOT repeated measures
# Non-RM data

d4a <- d3 %>% 
  filter(data_type != "RM") %>%
  mutate(data_type2 = data_type,
         study_age = as.character(study_age),  #--this will be changed to 'pooled', if applicable
         reps_pooledyrs = reps) #--no pooling so reps stay the same

# RM-data
#~~~~~~~~~~~~~~~~~~~~~

# Figure out if study_age is more than 2x rot length 

d4b <- d3 %>% 
  
  filter(data_type == "RM") %>%
  mutate(dothis = if_else(maxstudyage <= (rotlength_div * 2), 
                          "takelast",  #--if study is < 2 rotation lengths long, take the last value
                          "takeavg"))  #--otherwise, average over years > 2 rot lengths

# Break the dataset into two
# (1) study_age < rotlen*2 (we'll take last value)
# (2) where study_age > rotlen*2 (we'll avg)

# RM-last
#~~~~~~~~~~~~~~~~~~~~~~~~~

d4bLAST <- d4b %>% 
  
  filter(dothis == "takelast") %>%
  filter(study_age == maxstudyage) %>%
  
  mutate(data_type2 = "RM-last",
         study_age = as.character(study_age), #--necessary for later merging
         reps_pooledyrs = reps) %>% #--no increase in reps
  
  select(-dothis) 


# RM_avg
#~~~~~~~~~~~~~~~~~~~~~~

# Average
# NOTE: you wind up with some NaNs because if you take the mean of an empty vector
#   it gives you NaN

d4bAVG <- d4b %>% 
  
  filter(dothis == "takeavg") %>%
  
  # Keep only points taken after 2 complete rotations
  filter(study_age > rotlength_div*2)  %>%
  
  # group by same things we pooled over in step 1 + maxstudyage
  group_by(reference, lat, long, 
           #study_age, # only want to pool within years, not across years
           data_type, reps, checkcrop,
           cropseq_simp, rotlength_simp, speciesno_simp,
           cropseq_div, rotlength_div, speciesno_div, 
           sys_tillage, sys_weedmgmt, weedmsmt_unit, peren_divYN, fallow_simpYN, 
           PISD_diff, PICV_diff,
           maxstudyage) %>%
  
  summarise(bioLRR = mean(bioLRR, na.rm = T),
            denLRR = mean(denLRR, na.rm = T),
            reps_pooledyrs = sum(reps)) %>%  #--inc # of reps based on # of years
  
  ungroup() %>%
  
  mutate(study_age = "pooled",
         data_type2 = "RM-avg") 

# Bind datas back together
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

d4 <- bind_rows(d4a, d4bAVG, d4bLAST) %>%
  replace(is.na(.), NA) #--change NaNs to NAs

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 5. Modifier grouping  -----------------------------------
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Change sys_weedmgmt to binary (y/n)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Assign 2 categories - herb_Y and herb_N

d5 <- d4 %>%
  
  select(-newnobs, -nobs) %>%
  
  # Assign 2 categories - herb_Y and herb_N
  
  mutate(sys_weedmgmt2 = recode(sys_weedmgmt,
                                `herb/mech` =  "herb_Y",
                                herbicides =  "herb_Y",
                                mechanical = "herb_N",
                                none = "herb_N",
                                IC = "IC")) %>%
  
  # Make latitude-based classifications
  
  mutate(climclasslat2 = '999',
         climclasslat2 = if_else(lat > 35, "temperate", climclasslat2),
         climclasslat2 = if_else(lat <= 35, "sub/tropical", climclasslat2)) %>%
  
  mutate(mono_YN = ifelse(speciesno_simp == 1, "Y", "N")) %>%
  
#  Create non-parametric weighting, as described in Adams et al Ecology (1997)
#   (n1 x n2) / (n1 + n2)
#   use reps that are added if years are pooled
#   ctl and trt reps are the same in this case

  mutate(wgt = (reps_pooledyrs * reps_pooledyrs) / (reps_pooledyrs + reps_pooledyrs)) %>%
  
  # Calculate species ratios
  mutate(species_rat = speciesno_div / speciesno_simp)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 6. Ordering columns, write data to csv -----------------------------------
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~

d6a <- d5 %>%
  
  select(reference, lat, long, climclasslat2,
         reps, reps_pooledyrs, wgt, checkcrop, 
         cropseq_simp, rotlength_simp,
         cropseq_div,rotlength_div,
         data_type2, study_age, maxstudyage,  
         sys_tillage, sys_weedmgmt2, weedmsmt_unit, peren_divYN, fallow_simpYN, 
         PISD_diff, PICV_diff, species_rat,
         mono_YN, 
         bioLRR, denLRR) 

# Assign new study_id
#~~~~~~~~~~~~~~~~~~~~~
d6 <- d6a %>% 
  select(reference) %>% 
  unique() %>% arrange(reference) %>% mutate(study_id = 1:n()) %>% #--assign unique study_id
  right_join(d6a) %>%
  arrange(study_id) %>%
  select(study_id, everything()) 

write_csv(d6, "data-tidy-PIVI.csv")

