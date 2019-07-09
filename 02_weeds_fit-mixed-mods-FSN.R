#########################
#
# Date of creation: Dec 10 2018
# Date last modified: June 19 2019 (clean and check for git publication)
#
# Author: Gina Nichols (virginia.nichols@gmail.com)
#
# Purpose: Fit mixed linear models to all response variables and all modifiers
#          Get fail-safe number to assess publication bias
#          
# Inputs: 00_weeds_functions, tidy-data-PIVI
#
# Outputs: stats_lmer-resp.csv, stats_lmer-mods.csv, stats_lmer-mods-contrasts.csv
#
# NOTE: Using lme4 and lsmeans contrasts
#
#########################

# Clear env, load packages, set wd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list = ls())
library(tidyverse)
library(metafor) #--for calculating fail-safe-number



# load my functions
#~~~~~~~~~~~~~~~~~~~~~
source("00_weeds_functions.R")


#~~~~~~~~~~~~~~~~~~~~~~~~
#
# Read in data, subset into bio and den -----------------------------------
#
#~~~~~~~~~~~~~~~~~~~~~~~~~

dat <- read_csv("data-tidy-PIVI.csv")

den <- dat %>% 
  select(-bioLRR) %>%
  filter(!is.na(denLRR)) %>%
  mutate(yi = denLRR)

bio <- dat %>% 
  select(-denLRR) %>%
  filter(!is.na(bioLRR)) %>%
  mutate(yi = bioLRR)


#~~~~~~~~~~~~~~~~~~~~~~~~
#
# Overall responses -----------------------------------
#
#~~~~~~~~~~~~~~~~~~~~~~~~~

denres <- RunModelNoModsFun(den, "den")
biores <- RunModelNoModsFun(bio, "bio")

res <- bind_rows(denres, biores)

write_csv(res, "stats_lmer-resp.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~
#
# Fail-safe-number analysis -----------------------------------
#
#~~~~~~~~~~~~~~~~~~~~~~~~~

den_fsn <- den %>%
  # Assign variances as 1/wgts
  mutate(vi = 1/wgt)

bio_fsn <- bio %>%
  mutate(vi = 1/wgt)

fsn(yi, vi, data=den_fsn)
fsn(yi, vi, data=bio_fsn)

#~~~~~~~~~~~~~~~~~~~~~~~~
#
# Modifiers -----------------------------------
#
#~~~~~~~~~~~~~~~~~~~~~~~~~

dothese <- c("weedmsmt_unit", "sys_tillage", "climclasslat2", 
             "fallow_simpYN", "peren_divYN", "sys_weedmgmt2")

# Density
#~~~~~~~~~~~~
den_wu <- RunModelModsFun(den, dothese[1], "den")
den_till <- RunModelModsFun(den, dothese[2], "den")
den_clim <- RunModelModsFun(den, dothese[3], "den")
den_fall <- RunModelModsFun(den, dothese[4], "den")
den_peren <- RunModelModsFun(den, dothese[5], "den")
den_wm <- RunModelModsFun(den, dothese[6], "den")

den_mods <- bind_rows(den_wu, den_till, den_clim, den_fall, den_peren, den_wm) %>%
  select(resp, mod_code, desc, everything()) 

# Biomass
#~~~~~~~~~~~~
bio_wu <- RunModelModsFun(bio, dothese[1], "bio")
bio_till <- RunModelModsFun(bio, dothese[2], "bio")
bio_clim <- RunModelModsFun(bio, dothese[3], "bio")
bio_fall <- RunModelModsFun(bio, dothese[4], "bio")
bio_peren <- RunModelModsFun(bio, dothese[5], "bio")
bio_wm <- RunModelModsFun(bio, dothese[6], "bio")


bio_mods <- bind_rows(bio_wu, bio_till, bio_clim, bio_fall, bio_peren, bio_wm) %>%
  select(resp, mod_code, desc, everything()) 

# Put together and put in format for forest plot merging
res_mods <- bind_rows(den_mods, bio_mods) %>%
  mutate(mod_nice = recode(mod_code,
                           climclasslat2 = "Latitude",
                           fallow_simpYN = "Simple System Fallow",
                           sys_weedmgmt2 = "Weed Control",
                           peren_divYN = "Perennial Inclusion",
                           sys_tillage = "Tillage",
                           weedmsmt_unit = "Weed Unit")) %>%
  mutate(lvls = desc,
         
         # Make it match forest plot stuff, no idea why I did this
         lvls = ifelse(mod_code == "fallow_simpYN" & desc == "N", "fallow_N", lvls),
         lvls = ifelse(mod_code == "fallow_simpYN" & desc == "Y", "fallow_Y", lvls),
         
         lvls = ifelse(mod_code == "peren_divYN" & desc == "N", "peren_N", lvls),
         lvls = ifelse(mod_code == "peren_divYN" & desc == "Y", "peren_Y", lvls),
         
         lvls = recode(lvls, `single species` = "single")) %>%
  
  select(resp, mod_code, mod_nice, lvls, desc, everything())

write_csv(res_mods, "stats_lmer-mods.csv")



#~~~~~~~~~~~~~~~~~~~~~~~~
#
# Contrasts -----------------------------------
#
#~~~~~~~~~~~~~~~~~~~~~~~~~


# Density
#~~~~~~~~~~~~
denc_wu <- RunModelModsContrastFun(den, dothese[1], "den")
denc_till <- RunModelModsContrastFun(den, dothese[2], "den")
denc_clim <- RunModelModsContrastFun(den, dothese[3], "den")
denc_fall <- RunModelModsContrastFun(den, dothese[4], "den")
denc_peren <- RunModelModsContrastFun(den, dothese[5], "den")
denc_wm <- RunModelModsContrastFun(den, dothese[6], "den")

denc_mods <- bind_rows(denc_wu, denc_till, denc_clim, denc_fall, denc_peren, denc_wm) %>%
  select(resp, mod_code, everything()) 

# Biomass
#~~~~~~~~~~~~
bioc_wu <- RunModelModsContrastFun(bio, dothese[1], "bio")
bioc_till <- RunModelModsContrastFun(bio, dothese[2], "bio")
bioc_clim <- RunModelModsContrastFun(bio, dothese[3], "bio")
bioc_fall <- RunModelModsContrastFun(bio, dothese[4], "bio")
bioc_peren <- RunModelModsContrastFun(bio, dothese[5], "bio")
bioc_wm <- RunModelModsContrastFun(bio, dothese[6], "bio")

bioc_mods <- bind_rows(bioc_wu, bioc_till, bioc_clim, bioc_fall, bioc_peren, bioc_wm) %>%
  select(resp, mod_code, everything()) 

# Put together and put in format for forest plot merging
resc_mods <- bind_rows(denc_mods, bioc_mods) %>%
  mutate(mod_nice = recode(mod_code,
                           climclasslat2 = "Latitude",
                           fallow_simpYN = "Simple System Fallow",
                           sys_weedmgmt2 = "Weed Control",
                           peren_divYN = "Perennial Inclusion",
                           sys_tillage = "Tillage",
                           weedmsmt_unit = "Weed Unit")) %>%
  
  select(resp, mod_code, mod_nice, everything())

write_csv(resc_mods, "stats_lmer-mods-contrasts.csv")


