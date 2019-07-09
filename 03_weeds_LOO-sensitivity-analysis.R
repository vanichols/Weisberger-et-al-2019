#########################
##
## Date of creation: May 28 2018
## Date last modified: Dec 12 2018 (updated to 99%CIs, cleaned up fucntions)
##                     June 19 2019 (cleaned and checkd for git publication)
##
## Author: Gina Nichols, virginia.nichols@gmail.com
##
## Purpose: Do sensitivity analyses on results - leave one study out
##          
## Inputs: data-tidy.csv, weeds_functions.R
##
## Outputs: stats_LOO-resp.csv, stats_LOO-mods.csv
##
## NOTES: 
##
#########################



# Clear env, load packages, set wd ----------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list=ls())
library(tidyverse)
library(lubridate)
library(lme4)
path <- rstudioapi::getActiveDocumentContext()$path
Encoding(path) <- "UTF-8"
setwd(dirname(path))

#~~~~~~~~~~~~~~~~~
# Get LOO funcitons I wrote -----------------------------------
#~~~~~~~~~~~~~~~~~
source("00_weeds_functions.R")


#~~~~~~~~~~~~~~~~~
# Read in data ------------------------------------------------------------
#~~~~~~~~~~~~~~~~~

# Subset by response, change to yi notation

dat <- read_csv("data-tidy.csv")


den <- dat %>% 
  select(-bioLRR) %>%
  filter(!is.na(denLRR)) %>%
  mutate(yi = denLRR)

bio <- dat %>% 
  select(-denLRR) %>%
  filter(!is.na(bioLRR)) %>%
  mutate(yi = bioLRR)


#~~~~~~~~~~~~~~~~~
#
# LOO sensitivity on responses ------------------------------------------------------------
#
#~~~~~~~~~~~~~~~~~


loo_den <- DoLOSORespFun(mydat = den, myresp = "den")
loo_bio <- DoLOSORespFun(mydat = bio, myresp = "bio")

loo_resp <- bind_rows(loo_den, loo_bio) %>%
  group_by(resp) %>%
  summarise(min99ci = min(ci_99low),
            max99ci = max(ci_99up),
            min99ci_per = exp(min99ci)*100 -100,
            max99ci_per = exp(max99ci)*100 -100)


write_csv(loo_resp, "stats_LOO-resp.csv")



#~~~~~~~~~~~~~~~~~
#
# LOO sensitivity on density modifiers ------------------------------------------------------------
#
#~~~~~~~~~~~~~~~~~


# 6 modifiers
# weedmsmt_unit, sys_tillage, climclasslat2, fallow_simpYN, peren_divYN, sys_weedmgmt
dothese <- c("weedmsmt_unit", "sys_tillage", "climclasslat2", "fallow_simpYN", "peren_divYN", "sys_weedmgmt2")

den_wu <- DoLOSOModFun(den, dothese[1], "den")
den_till <- DoLOSOModFun(den, dothese[2], "den")
den_clim <- DoLOSOModFun(den, dothese[3], "den")
den_fallow <- DoLOSOModFun(den, dothese[4], "den")
den_peren <- DoLOSOModFun(den, dothese[5], "den")
den_wm <- DoLOSOModFun(den, dothese[6], "den")

loo_den_mod <- bind_rows(den_wu, den_till, den_clim, den_fallow, 
                        den_peren, den_wm) %>%
  group_by(resp, mod, modlvl) %>%
  summarise(min99ci = min(ci_99low),
            max99ci = max(ci_99up),
            min99ci_per = exp(min99ci)*100 -100,
            max99ci_per = exp(max99ci)*100 -100)


#~~~~~~~~~~~~~~~~~
#
# LOO sensitivity on biomass modifiers ------------------------------------------------------------
#
#~~~~~~~~~~~~~~~~~

bio_wu <- DoLOSOModFun(bio, dothese[1], "bio")
bio_till <- DoLOSOModFun(bio, dothese[2], "bio")
bio_clim <- DoLOSOModFun(bio, dothese[3], "bio")

# NOTE: Biomass meausured in simple system fallow only occured in study_no 5
# so if we leave that out there are not 2 levels to test here
#bio_fallow <- DoLOSOModFun(bio, dothese[4])

# NOTE: Biomass meausured in peren_divYN == Y
# occured in 2 studies (6, 63)
bio_peren <- DoLOSOModFun(bio, dothese[5], "bio")
bio_wm <- DoLOSOModFun(bio, dothese[6], "bio")

loo_bio_mod <- bind_rows(bio_wu, bio_till, bio_clim, #bio_fallow, 
                        bio_peren, bio_wm) %>%
  group_by(resp, mod, modlvl) %>%
  summarise(min99ci = min(ci_99low),
            max99ci = max(ci_99up),
            min99ci_per = exp(min99ci)*100 -100,
            max99ci_per = exp(max99ci)*100 -100)


#~~~~~~~~~~~~~~~~~
#
# Write LOO-modifier results ------------------------------------------------------------
#
#~~~~~~~~~~~~~~~~~

loo_mod <- bind_rows(loo_den_mod, loo_bio_mod)

write_csv(loo_mod, "stats_LOO-mods.csv")

