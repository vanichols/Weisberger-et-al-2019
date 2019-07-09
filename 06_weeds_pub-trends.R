#########################
#
# Date of creation: May 15 2019
# Date last modified: May 15 2019
#                     June 19 2019 (clean and check for git publication)
#
# Author: Gina Nichols (virginia.nichols@gmail.com)
#
# Purpose: Fit mixed linear models to all response variables and all modifiers
#            as a cumulative year of pub
#          
# Inputs: tidy-data-PIVI
#
# Outputs: stats_est-over-time.csv
#
# NOTE: See what estimates look like over years
#
#########################

# Clear env, load packages, set wd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list = ls())
library(tidyverse)

source("00_weeds_functions.R")


#~~~~~~~~~~~~~~~~~~~~~~~~
#
# Read in data, subset into bio and den -----------------------------------
#
#~~~~~~~~~~~~~~~~~~~~~~~~~

dat <- read_csv("data-tidy-PIVI.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~
#
# Subset into den/bio, loop through adding next year's studies -----------------------------------
#
#~~~~~~~~~~~~~~~~~~~~~~~~~


# Density -----------------------------------------------------------------

den <- dat %>% 
  select(-bioLRR) %>%
  filter(!is.na(denLRR)) %>%
  mutate(yi = denLRR) %>%
  mutate(year = str_sub(reference, -4, -1)) %>%
  select(year, everything()) %>%
  mutate(year = as.numeric(year)) %>%
  arrange(year) %>%
  mutate(year_id = group_indices(., year)) %>%
  select(year_id, everything())


denres <- tibble(year = NA,
                 mod = NA,
                 est = NA,
                 se = NA,
                 ci_99low = NA,
                 ci_99up = NA,
                 p_val = NA)

for (i in 2:max(den$year_id)) {

  dome <- den %>%
    filter(year_id %in% c(1:i))
  
  year.tmp <- max(dome$year)
  
  res.tmp <- RunModelNoModsFun(dome, "den") %>%
    select(mod, est, se, ci_99low, ci_99up, p_val) %>%
    mutate(year = year.tmp)
  
  denres <- bind_rows(denres, res.tmp)
  
  
}

denres2 <- denres %>%
  filter(!is.na(year),
         year !=1992) 

denres2 %>%
  ggplot(aes(year, est)) + 
  geom_pointrange(aes(ymin = ci_99low, ymax = ci_99up))


# biomass -----------------------------------------------------------------


bio <- dat %>% 
  select(-denLRR) %>%
  filter(!is.na(bioLRR)) %>%
  mutate(yi = bioLRR) %>%
  mutate(year = str_sub(reference, -4, -1)) %>%
  select(year, everything()) %>%
  mutate(year = as.numeric(year)) %>%
  arrange(year) %>%
  mutate(year_id = group_indices(., year)) %>%
  select(year_id, everything())


biores <- tibble(year = NA,
                 mod = NA,
                 est = NA,
                 se = NA,
                 ci_99low = NA,
                 ci_99up = NA,
                 p_val = NA)

for (i in 1:max(bio$year_id)) {
  
  dome <- bio %>%
    filter(year_id %in% c(1:i))
  
  year.tmp <- max(dome$year)
  
  res.tmp <- RunModelNoModsFun(dome, "bio") %>%
    select(mod, est, se, ci_99low, ci_99up, p_val) %>%
    mutate(year = year.tmp)
  
  biores <- bind_rows(biores, res.tmp)
  
  
}

biores2 <- biores %>%
  filter(!is.na(year),
         year > 1997)

biores2 %>%
  ggplot(aes(year, est)) + 
  geom_pointrange(aes(ymin = ci_99low, ymax = ci_99up))

# write that shit ---------------------------------------------------------

myres <- bind_rows(denres2, biores2) 

myres %>%
  write_csv("stats_est-over-time.csv")

