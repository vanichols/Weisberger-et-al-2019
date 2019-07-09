#########################
##
## Date of creation: Dec 10 2018
##
## Last updated: June 19 2019 (check and clean for git publication)
##               
##
## Author: Gina Nichols (virginia.nichols@gmail.com)
##
## Purpose: Look at relationship between weed biomass and weed density, for studies that report both
##
## Inputs: data-raw-PIVI
##
## Outputs: data-bio-den-paired.csv
##
#########################

# Clear env, load packages, set wd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list = ls())
library(tidyverse)



#~~~~~~~~~~~~~~~~~
# Read in data ------------------------------------------------------------
#~~~~~~~~~~~~~~~~~

raw <- read_csv("data-raw-PIVI.csv") %>%
  mutate(obs_no = row_number()) %>%
  select(obs_no, everything())

# We have 74 comparisons that have both biomass and density
raw %>%
  #filter( (!is.na(bio_simp) & !is.na(den_simp)) ) %>%
  filter( !is.na(bio_div) & !is.na(den_div) ) %>%
  select(study_no) %>%
  unique(.)

#~~~~~~~~~~~~~~~~~
# WRangle data ------------------------------------------------------------
#~~~~~~~~~~~~~~~~~

# Get data into long form

simp <- raw %>%
  select(reference, bio_simp, den_simp) %>%
  rename(bio = bio_simp,
         den = den_simp) %>%
  mutate(systype = "simp") %>%
  unique(.) %>% #--get rid of repeats, simple systems were compared to more than 1 diverse system
  filter( (!is.na(bio) & !is.na(den)) )


div <- raw %>%
  select(reference, bio_div, den_div) %>%
  rename(bio = bio_div,
         den = den_div) %>%
  mutate(systype = "div")  %>%
  unique(.) %>% #--just in case
  filter( (!is.na(bio) & !is.na(den)) )


raw_lng <- bind_rows(simp, div) %>%
  rename(bio_gm2 = bio,
         den_plm2 = den) %>%
  mutate(lbio = log(bio_gm2),
         lden = log(den_plm2)) %>%
  filter(!is.infinite(lden), !is.infinite(lbio))
  



#~~~~~~~~~~~~~~~~~
# Get correlations and write data ------------------------------------------------------------
#~~~~~~~~~~~~~~~~~

write_csv(raw_lng, "data-bio-den-paired.csv")

(cor(raw_lng$bio_gm2, raw_lng$den_plm2))
(cor(raw_lng$lbio, raw_lng$lden))
(cor(raw_lng$lbio, raw_lng$lden, method = "spearman"))
cor.test(raw_lng$lbio, raw_lng$lden, method = "spearman")

