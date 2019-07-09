##############################
# Author: Gina Nichols (virginia.nichols@gmail.com)
# Code and data from publication 'Does diversifying crop rotations supress weeds? A meta-analysis.
# Date last modified: June 19 2019
##############################

##############
# Code
##############
XX_weeds_XX
Code is numbered in the order it should be run. 

00_weeds_functions
Contains helper functions, and is called when needed. 

01_weeds_process-raw-data-PIVI
Takes the data-raw-PIVI file and processes it into data-tidy-PIVI.

02_weeds_fit-mixed-mods
Uses the data-tidy-PIVI, fits statistical models and outputs results including
stats_lmer-mods, stats_lmer-mods-contrasts, and stats_lmer-resp

03_weeds_LOO-sensitivity-analysis
Uses data-tidy-PIVI, fits statistical models on dataset built removing one study at a time.
Produces stats_LOO-mods and stats_LOO-resp

04_weeds_biomass-vs-density
Uses data-raw-PIVI, finds studies that measured both biomass and density, creates data-bio-den-paired

05_weeds_forests
Uses data-tidy-PIVI and fits a generalized boosted regression model (Elia et al. 2008).
Produces stats_gbm

06_weeds_pub-trends
Uses data-tidy-PIVI, calculates overall effect for studies published up to a given year.
Produces stats_est-over-time

07_weeds_figures
Creates all figures, utilizes all three datasets.

08_weeds_S4-text-data
Creates supplementary file (a pdf) describing database building and processing.

08_weeds_S5-tables-stats
Creates supplementary file (a pdf) with summaries of all statistical procedures.

08_weeds_S6-fig-pubbias
Creates a supplementary file (a pdf) containing a figure demonstrating the trends in estimates over publication years.
##############
# Data
##############

data-bio-den-paired
Data from studies that report both weed biomass and density from the same experimental unit

data-dictionary-PIVI
A data dictionary describing the data-tidy-PIVI dataset

data-raw-PIVI
The raw data as extracted from publications

data-tidy-PIVI
The database used for actual analysis

stats_XX
Summaries of statistics, produced by R code