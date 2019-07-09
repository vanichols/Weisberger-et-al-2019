#########################
##
## Date of creation: Dec 10 2018
##
## Last updated: Dec 26 2018
##               Feb 1 2019 (added color)
##               April 22 2019 (made Fig3 den vs bio, changed Fig2 to histogram)
##               April 30 2019 (add gbm res, change forest plot)
##               June 19 2019 (clean, change fig 3 based on reviewer comments)
##
## Author: Gina Nichols (virginia.nichols@gmail.com)
##
## Purpose: Create figures of results
##
## Inputs: data-tidy, data-raw, data-bio-den-paired
##
##
## Outputs: all figures
##
##
##
#########################


# Clear environment and load packages
#~~~~~~~~~~~~~~~~~~~~~~
rm(list = ls())
library(tidyverse)
library(lubridate)
library(maps) # pre-loaded maps package
library(ggridges) #--for graphing distributions
library(ggstance) #--for horizontal lineranges
library(broom) #--for cleaning stats
library(colortools)
library(viridis) #--has nice colors
library(here)

setwd(here())



# tidy dataset
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dat <- read_csv("data-tidy-PIVI.csv") %>%
  gather(bioLRR:denLRR, key = resp, value = LRR) %>%
  filter(!is.na(LRR)) %>%
  mutate(resp = recode(resp, 
                       bioLRR = "bio",
                       denLRR = "den"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Fig 1. World Map --------------------------------------------------------------------
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Get number of points per climclass
#~~~~~~~~~~~~~~~~~~~~~~~
f1ns <- dat %>%
  group_by(climclasslat2) %>%
  mutate(ncomp = n()) %>%
  select(study_id, climclasslat2, ncomp) %>%
  distinct() %>%
  group_by(climclasslat2) %>%
  summarise(nstud = n(),
            ncomp = mean(ncomp))
  

subnc <- unname(as.numeric(f1ns[1,3]))
tmptnc <- unname(as.numeric(f1ns[2,3]))

subns <- unname(as.numeric(f1ns[1,2]))
tmptns <- unname(as.numeric(f1ns[2,2]))

# Make tibble for figure, get world map
#~~~~~~~~~~~~~~~~~~~~~~~
f1dat <- dat %>%
  mutate(climclasslat2 = factor(climclasslat2, levels = c("temperate", "sub/tropical")),
         climclasslat2 = 
           recode(climclasslat2,
                  `sub/tropical` = 
                    (paste0('(Sub)Tropical (<35°); ',subnc, "(", subns, ")")),
                    temperate = 
                    (paste0('Temperate (>35°); ', tmptnc, "(", tmptns, ")"))))

map.w <- map_data('world')


# Make figure
#~~~~~~~~~~~~~~~~~~~~~~~

ggplot() +
  geom_polygon(data = map.w, aes(x = long, y = lat, group = group), 
               fill = "white", color = "gray60") +

  geom_jitter(data = f1dat, size = 2, width = 2, height = 2,
             aes(x = long, y = lat, pch = climclasslat2, fill = climclasslat2)) +
  
  theme_bw() +
  
  theme(legend.justification = c(0,0), 
        legend.position = c(0, 0),
        legend.box.margin = margin(c(5, 5, 5, 5)),
  
        legend.text = element_text(size = rel(0.8)),
        legend.title = element_text(size = rel(1), face = "bold"),
        legend.background = element_rect(linetype = 1, size = 0.5, colour = 1),
        
        axis.text = element_blank()) +
  
  labs(x = NULL, y = NULL) + 
  coord_fixed(1.3) +
  
  scale_fill_manual("Latitude Class", values = c("#0054EE", "#EE9A00")) +
  scale_shape_manual("Latitude Class", values = c(21, 24))

ggsave("Fig1.png", height = 4.2, width = 5.2, units = "in")
ggsave("../../../../Dropbox/ROTATION_META_DRAFT+SI/PLOS-ONE_revisions2/Fig1.tiff", 
       height = 4.2, width = 5.2, units = "in")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Fig 2. Data distributions --------------------------------------------------------------------
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Get statistical results
#~~~~~~~~~~~~~~~~~
f2stat <- read_csv("stats_lmer-resp.csv") %>%
  
   # Calculate % change
   mutate(pavg = exp(est) * 100 - 100,
          plow = exp(ci_99low) * 100 - 100,
          pup = exp(ci_99up) * 100 - 100) %>%
  rename(resp = mod) %>%
   select(-desc) #--get rid of intercept label 


# Calculate % change in raw data dist
#~~~~~~~~~~~~~~~~~~~~~
f2dat <- dat %>%
    select(resp, LRR) %>%
    # Change to %s
    mutate(pLRR = exp(LRR) * 100 - 100,
           resp_nice = recode(resp,
                            bio = "Weed Biomass",
                            den = "Weed Density"),
           resp_nice = factor(resp_nice, levels = rev(c("Weed Biomass", "Weed Density"))))

# Means, n, CIs
#~~~~~~~~~~~~~~~~~~~~~~~
f2ns <- dat %>%
   group_by(resp) %>%
   mutate(ntot = n()) %>%
  
  select(study_id, resp, ntot) %>%
  distinct() %>%
  group_by(resp) %>%
  summarise(nstuds = n(),
            ncomps = mean(ntot)) %>%
  
   left_join(f2stat) %>%
   mutate(resp_nice = recode(resp,
                             bio = "Weed Biomass",
                             den = "Weed Density"),
          resp_nice = factor(resp_nice, levels = rev(c("Weed Biomass", "Weed Density"))))


# Make Figure
#~~~~~~~~~~~~~~~~~~~~~~~~

# Historgram
#
ggplot(f2dat) + 
  geom_histogram(aes(pLRR), 
                 binwidth = 25, 
                 fill = "gray80", color = "black") +
  geom_vline(xintercept = 0, linetype = "solid", size = 1)  +
  
  # Mean value
  geom_text(data = f2ns, x = 900, y = 35, 
            aes(label = paste("Mean = ", round(pavg, 0), "%", sep = "")), 
            fontface = "italic", 
            vjust = -3, hjust = 1, 
            size = 5,
            color = "black")  +
  
  # Number of points
  geom_text(data = f2ns, x = 900, y = 35, aes(label = paste0(ncomps, "(", nstuds, ")")), 
            fontface = "italic", 
            vjust = -1.8, hjust = 1, 
            size = 4,
            color = "black")  +
  
  geom_rect(data = f2ns, ymin = -2.5, ymax = 47.5, aes(xmin = plow, xmax = pup), 
            fill = "red",
            alpha = 0.2) +
  geom_vline(data = f2ns, aes(xintercept = pavg), color = "red", linetype = "solid", size = 1) +
  
  facet_grid(~resp_nice) +
  labs(y = "Number of Observations", x = "Change From Simple Rotation [%]") + 
  
  # Get scales right
  #scale_x_continuous(breaks = c(-250, 0, 250, 500, 750, 1000)) +
  #coord_cartesian(ylim = c(0, 45)) +
  #expand_limits(x = c(-250, 1000)) +
  
  theme(
    panel.spacing = unit(0.2, "lines"),
    panel.background = element_rect(fill = NA),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    panel.grid.major.y = element_line( size = 0.1, color = "gray80", linetype = "dashed"),
    panel.grid.minor.y = element_line( size = 0.1, color = "gray80", linetype = "dashed"),
    
    strip.background = element_blank(),
    strip.text.x = element_text(size = rel(1.2), 
                                vjust = 2),
    strip.text.y = element_blank())



#ggsave("Fig2.png", width = 5.2, height = 5, units = "in") 

# in log scale
ggplot(f2dat) + 
  geom_histogram(aes(LRR), 
                 binwidth = 0.2, 
                 fill = "gray80", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "solid")  +
  
  # Mean value
  geom_text(data = f2ns, x = -4, y = 30, 
            aes(label = paste("Mean = ", round(pavg, 0), "%", sep = "")), 
            fontface = "italic", 
            vjust = 0, hjust = "left", 
            size = 5,
            color = "black")  +
  
  # Number of points
  geom_text(data = f2ns, x = -4, y = 28, 
            aes(label = paste0(ncomps, "(", nstuds, ")")), 
            fontface = "italic", 
            vjust = 0, hjust = "left", 
            size = 4,
            color = "black")  +
  
  geom_rect(data = f2ns, ymin = -2.5, ymax = 47.5, 
            aes(xmin = ci_99low, xmax = ci_99up), 
             fill = "red",
             alpha = 0.2) +
  geom_vline(data = f2ns, 
             aes(xintercept = est), 
             color = "red", linetype = "dotted", size = 1) +
   
  facet_grid(~resp_nice) +
  labs(y = "Number of Observations", x = "Natural Log of (Diverse/Simple) Weed Response") + 
  
  # Get scales right
  #scale_x_continuous(breaks = c(-250, 0, 250, 500, 750, 1000)) +
  #coord_cartesian(ylim = c(0, 45)) +
  #expand_limits(x = c(-250, 1000)) +
  
  theme(
    panel.spacing = unit(0.2, "lines"),
    panel.background = element_rect(fill = NA),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    panel.grid.major.y = element_line( size = 0.1, color = "gray80", linetype = "dashed"),
    panel.grid.minor.y = element_line( size = 0.1, color = "gray80", linetype = "dashed"),
    
    strip.background = element_blank(),
    strip.text.x = element_text(size = rel(1.2), 
                                vjust = 2),
    strip.text.y = element_blank())

ggsave("Fig2.png", width = 5.2, height = 5, units = "in") 
ggsave("../../../../Dropbox/ROTATION_META_DRAFT+SI/PLOS-ONE_revisions2/Fig2.tiff", 
       width = 5.2, height = 5, units = "in") 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Fig 3. - Weed biomass by weed density -----------------------------------------------
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

f3 <- read_csv("data-bio-den-paired.csv")

xlab = expression('Natural Log of Weed Density (plants m'^"-2"*')')
ylab = expression('Natural Log of Weed Biomass (g m'^"-2"*')')

f3 %>%
  ggplot(aes(lden, lbio)) + 
  geom_point(color = "gray50", size = 2, pch = 18) + 
  geom_smooth(method ="lm", se= F, color = "black") +
  #coord_cartesian(xlim = c(0,7), ylim = c(0, 7)) +
  labs(y = ylab,
       x = xlab) +
  theme_classic() + 
  theme(panel.border = element_rect(color = "black", fill = NA))

ggsave("Fig3.png", width = 5.2, height = 5.2, units = "in")
ggsave("../../../../Dropbox/ROTATION_META_DRAFT+SI/PLOS-ONE_revisions2/Fig3.tiff", 
       width = 5.2, height = 5.2, units = "in")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Fig 4 - Boosted Tree Results -----------------------------------------------
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

f4 <- read_csv("stats_gbm.csv")

f4 %>%
  # Make nice labels
  mutate(var_nice = recode(var, 
                           weedmsmt_unit = "Weed Unit",
                           checkcrop = "Check Crop",
                           maxstudyage = "Maximum Study Age",
                           sys_tillage = "Tillage System",
                           PISD_diff = "Increased Planting Interval Variance",
                           PICV_diff = "Increased Planting Interval Variance",
                           climclasslat2 = "Latitude Class",
                           sys_weedmgmt2 = "Herbicide Usage",
                           species_rat = "Increased Species Richness",
                           fallow_simpYN = "Fallow Usage",
                           peren_divYN = "Perennial Usage",
                           mono_YN = "Diversification From Monoculture")) %>%
  arrange(rel_inf) %>%
  mutate(order = 1:n()) %>%
  
  ggplot(aes(rel_inf, order)) + 
  
  geom_colh(color = "gray50", fill = "darkolivegreen3") + 
  geom_text(x = 0.5, aes(y = order, label = var_nice), 
            hjust = 0, vjust = 0.5, size = 4, fontface = "bold") + 
  
  theme_bw() +
  labs(
    x = "Scaled Importance",
    y = NULL
  ) + 
  #theme_pubr() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        
        panel.spacing = unit(0.2, "lines"),
        panel.background = element_rect(fill = NA),
        panel.border = element_rect(colour = "black", fill = NA, size = 1)) + 
  
  scale_y_continuous(breaks = seq(0, 9, 1), 
                     minor_breaks = seq(0, 9, 1))


ggsave("Fig4.png", width = 5.2, height = 5.2, units = "in")
ggsave("../../../../Dropbox/ROTATION_META_DRAFT+SI/PLOS-ONE_revisions2/Fig4.tiff", 
       width = 5.2, height = 5.2, units = "in")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Fig 5. Forest of modifiers --------------------------------------------------------------------
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

f5stat <- read_csv("stats_lmer-mods.csv") %>%
  mutate(mod_nice = recode(mod_nice,
                           Tillage = "Tillage System"))


# Extract info for labels (n-vals)
#~~~~~~~~~~~~~~~~~~~~

# Tillage
f5n_t <- dat %>%
  filter(sys_tillage != "IC") %>%
  group_by(resp, sys_tillage) %>%
  mutate(ntot = n()) %>%
  select(study_id, resp, sys_tillage, ntot) %>%
  distinct() %>%
  group_by(resp, sys_tillage) %>%
  summarise(nstuds = n(),
            ncomps = mean(ntot)) %>%
  rename(lvls = sys_tillage) %>%
  mutate(mod_nice = "Tillage System",
         mod_code = "sys_tillage")

# weed unit
f5n_wu <- dat %>%
  
  filter(weedmsmt_unit != "IC") %>%
  group_by(resp, weedmsmt_unit) %>%
  
  mutate(ntot = n()) %>%
  select(study_id, resp, weedmsmt_unit, ntot) %>%
  distinct() %>%
  
  group_by(resp, weedmsmt_unit) %>%
  summarise(nstuds = n(),
            ncomps = mean(ntot)) %>%
  
  rename(lvls = weedmsmt_unit) %>%
  mutate(lvls = recode(lvls, `single species` = "single"),
         mod_nice = "Weed Unit",
         mod_code = "weedmsmt_unit")

# Combine all labs
(f5labs <- bind_rows(f5n_t, f5n_wu))


# Make dataframe to use for fig -------------------------------------------

f5 <- f5labs %>%
   left_join(f5stat) %>%
   
   # Change to % control
   mutate(per = exp(est) * 100 - 100,
          plow = exp(ci_99low) * 100 - 100,
          pup = exp(ci_99up) * 100 - 100) %>%
   
   mutate(lvls = recode(lvls, NT = "Zero-Tillage",
                        tilled = "Tilled",
                       
                        community = "Community",
                        single = "Single"),
          
          mod_nice = recode(mod_nice,
                            `Weed Unit` = "Weed\nUnit",
                            `Tillage System` = "Tillage\nSystem"),
          
          mod_nice = factor(mod_nice, levels = c("Weed\nUnit",
                                                 "Tillage\nSystem"))) %>%

    mutate(resp_nice = resp,
         resp_nice = recode(resp_nice, bio = "Weed Biomass"),
         resp_nice = recode(resp_nice, den = "Weed Density"),
         resp_nice = factor(resp_nice, levels = (c("Weed Density", "Weed Biomass")))) %>%

   
  mutate(sigcolor = ifelse( (ci_99up > 0 & ci_99low < 0), "Not Significant", "Significant (p<0.01)"),
          siglvls = ifelse( ((lvls == "Community" | lvls == "Tilled") &
                                resp == "den"),
                            lvls, NA),
         labpos = ifelse( resp == "bio", -150, -110),
         sigcolor = factor(sigcolor, levels = c("Significant (p<0.01)","Not Significant"))) %>%
  
  mutate(lvlsn = recode(lvls, 
                        Yes = '2',
                        No = '1',
                        `Zero-Tillage` = '2',
                        Tilled = '1',
                        Single = '2',
                        Community = '1',
                        Tropical = '2',
                        Temperate = '1'),
         lvlsn = as.numeric(lvlsn))


# Figure
#~~~~~~~~~~~~~~~~~~~~~~~~~~

f5 %>%
  filter(mod_code %in% c("sys_tillage", "weedmsmt_unit")) %>%
  ggplot(aes(per, lvls)) + 
  
  geom_linerangeh(aes(xmin = plow, xmax = pup, color = sigcolor), 
                  alpha = 0,size = 1.5) +
  geom_rect(aes(xmin = plow, xmax = pup, 
                ymin = lvlsn - 0.2, ymax = lvlsn + 0.2,
                fill = sigcolor),
            color = "gray90", 
            alpha = 0.5, size = 0.5) +
  # 
  # 
  # 
  # geom_rect(aes(xmin = plow, xmax = pup,
  #               ymin = lvlsn - 0.2, ymax = lvlsn + 0.2, 
  #               fill = sigcolor),
  #               color = "gray90",
  #           alpha = 0.2, size = 0.5) +
  geom_point(color = "black",
    aes(size = -per)) +

    # n-values
  geom_text(aes(x = labpos, y = lvls, label = paste0(round(ncomps,0), "(", nstuds, ")")), 
            fontface = "italic", 
            color = "gray50", 
            hjust = 0, 
            size = 2.5) + 
  
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.2) + 
  
  scale_color_manual(values = c("Not Significant" = "gray90", 
                                 "Significant (p<0.01)" = "red")) +
  scale_fill_manual(values = c("Not Significant" = "gray90", 
                                "Significant (p<0.01)" = "red")) +
  guides(color = F, size = F) +
  
  labs(y = NULL, x = "Change From Simple Rotation [%]", fill = NULL) + 
  facet_grid(mod_nice ~ resp_nice, scales = "free") +
  
  theme_classic() + 
  theme(panel.border = element_rect(color = "black", fill = NA),
        strip.text.y = element_text(angle = 720),
        legend.position = "bottom",
        legend.direction = "horizontal")


ggsave("Fig5.png", width = 7.5, height = 5) 
ggsave("../../../../Dropbox/ROTATION_META_DRAFT+SI/PLOS-ONE_revisions2/Fig5.tiff", 
       width = 7.5, height = 5, units = "in")
