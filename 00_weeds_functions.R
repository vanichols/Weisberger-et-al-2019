#########################
##
## Contains the following functions:
##
##  RunModelNoModsFun  #--fits lmer to overall response (no fixed effect), gives est, se, 99% cis
##  RunModelModsFun    #--same as above, but w/fixed effect of modifier
##  RUnModelModsContrastFun #--contrasts levels of modifiers
##
##  DoLOSORespFun  #--Does a leave-one-out analysis on a response
##  BangLOSOModFun #--Helper function for DoLOSOModFun
##  DoLOSOModFun #--Does a leave-one-out analysis on a response and given modifier
##
#########################

library(lme4)
library(lmerTest)
library(emmeans)
library(broom)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Run lmer model w/o modifier ---------------------------------------------
# Spits out est, ci_99s, and p_vals
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RunModelNoModsFun <- function(mydata, resp) {
  
  ## For troubel shooting
  #mydata <- den
  #resp = "den"
  
  F.res <- lmer(yi ~ 1 + (1|study_id), 
                data = mydata, 
                weights = wgt) 
  
  # Extract lmer results using contest from lmerTest package
  F.tidy <- contest(F.res, L = 1, joint = F, level = 0.99) 
  
  # Fix names
  names(F.tidy) <- c("est", "se", "df", "t", "ci_99low", "ci_99up", "p_val")
  
  F.tidy <- F.tidy %>%
    mutate(mod = resp,
           desc = "intercept") %>%
    select(mod, desc, everything())
  
  return(F.tidy)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Run lmer model w/modifier ---------------------------------------------
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RunModelModsFun <- function(mydata, mymod, myresp) {
  
  
  ##### For trouble shooting, comment out when you actually run it######
  #~~~~~~~~~~~~~~~~~
  #mydata <- den
  #mymod = "sys_tillage"
  #myresp = "den"
  
  d.tmp <- mydata %>% 
    select(study_id, yi, wgt, mymod) %>%
    rename_at(4, ~"mod") %>%         # change column name to generalizable 'mymod'
    filter(mod != "IC") %>%          # remove 'IC' (it's complicated) values from analysis
    mutate(mod= as.factor(mod)) 
  
  # Fit model to feed to emmeans
  F.res <- lmer(yi ~ mod  + (1|study_id), 
                data = d.tmp, 
                weights = wgt) 
  
  # Use emmeans on lmer output
  F.lsm <- (emmeans(F.res, spec = "mod"))
  
  # Get CIs and pvals
  F.cilow <- confint(F.lsm, adjust = "none", level = 0.99)$lower.CL
  F.ciup <- confint(F.lsm, adjust = "none", level = 0.99)$upper.CL
  F.ps <- test(F.lsm, level = 0.99)$p.value
  
  # Combine emmeans' estimates, pvals, and 99% cis
  F.sum <- tidy(F.lsm) %>%
    select(-conf.low, -conf.high) %>%
    mutate(p_val = F.ps,
           ci_99low = F.cilow,
           ci_99up = F.ciup, 
           resp = myresp,
           mod_code = mymod) %>%
    rename(est = estimate,
           se = std.error,
           desc = mod)
  
  return(F.sum)
  
  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Run lmer model w/modifier, compare levels ---------------------------------------------
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RunModelModsContrastFun <- function(mydata, mymod, myresp) {
  
  
  ##### For trouble shooting, comment out when you actually run it######
  #~~~~~~~~~~~~~~~~~
  #mydata <- den
  #mymod = "sys_tillage"
  #myresp = "den"
  
  d.tmp <- mydata %>% 
    select(study_id, yi, wgt, mymod) %>%
    rename_at(4, ~"mod") %>%         # change column name to generalizable 'mymod'
    filter(mod != "IC") %>%          # remove 'IC' (it's complicated) values from analysis
    mutate(mod = as.factor(mod)) 
  
  # Fit model to feed to emmeans
  F.res <- lmer(yi ~ mod  + (1|study_id), 
                data = d.tmp, 
                weights = wgt) 
  
  # Use emmeans on lmer output
  F.lsm <- (emmeans(F.res, spec = "mod"))
  
  # Get comparisons of each level
  F.prs <- pairs(F.lsm)
  
  # Combine emmeans' estimates, pvals, and 99% cis
  F.sum <- tidy(F.prs) %>%
    mutate(resp = myresp,
           mod_code = mymod) %>%
    rename(est = estimate,
           se = std.error)
  
  return(F.sum)
  
  
  
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Do a leave-one-out analysis on overall response ---------------------------------------------
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

DoLOSORespFun <- function(mydat, myresp) {
  
  ##### For trouble shooting, comment out when you actually run it######
  #~~~~~~~~~~~~~~~~~
  #mydat <- den
  #myresp = "den"
  
  # Use previously made function
  notmestudy_id <- 0
  
  msum_base <- RunModelNoModsFun(mydat, myresp) %>%
    rename(resp = mod) %>%
    select(-desc) %>%
    
    mutate(nu_st = length(unique(mydat$study_id)),
           lo = notmestudy_id) %>%                   # number of study that was left out
    select(nu_st, lo, resp, everything())
  
  # Loop through, leaving one study out each time
  #~~~~~~~~~~~~~~~~~
  
  for(i in unique(mydat$study_id)){
    
    ######For trouble shooting, comment out######
    #~~~~~~~~~~~~~~~~~
    #notmestudy_id <- 1
    
    # Identify study, filter it out
    #~~~~~~~~~~~~~~~~~
    notmestudy_id <- i
    
    d.tmp <- mydat %>% filter(study_id != notmestudy_id)
    
    # Do same process as above w/filtered data
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    mres.tmp <- RunModelNoModsFun(d.tmp, myresp) %>%
      rename(resp = mod) %>%
      select(-desc) %>%
      
      mutate(nu_st = length(unique(mydat$study_id)),
             lo = notmestudy_id) %>%                   # number of study that was left out
      select(nu_st, lo, resp, everything())
    
    
    msum_base <- as.tibble(bind_rows(msum_base, mres.tmp))
    
    print(notmestudy_id)
  }
  
  return(msum_base)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# LOSO w/modifiers --------------------------------------------------------
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Given a dataset, a response, a modifier, and study to leave out, 
#  fit lmer model, spit out est and CIS
#~~~~~~~~~~~~~~~~~

BangLOSOModFun <- function(mydat, mymod, myresp, lout) {
  
  ##### For trouble shooting, comment out when you actually run it######
  #~~~~~~~~~~~~~~~~~
  #mydat <- den
  #mymod = "sys_tillage"
  #myresp = "den"
  #lout <- 0  #--study id to leave out 
  
  d.tmp <- mydat %>%
    filter(study_id != lout)
  
  # Use previously built function RunModelModsFun
  msum_base <- RunModelModsFun(d.tmp, mymod, myresp) %>%
    rename(modlvl = desc,
           mod = mod_code) %>%
    mutate(lo = lout) %>%   #--study_id that was left out
    select(resp, lo, mod, modlvl, everything())
  
  return(msum_base)
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Do a leave-one-out analysis on response/modifier combo ---------------------------------------------
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

DoLOSOModFun <- function(mydat, mymod, myresp) {
  
  ##### For trouble shooting, comment out when you actually run it######
  #~~~~~~~~~~~~~~~~~
  #mydat <- den
  #mymod = "sys_tillage"
  #myresp = "den"
  
  
  # Run lmer (using my function) to get estimates of means for each mod level
  #~~~~~~~~~~~~~~~~~
  msum_base <- BangLOSOModFun(mydat, mymod, myresp, 0)
  
  
  # Loop through, leaving one study out each time
  #~~~~~~~~~~~~~~~~~
  
  for (i in unique(mydat$study_id)) {
    
    # Identify study, to be left out
    #~~~~~~~~~~~~~~~~~
    mylout <- i
    msum_tmp <- BangLOSOModFun(mydat, mymod, myresp, mylout)
    
    
    msum_base <- bind_rows(msum_base, msum_tmp)
    
    print(i)
    
  }
  
  return(msum_base)
}

