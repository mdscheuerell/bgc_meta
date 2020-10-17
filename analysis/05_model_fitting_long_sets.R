## This script loads the munged `solutes` data frame and fits MARSS models

## load libraries
library(dplyr)
## library(tidyr)
library(MARSS)

## load solutes data
df <- readr::read_csv(file.path(here::here("data"), "tbl_solutes_unmanaged_mon.csv"))

## sites and years to include
sites_yrs <- data.frame(site = c("DOR", "ELA", "HJA", "HBEF", "MEF", "TLW"),
                        year_first = c(1984, 1971, 1970, 1995, 1986, 1982),
                        year_last = c(2014, 2013, 2018, 2018, 2016, 2010))

## names of solutes to analyze
solutes <- c("FWACamgL", "FWATDPmgL")

## models to fit
models <- c("catch", "catch + bias", "site", "site + bias")

n_mods <- length(solutes) * length(sites_yrs$site) * length(models)
  
## df for results
tbl_mod_sel <- data.frame(solute = rep(c("Ca", "TDP"), ea = length(sites_yrs$site) * length(models)),
                          site = rep(rep(sites_yrs$site, ea = length(models)), length(solutes)),
                          model = rep(models, length(solutes) * length(sites_yrs$site)),
                          AIC = rep(NA, n_mods))


## Setup

## 1) keep only sites with multiple catchments
df <- df %>%
  filter(site %in% sites_yrs$site)

## 2) keep only solutes of interest
df <- df %>%
  select(site:dec_water_yr, all_of(solutes))

## 3) convert 0 to NA following mtg and emails (ie, "no sample")
df <- df %>%
   purrr::modify_at(-c(1:3), ~na_if(., 0))
  

## loop over sites
for(i in 1:length(sites_yrs$site)) {
  
  ## select site and years specific to it
  dat_sol <- df %>%
    filter(site == sites_yrs$site[i]) %>%
    filter(dec_water_yr >= sites_yrs$year_first[i] & dec_water_yr <= sites_yrs$year_last[i])
  
  ## dummy covariates for season
  n_months <- dat_sol$dec_water_yr %>%
    unique() %>%
    length()
  seas_1 <- sin(2 * pi * seq(n_months) /12)
  seas_2 <- cos(2 * pi * seq(n_months) /12)
  
  ## loop over solutes
  for(j in solutes) {
    
    ## select solute and pivot wider
    dat_to_fit <- dat_sol %>%
      select(site:dec_water_yr, all_of(j)) %>%
      tidyr::pivot_wider(names_from = c(site, catchment),
                         values_from = all_of(j)) %>%
      select(-dec_water_yr) %>%
      log() %>%
      scale(scale = FALSE) %>%
      t()
    
    ## SET 1: all catchments are unique processes
    ## (a) RW without bias
    ## model setup
    mod_list <- list(
      B = "identity",
      U = "zero",
      C = "unconstrained",
      c = rbind(seas_1, seas_2),
      Q = "diagonal and unequal",
      Z = "identity",
      A = "zero",
      R = "diagonal and equal"
    )
    ## fit model
    tmp <- MARSS(dat_to_fit, model = mod_list,
                 control = list(maxit = 5000), method = "BFGS")
    
    ## assign AIC to results table
    tbl_mod_sel <- tbl_mod_sel %>%
      mutate(AIC = replace(AIC, which(solute == sub("(FWA)(.{2,3})(mgL)", "\\2", j)
                                      & site == sites_yrs$site[i]
                                      & model == "catch"),
                           tmp$AIC))
    
    ## (b) RW with bias
    mod_list$U = "unconstrained"
    ## fit model
    tmp <- MARSS(dat_to_fit, model = mod_list,
                 control = list(maxit = 5000), method = "BFGS")
    
    ## assign AIC to results table
    tbl_mod_sel <- tbl_mod_sel %>%
      mutate(AIC = replace(AIC, which(solute == sub("(FWA)(.{2,3})(mgL)", "\\2", j)
                                      & site == sites_yrs$site[i]
                                      & model == "catch + bias"),
                           tmp$AIC))
    
    ## SET 2: all catchments share a process within a site
    ## (a) RW without bias
    ## model setup
    mod_list <- list(
      B = "identity",
      U = "zero",
      C = "unconstrained",
      c = rbind(seas_1, seas_2),
      Q = "diagonal and unequal",
      Z = matrix(1, nrow = nrow(dat_to_fit), ncol = 1),
      A = "zero",
      R = "diagonal and equal"
    )
    ## fit model
    tmp <- MARSS(dat_to_fit, model = mod_list,
                 control = list(maxit = 5000), method = "BFGS")
    
    ## assign AIC to results table
    tbl_mod_sel <- tbl_mod_sel %>%
      mutate(AIC = replace(AIC, which(solute == sub("(FWA)(.{2,3})(mgL)", "\\2", j)
                                      & site == sites_yrs$site[i]
                                      & model == "site"),
                           tmp$AIC))
    
    # ## (b) RW with bias
    ## U for biased RW's
    mod_list$U <- matrix("bias")
    ## fit model
    tmp <- MARSS(dat_to_fit, model = mod_list,
                 control = list(maxit = 5000), method = "BFGS")
    
    ## assign AIC to results table
    tbl_mod_sel <- tbl_mod_sel %>%
      mutate(AIC = replace(AIC, which(solute == sub("(FWA)(.{2,3})(mgL)", "\\2", j)
                                      & site == sites_yrs$site[i]
                                      & model == "site + bias"),
                           tmp$AIC))
    
  } ## end loop over solutes
  
} ## end loop over sites


tbl_mod_sel %>%
  group_by(solute, site) %>%
  mutate(delta_AIC = AIC - min(AIC)) %>%
  mutate_if(is.double, round, 1) %>%
  select(-AIC) %>%
  as.data.frame()


