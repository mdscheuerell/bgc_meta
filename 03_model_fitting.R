## This script loads the munged `solutes` data frame and fits MARSS models

## load libraries
library(dplyr)
library(tidyr)
library(MARSS)

## load solutes data
df <- readr::read_csv(file.path(here::here("data"), "tbl_solutes_unmanaged_mon.csv"))

## remove SLP
df <- df %>%
  filter(site != "SLP")

## names of solutes
solutes <- df %>%
  # select(starts_with("FWA")) %>%
  select(starts_with("Str")) %>%
  colnames

## first and last water years
yr_first <- min(df$dec_water_yr)
yr_last <- floor(max(df$dec_water_yr))

# ## sequence of decimal water years
# tvec <- seq(min(df$dec_water_yr), max(df$dec_water_yr),
#             length.out = 12 * (yr_last - yr_first + 1))

## empty lists for model fits
mod_list_RW <- vector("list", length(solutes))
mod_list_RW_b <- vector("list", length(solutes))

## loop over solutes
for(i in 1:length(solutes)) {
  
  ## select solute and pivot wider
  dat_sol <- df %>%
    select(region:dec_water_yr, all_of(solutes[i])) %>%
    pivot_wider(names_from = c(region, site, catchment),
                values_from = solutes[i]) %>%
    select(-dec_water_yr) %>%
    log() %>%
    scale(scale = FALSE) %>%
    t()
  
  ## SET 1: all catchments are unique processes
  ## (a) RW without bias
  mod_list <- list(
    B = "identity",
    U = "zero",
    Q = "diagonal and unequal",
    Z = "identity",
    A = "zero",
    R = "diagonal and equal"
  )
  ## fit model
  mod_list_RW[[i]] <- MARSS(dat_sol, mod_list, control = list(maxit = 2000))
  
  ## (b) RW with bias
  mod_list$U = "unconstrained"
  ## fit model
  mod_list_RW_b[[i]] <- MARSS(dat_sol, mod_list, control = list(maxit = 2000))
  
  
}





