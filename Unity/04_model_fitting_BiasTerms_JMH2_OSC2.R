# JMH 25 Sept 22

# Libraries
library(MARSS)
library(doParallel)

# load MARSS model
mod_set_RW_b <- readRDS("fitted_seas_unique_states_RW_b.rds")

## bootstrap biased RW models by site
Sys.time()

no_cores <- detectCores() - 1; no_cores
cl <- makeCluster(no_cores, type = "FORK"); cl
registerDoParallel(cl)

bias_bootstrap <- foreach(i = mod_set_RW_b) %dopar% MARSSparamCIs(i, method = "parametric", nboot = 2)

Sys.time()

# Save
saveRDS(bias_bootstrap, file = "mod_set_site_RW_b_BiasTerms.rds")


