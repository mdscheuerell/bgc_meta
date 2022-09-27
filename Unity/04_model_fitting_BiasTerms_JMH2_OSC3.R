# JMH 25 Sept 22

# Libraries
library(MARSS)
library(doParallel)

# load MARSS model
mod_set_RW_b <- readRDS("fitted_seas_unique_states_RW_b.rds")

## bootstrap biased RW models by site
Sys.time()

bias_bootstrap <- mclapply(mod_set_RW_b, MARSSparamCIs, method = "parametric", nboot = 2, mc.cores = 6)

Sys.time()

# Save
saveRDS(bias_bootstrap, file = "mod_set_site_RW_b_BiasTerms.rds")


