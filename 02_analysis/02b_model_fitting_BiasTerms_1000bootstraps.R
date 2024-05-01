# JMH Sept 16 22
# started 25 Sept. (not date last run started)
# finished 19 October

library(MARSS)
library(doParallel)

# load MARSS model
mod_set_RW_b <- readRDS("06_ModelRDS/fitted_seas_unique_states_RW_b.rds")



## bootstrap biased RW models by site
# This takes 2-3 weeks on modern desktop computer

startTime <-Sys.time()
bias_bootstrap <- mclapply(mod_set_RW_b, MARSSparamCIs, method = "parametric", nboot = 1000, mc.cores = 6)
EndTime <- Sys.time()
EndTime - startTime


# Save
saveRDS(bias_bootstrap, file = "06_ModelRDS/mod_set_site_RW_b_BiasTerms_1000.rds")


# Not on GitHub
# save.image("Unity/04_model_fitting_BiasTerms_JMH2_250steps_RDat")

