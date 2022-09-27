# JMH Sept 16 22

library(MARSS)
library(doParallel)

# load MARSS model
mod_set_RW_b <- readRDS("fitted_seas_unique_states_RW_b.rds")



## bootstrap biased RW models by site
# startTime <- 

#mclapply
# 2 nboot took 1.298418 hours on iMAc; 40.12 mins on MacS; 1.43 on super computer
# Meaning the whole thing will take 27 days or 14 days

startTime <-Sys.time()
bias_bootstrap <- mclapply(mod_set_RW_b, MARSSparamCIs, method = "parametric", nboot = 1000, mc.cores = 6)
EndTime <- Sys.time()
EndTime - startTime

# for each
# https://www.r-bloggers.com/2016/07/lets-be-faster-and-more-parallel-in-r-with-doparallel-package/
# This takes 57 mins on MacS
# for some reason used this for the 250 step version, 2x as long as mclapply
# no_cores <- detectCores() - 1
# cl <- makeCluster(no_cores, type = "FORK")
# registerDoParallel(cl)
# startTime <-Sys.time()
# bias_bootstrap <- foreach(i = mod_set_RW_b) %dopar% MARSSparamCIs(i, method = "parametric", nboot = 250)

EndTime <- Sys.time()
EndTime - startTime
# Save
saveRDS(bias_bootstrap, file = "mod_set_site_RW_b_BiasTerms.rds")
save.image("04_model_fitting_BiasTerms_JMH2_250steps_RDat")

