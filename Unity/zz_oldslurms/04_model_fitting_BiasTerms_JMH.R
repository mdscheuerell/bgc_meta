
# library(tidyverse, lib.loc = "/home/hood.211/R_libs")
#KFASurl and MARSurl were uncommented during last run
# KFASurl <- "https://cran.r-project.org/src/contrib/Archive/KFAS/KFAS_1.3.6.tar.gz"
# install.packages(KFASurl, repos = NULL, type = "source")
# MARSurl <- "https://cran.r-project.org/src/contrib/Archive/MARSS/MARSS_3.10.12.tar.gz"
# install.packages(MARSurl, repos = NULL, type = "source")
# mvtnormURL <- "https://cran.r-project.org/src/contrib/Archive/mvtnorm/mvtnorm_0.1-8.tar.gz"
# install.packages(mvtnormURL, repos = NULL, type = "source")

# library(mvtnorm)
# installibrary(KFAS)
library(MARSS)
# library(doParallel, lib.loc = "/home/hood.211/R_libs")
library(doParallel)

#/tmp/RtmpEsRhTh/downloaded_packages
#‘/tmp/Rtmp0Y9pwz/downloaded_packages’ 14 Sept 22

# load MARSS model
mod_set_RW_b <- readRDS("fitted_seas_unique_states_RW_b.rds")



## bootstrap biased RW models by site
# startTime <- 
Sys.time()
# 2 nboot took 1.298418 hours on iMAc; 40.12 mins on MacS; 1.43 on super computer
# Meaning the whole thing will take 27 days or 14 days

bias_bootstrap <- mclapply(mod_set_RW_b, MARSSparamCIs, method = "parametric", nboot = 2, mc.cores = 6)
# EndTime <- 
Sys.time()

# Save
save(bias_bootstrap, file = "~mod_set_site_RW_b_BiasTerms")