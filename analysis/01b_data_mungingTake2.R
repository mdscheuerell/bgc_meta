# This script calculates flow weighted means for all unmanaged sites
# JMH started 30 Jan 2021




##-------------------
## required packages
##-------------------
library(here)
library(readr)
library(tidyverse)
library(readxl)


##-------------------
## Load data
##-------------------

# ELA
# units on Q are m3/s
# no info on qualifiers

# ELA EIF
# 318 NAs/ 16802, 16,802 days 
#1971-2016
ELA.eif <- readxl::read_xlsx(file.path(here::here("data/NewDataFromIrena20210130/New MAR Data/Raw Data Files/ELA"), 
                                          "ELA EIF discharge.xlsx"),1) %>% 
        mutate_at(vars(data_set:station,qualifier), factor)

ggplot(ELA.eif, aes(y = mean_daily_discharge, mean_date)) +
  geom_point()

# ELA NEIF
# 104 NAs/16606
# 1971 to 2016 - 16,748 days
# There are 142 days missing here
ELA.neif <- readxl::read_xlsx(file.path(here::here("data/NewDataFromIrena20210130/New MAR Data/Raw Data Files/ELA"), 
                                       "ELA NEIF discharge.xlsx"),1) %>% 
  mutate_at(vars(data_set:station,qualifier), factor)

ggplot(ELA.neif, aes(y = mean_daily_discharge, mean_date)) +
  geom_point()

# ELA NWIF
# 227 NAs/127113 
# 1970 to 2016 - 17,113 days
ELA.nwif <- readxl::read_xlsx(file.path(here::here("data/NewDataFromIrena20210130/New MAR Data/Raw Data Files/ELA"), 
                                        "ELA NWIF discharge.xlsx"),1) %>% 
  mutate_at(vars(data_set:station,qualifier), factor)

ggplot(ELA.neif, aes(y = mean_daily_discharge, mean_date)) +
  geom_point()

# ELA chemistry
# 1970-04-07 to 2017-03-23
# codes in solute columns
    # -1111 analysis not conducted
    # -1 below minimum detection
    # 0 analysis not requested
    # -200 no sample available
    # other wise not dealing with detection limits ------ WAS THIS THE GROUP DECISION?
# Variable NAs ~ 40-90 of 5163
# --------- WHAT ARE THE SUBLOC AND DO I WANT ANY

BadCodeNAfun <- function(x){
  x <- ifelse(x <= 0, as.numeric("NA"), x)
  x
}


ELA.chem <- readxl::read_xlsx(file.path(here::here("data/NewDataFromIrena20210130/New MAR Data/Raw Data Files/ELA"), 
                                        "ELA stream chemistry.xlsx"),2) %>% 
  mutate_at(vars(TYPE:STATION), factor) %>% 
  # select solutes of interest
  select("SAMPLED", "SUBLOC", "STATION", "CA", "DOC", "NH4", "NO3", "TDP", "SO4") %>% 
  # select focal catchments
  filter(STATION %in% c("EIF", "NWIF", "NEIF")) %>% 
  # remove solute values that are codes (all below zero)
  mutate(across(CA:SO4, BadCodeNAfun))#ifelse(. <= 0, as.numeric("NA"),.)


ggplot(ELA.chem %>% 
         pivot_longer(CA:SO4, names_to = "solutes", values_to = "values"), aes(y = values, x = SAMPLED)) +
  geom_point() +
  facet_wrap(vars(solutes), scales = "free_y")

# COMBINE Q AND CHEM
ELA.eif2 <- ELA.eif %>% 
  full_join(ELA.chem %>% 
              filter(STATION == "EIF"), by = c("mean_date" = "SAMPLED"))

########## NEED TO CHECK THIS #########

                            