# This script combines all Q and chem data for all unmanaged SLP sites
# JMH; 18 May 21, 14 Jul 21

# required packages ----
library(here)
library(readr)
library(tidyverse)
library(readxl)
library(GGally)
library(openxlsx)

# Common time period ----
CTstart <- as.POSIXct(paste0("1985-10-31", format = "%Y-%m-%d"))
CTend <- as.POSIXct(paste0("2010-10-31", format = "%Y-%m-%d"))
# 9130 days

# Discharge & Chem ----
# Q in cfs

# THIS IS A NEW DATASET FROM THE SLEEPERS TEAM
# PREVIOUS DATASET HAD SOME SRP DATA DURING OUR FOCAL PERIOD, BUT THIS DOES NOT
# had some issues with this. had to change all µ's to u's and added "zz_" to column names that started with #s
SLPall <- read.csv("01_data/SleepersRiverAqueousChemistry.csv") %>% 
          filter(Sample_Name == "W-9" & Sample_Type == "ST") %>% 
          mutate(EventBased = as.factor(ifelse(SSN >= 50000, "Routine", "Event"))) %>% 
          mutate(Date_Time = as.POSIXct(Date_Time, format = "%m/%d/%y")) %>% 
          select(Sample_Name, Sample_Type, Replicate, Date_Time, W9_Flow_cfs, Chemistry_Flag,
                 Ca.ueq.L, DOC.mg.L, NH4.ueq.L, NO3.ueq.L, PO4.mg.P.L, SO4.ueq.L,
                 Ca.ueq.L_Lab, DOC.mg.L_Lab, NH4.ueq.L_Lab, NO3.ueq.L_Lab, PO4.mg.P.L_Lab, SO4.ueq.L_Lab,
                 EventBased) %>% 
          mutate(across(Ca.ueq.L_Lab:SO4.ueq.L_Lab, factor)) 

# multiple labs ran chem
# only one date with data from 2 labs
SLPall %>% 
  select(-Replicate, -Ca.ueq.L_Lab:SO4.ueq.L_Lab) %>%
  filter(duplicated(.))

SLPall %>% filter(EventBased == "Event")

# seems like a lot more samples
SLPallL <- SLPall %>% 
  select(Sample_Name, Sample_Type, Replicate, Date_Time, W9_Flow_cfs, Chemistry_Flag,
         Ca.ueq.L, DOC.mg.L, NH4.ueq.L, NO3.ueq.L, PO4.mg.P.L, SO4.ueq.L,
         EventBased) %>% 
  pivot_longer(cols = c("W9_Flow_cfs", "Ca.ueq.L", "DOC.mg.L", "NH4.ueq.L", "NO3.ueq.L", "PO4.mg.P.L", "SO4.ueq.L"),
               names_to = "solute", values_to = "conc") %>% 
  mutate(ChemFlag = ifelse(is.na(Chemistry_Flag), "NotFlagged", "Flagged"))

# switch shape and color depending upon whether focus is sampling or flags
ggplot(SLPallL, aes(y = conc, x = Date_Time, shape = EventBased, color = ChemFlag)) +
  geom_point(size = 0.5) +
  facet_grid(solute ~., scales = "free_y")


# These were flagged and removed
SLPflagged <- SLPall %>% 
  # just going to remove the flagged samples to be safe - removes 46 rows
  # most samples from USGS Aikin or Troy labs
  mutate(ChemFlag = ifelse(is.na(Chemistry_Flag), "NotFlagged", "Flagged")) %>% 
  filter(ChemFlag == "Flagged")

# Convert to mgL
SLPall2 <- SLPall %>% 
  # just going to remove the flagged samples to be safe - removes 46 rows
  # most samples from USGS Aikin or Troy labs
  mutate(ChemFlag = ifelse(is.na(Chemistry_Flag), "NotFlagged", "Flagged")) %>% 
  filter(ChemFlag != "Flagged") %>% 
  # detection limit for NH4 and NO3 changes a lot among labs
  # applying conservative detection limit of 2 ueq/L NH4 and No3
  mutate(NH4.ueq.L = ifelse(NH4.ueq.L <= 2, 2, NH4.ueq.L),
         NO3.ueq.L = ifelse(NO3.ueq.L <= 2, 2, NO3.ueq.L)) %>% 
  mutate(across(c(Sample_Name:Replicate), factor)) %>% 
  mutate(Site = "SLP",
         WS = "W9",
         Date = Date_Time) %>% 
  mutate(Q_Ls = W9_Flow_cfs * 28.32, 
         # 1 ueq/L = u mole * charge = (ug/atomic mass) * charge
         # so mg Ca/L = ueqL / charge * atomic mass /1000
         Ca_mgL = Ca.ueq.L /2 * 40.078 / 1000, 
         DOC_mgL = DOC.mg.L, 
         NH4_mgL = NH4.ueq.L/1 * 18.039 * 14.006/18.039 /1000, # converting to mg NH4-N/L
         NO3_mgL = NO3.ueq.L/1 * 46.006 * 14.006/46.006 /1000, # converting to mg NO3-N/L
         SRP_mgL = PO4.mg.P.L, 
         SO4_mgL = SO4.ueq.L/2 * 96.06 * 32.066/96.06 /1000) %>% 
  select(Site, WS, Replicate, Date, Q_Ls, Ca_mgL:SO4_mgL) %>% 
  # samples below the detection limit in data set are -1 * detection limit; forcing all to DL
  mutate(across(c(Ca_mgL:SO4_mgL), abs)) %>% 
  # Average across Dates to remove reps: 4258 to 2241 rows 
  group_by(Site, WS, Date) %>% 
  summarize(across(Q_Ls:SO4_mgL, mean, na.rm = TRUE)) 


# much of NH4 is close to detection limit
ggplot(SLPall2 %>% 
         pivot_longer(cols = c(Q_Ls:SO4_mgL), names_to = "solute", values_to = "conc") %>% 
         mutate(solute = fct_relevel(solute, "Q_Ls", "Ca_mgL", "DOC_mgL", "NH4_mgL", "NO3_mgL", "SRP_mgL", "SO4_mgL")), 
       aes(y = conc, x = Date)) +
  geom_point(size = 0.5) +
  facet_grid(solute ~., scales = "free_y")


# EXPORT DATAFRAME ----
write_csv(SLPall2, file.path(here::here("03_generatedData/"), "01_SLPcomb.csv"))


