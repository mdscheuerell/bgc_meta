### Assess approaches for handling values below detection ###
## Author: TKH

library(tidyverse)
library(here)
library(readxl)

## Data ##
# Raw concentrations only

file_list <- list.files(path = here("data", "JMHnewMungedDat"),
                            recursive=F, 
                            pattern="01_", 
                            full.names=TRUE)

dat <- do.call("rbind", lapply(file_list, 
                                   read.csv, 
                                   stringsAsFactors=FALSE, 
                                   header=TRUE))

# Format date/time
dat <- dat %>% separate(Date, into = c("Date", "Time"), sep = "T") %>%
               mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

# LOQ notes
LOQs <- read_excel(here("data", "JMHnewMungedDat", "00_WatershedDataNotes.xlsx"), sheet = "LOQ")

LOQs <- LOQs %>% unite(LOQ_id, Site, Date_start, Date_end, sep = " ", remove = FALSE, na.rm = TRUE)

## Plots ##
# Can't find a way to annotate reference lines w/ facets

Ca.LOQ <- LOQs %>% filter(Solute == "Ca")
Ca.pl <- ggplot(data = dat, aes(x = Date, y = Ca_mgL, color = WS)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = Ca.LOQ$LOQ) +
  facet_wrap(~Site, ncol = 3, scales = "free_y")
      
DOC.LOQ <- LOQs %>% filter(Solute == "DOC")
DOC.pl <- ggplot(data = dat, aes(x = Date, y = DOC_mgL, color = WS)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = DOC.LOQ$LOQ) +
  facet_wrap(~Site, ncol = 3, scales = "free_y")

NH4.LOQ <- LOQs %>% filter(Solute == "NH4")
NH4.pl <- ggplot(data = dat, aes(x = Date, y = NH4_mgL, color = WS)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = NH4.LOQ$LOQ) +
  facet_wrap(~Site, ncol = 3, scales = "free_y")

NO3.LOQ <- LOQs %>% filter(Solute == "NO3")
NO3.pl <- ggplot(data = dat, aes(x = Date, y = NO3_mgL, color = WS)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = NO3.LOQ$LOQ) +
  facet_wrap(~Site, ncol = 3, scales = "free_y")

SRP.LOQ <- LOQs %>% filter(Solute == "PO4")
SRP.pl <- ggplot(data = dat, aes(x = Date, y = SRP_mgL, color = WS)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = SRP.LOQ$LOQ) +
  facet_wrap(~Site, ncol = 3, scales = "free_y")

SO4.LOQ <- LOQs %>% filter(Solute == "SO4")
SO4.pl <- ggplot(data = dat, aes(x = Date, y = SO4_mgL, color = WS)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = SO4.LOQ$LOQ) +
  facet_wrap(~Site, ncol = 3, scales = "free_y")

dir.create(here("plots", "LOQ"))
ggsave(SO4.pl, path = here("plots", "LOQ"), file = "SO4.LOQ.pdf", width = 12, height = 10, units = "in")

## geom segment for time-varying LOQ @ TLW

## Time series of LOQ for SLP- where are the input data with LOQ?