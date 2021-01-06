library(tidyverse)
library(GGally)

df <- readr::read_csv(file.path(here::here("data"), "monthly_data.csv")) %>% 
  select(catchment, Year, Month, StrCamgL:StrSO4SmgL, FWACamgL:FWASO4SmgL) %>% 
  mutate(date = as.POSIXct(paste0(Year, "-", Month, "-01"), format ="%Y-%m-%d"))

ggpairs(df %>% 
          select(StrCamgL:FWASO4SmgL))

