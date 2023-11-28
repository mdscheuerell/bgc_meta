# mann kendal and sens slopes
# JMH Nov 2022
# JMH JUN 2023

# libraries
library(tidyverse)
library(trend)
library(EnvStats)
library(here)
library(broom)

# Data used in MARSS models
df <- readr::read_csv(here::here("data", "tbl_solutes_unmanaged_mon_v2.csv")) %>% 
        pivot_longer(cols = Ca:TDP, names_to = "solute", values_to = "FWMC") %>% 
        # have to remove NAs
        filter(!is.na(FWMC)) %>% 
        # log and center on mean
        group_by(catchment, solute) %>% 
        mutate(FWMC = log(FWMC),
               FWMC = scale(FWMC, scale = FALSE))

# Mann-Kendall & sens slope
# tested a few to make sure the code below was working
# mk.test(df[df$catchment == "EB" & df$solute == "Ca",]$FWMC)

df_mk <- df %>% 
          group_by(catchment, solute) %>% 
          nest() %>% 
          mutate(mk_pval = map(data, ~trend::mk.test(.x$FWMC, alternative = "two.sided")$p.value),
                 mk_n = map(data, ~trend::mk.test(.x$FWMC, alternative = "two.sided")$parameter),
                 mk_stat = map(data, ~trend::mk.test(.x$FWMC, alternative = "two.sided")$statistic),
                 s_slope = map(data, ~trend::sens.slope(.x$FWMC)$estimates),
                 s_L95 = map(data, ~trend::sens.slope(.x$FWMC)$conf.int[1]),
                 s_U95 = map(data, ~trend::sens.slope(.x$FWMC)$conf.int[2])) %>% 
          unnest(c(mk_pval, mk_n, mk_stat, s_slope, s_L95, s_U95)) %>% 
          select(-data)

# Model coefs ----
# run in 04_model_fitting_BiasTerms_1000bootstraps.R
biasBS <- readRDS(file = file.path(here::here("analysis"), "mod_set_site_RW_b_BiasTerms_1000.rds"))

# Bias plot ----

## create table of bias estimates (+/- CI) ----

SoluteList <- c("Ca", "DOC", "NO3N", "SO4", "NH4N", "TDP")

tmp <- list()
for(i in 1:length(SoluteList)) {
  bias_ID <- rownames(biasBS[[i]]$par$U)
  tmp$solute <- rep(SoluteList[i], length(bias_ID))
  tmp$site <- bias_ID
  tmp$bias <- biasBS[[i]]$par$U
  tmp$loCI <- biasBS[[i]]$par.lowCI$U
  tmp$upCI <- biasBS[[i]]$par.upCI$U
  if(i == 1) {
    tbl_fit_bootstrap <- data.frame(tmp)
  } else {
    tbl_fit_bootstrap <- rbind(tbl_fit_bootstrap, data.frame(tmp))
  }
}
tbl_fit_bootstrap[, -c(1:2)] <- signif(tbl_fit_bootstrap[, -c(1:2)], 3)
rownames(tbl_fit_bootstrap) <- NULL


## cleans up df ----
tbl_fit_bias_bs <- tbl_fit_bootstrap %>% 
  # remove seasonality coefs
  filter(!grepl("seas", site)) %>% 
  separate(site, sep = "_", into = c("region", "site", "watershed")) %>% 
  mutate_at("region", str_remove, "X.") %>%
  #units percent decline/mo to percent decline/yr
  mutate(
    # Convert coef to % change/year
    # GET MARK TO CHECK
    U_perChange_y = (exp(bias)-1)*12*100, 
    U_perChange_y_lowCI = (exp(loCI)-1)*12*100,
    U_perChange_y_upCI = (exp(upCI)-1)*12*100) %>% 
  mutate_at(c("region", "site", "watershed"), factor)  %>%
  mutate(site = fct_relevel(site, c("HJA", "ELA", "MEF", "TLW", "DOR", "HBEF", "BBWM", "SLP")),
         watershed = fct_relevel(watershed,
                                 c("GSWS08", "GSWS09",
                                   "EIF", "NEIF", "NWIF",
                                   "S2", "S5",
                                   "C32", "C35", "C38",
                                   "HP3", "HP3A", "HP4", "HP5", "HP6", "HP6A",
                                   "WS6", "WS7", "WS8", "WS9",
                                   "EB",
                                   "W9"))) %>% 
  # Label sig if CI's don't overlap zero
  mutate(Sig = ifelse((loCI > 0 & upCI > 0) | (loCI <0 & upCI < 0),
                      "Sig", "NS"),
         Sig = fct_relevel(Sig, "Sig", "NS"))
     
# combine
df_mk_c <- df_mk %>% 
              mutate(Sig = ifelse(mk_pval < 0.05, "Sig", "NS"),
                     s_slope = s_slope*12*100,
                     s_L95 = s_L95*12*100,
                     s_U95 = s_U95*12*100) %>% 
              select(solute, watershed = catchment, Bias = s_slope, Bias_L95 = s_L95, Bias_U95 = s_U95, pval = mk_pval, Sig) %>% 
              mutate(analysis = "MKandSens",
                     solute = ifelse(solute == "NH4","NH4N",
                                  ifelse(solute == "NO3", "NO3N", solute)))

tbl_fit_bias_bs_c <- tbl_fit_bias_bs %>% 
                          mutate(pval = as.numeric("NA"),
                                 analysis = "MARSS") %>% 
                        select(solute, watershed, Bias = U_perChange_y, Bias_L95 = U_perChange_y_lowCI, 
                               Bias_U95 = U_perChange_y_upCI, pval, Sig, analysis)


MARSSmkTab <- rbind(df_mk_c, tbl_fit_bias_bs_c)

ggplot(MARSSmkTab, aes(y = Bias, x = analysis, color = Sig)) +
  geom_pointrange(aes(ymin = Bias_L95, ymax = Bias_U95)) +
  facet_grid(solute ~ watershed, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90))

ggplot(MARSSmkTab %>% 
         ungroup() %>% 
         group_by(analysis) %>% 
         mutate(ID = as.factor(paste0(solute,"_", watershed)),
                ID = fct_reorder(ID, Bias)), aes(y = Bias, x = ID, shape = analysis, color = Sig)) +
        geom_point()+
        theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(vars(solute), scales = "free_y")
        # geom_pointrange(aes(ymin = Bias_L95, ymax = Bias_U95)) 

## FIG 7 ----
MARSSmkTab %>%
  ungroup() %>% 
  mutate(watershed = as.factor(watershed),
         watershed = fct_reorder(watershed, Bias)) %>% 
  mutate(watershed = fct_relevel(watershed,
                                 # BBWM
                                 "EB", 
                                 # DOR
                                 "HP3",    "HP3A",   "HP4",    "HP5",    "HP6",    "HP6A",
                                 # ELA
                                 "EIF",    "NEIF",   "NWIF",
                                 # HBEF
                                 "WS6",    "WS7",    "WS8",    "WS9",
                                 # MEF - NO NO3 DATA FOR MEF
                                 "S2",     "S5",
                                 # SLP
                                 "W9",
                                 # TLW
                                 "C32",    "C35",    "C38",
                                 # HJA
                                 "GSWS08", "GSWS09")) %>% 
  mutate(Sig = case_when(Sig == "Sig" ~ "Yes",
                         Sig == "NS" ~ "No"),
         analysis = case_when(analysis == "MARSS" ~ "MARSS",
                              analysis == "MKandSens" ~ "Sens slope"),
         solute2 = fct_recode(solute, "Calcium" = "Ca", "DOC" = "DOC",
                              "Ammonium" = "NH4N", "Nitrate" = "NO3N", "TDP" = "TDP",
                              "Sulfate" = "SO4")) %>% 
  # fig
  ggplot() +
  geom_hline(yintercept = 0, color = "grey") +
  geom_pointrange(aes(y = Bias, x = watershed, ymin = Bias_L95, ymax = Bias_U95, 
                      fill = Sig, shape = analysis), 
                  position = position_jitter(w = 0.3), 
                  size = 1.25) +
  scale_fill_manual(values = c("grey90", "steelblue"), name = "P < 0.05") +
  scale_shape_manual(values = c(21,22), name = "Analysis") +
  facet_wrap(vars(solute2),
             nrow = 3,
             ncol = 2) +
  ylim(-30,20) +
  ylab(expression(paste("Bias ± 95% CI (% change ", y^-1,")"))) +
  theme_bw() +
  guides(fill=guide_legend(override.aes=list(shape=21))) +
  theme(legend.position = "top",
        legend.background = element_rect(fill = NA, color = NA),
        legend.text = element_text(size = 26),
        legend.title = element_text(size = 28, face = "bold"),
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 2),
        plot.margin = unit(c(t = 0.5, r = 0.5, b = 0.5, l = 0.5), "cm"),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(size = 20, vjust = 0.5, hjust = 1, angle = 90),
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 34)) +
  ggsave(path = "plots", file = "MARSS_SensSlopeComp.pdf", width = 16, height = 10, units = "in")

## export table ----
MARSSmkTab_2 <- MARSSmkTab %>% 
  pivot_wider(id_cols = c("solute", "watershed"), names_from = analysis, values_from = Bias:Sig) %>% 
  select(solute, watershed, Bias_MARSS, Bias_L95_MARSS, Bias_U95_MARSS, Sig_MARSS,
         Bias_MKandSens, Bias_L95_MKandSens, Bias_U95_MKandSens, pval_MKandSens, Sig_MKandSens) %>% 
  arrange(solute, watershed)

write.csv(MARSSmkTab_2, "Tables/08_MARSS_SensSlops_BiasTable.csv", row.names = FALSE)


## TKH recreate bias & Sen plot
MARSSmkTab_2 <- read.csv("Tables/08_MARSS_SensSlops_BiasTable.csv")

Sens <- MARSSmkTab_2 %>% select(solute, watershed, ends_with("Sens"))
mars <- MARSSmkTab_2 %>% select(solute, watershed, ends_with("MARSS"))

names(Sens) <- c("solute", "watershed", "Bias", "Bias_L95", "Bias_U95", "pval", "Sig")
names(mars) <- c("solute", "watershed", "Bias", "Bias_L95", "Bias_U95", "Sig")

Sens <- Sens %>% mutate(analysis = "MKandSens")
mars <- mars %>% mutate(analysis = "MARSS") %>%
                 mutate(Sig = ifelse((Bias_L95 > 0 & Bias_U95 > 0) | (Bias_L95 < 0 & Bias_U95 < 0), "Sig", "NS"),
                 Sig = fct_relevel(Sig, "Sig", "NS"))

marMK <- bind_rows(Sens, mars)

marMK <- marMK %>% mutate(solute2 = fct_recode(solute, "Calcium" = "Ca", 
                                                       "DOC" = "DOC",
                                                       "Ammonium" = "NH4N", 
                                                       "Nitrate" = "NO3N", 
                                                       "TDP" = "TDP",
                                                       "Sulfate" = "SO4")) %>%
                   mutate(watershed = fct_relevel(watershed,
                                 # BBWM
                                 "EB", 
                                 # DOR
                                 "HP3",    "HP3A",   "HP4",    "HP5",    "HP6",    "HP6A",
                                 # ELA
                                 "EIF",    "NEIF",   "NWIF",
                                 # HBEF
                                 "WS6",    "WS7",    "WS8",    "WS9",
                                 # MEF - NO NO3 DATA FOR MEF
                                 "S2",     "S5",
                                 # SLP
                                 "W9",
                                 # TLW
                                 "C32",    "C35",    "C38",
                                 # HJA
                                 "GSWS08", "GSWS09")) %>%
                   mutate(Sig = case_when(Sig == "Sig" ~ "Yes", Sig == "NS" ~ "No"))

biasSen.pl <- marMK %>% filter(!is.na(Sig)) %>%
                        ggplot(x = watershed, y = Bias) +
                               geom_hline(yintercept = 0, color = "grey") +
                               geom_pointrange(aes(y = Bias, x = watershed, ymin = Bias_L95, ymax = Bias_U95, 
                                                   fill = Sig, shape = analysis), 
                                                   position = position_jitter(w = 0.3), 
                                                   size = 1.25) +
                               scale_fill_manual(values = c("grey90", "steelblue"), name = "P < 0.05") +
                               scale_shape_manual(values = c(21,22), name = "Analysis") +
                               facet_wrap(vars(solute2), nrow = 3, ncol = 2) +
                               ylim(-25,25) +
                               ylab(expression(paste("Bias ± 95% CI (% change ", y^-1,")"))) +
                               theme_bw() +
                               guides(fill = guide_legend(override.aes=list(shape=21))) +
                               theme(legend.position = "bottom",
                                  legend.background = element_rect(fill = NA, color = NA),
                                  legend.text = element_text(size = 24),
                                  legend.title = element_text(size = 24, face = "bold"),
                                  panel.grid = element_blank(),
                                  panel.border = element_rect(color = "black", linewidth = 2),
                                  plot.margin = unit(c(t = 0.5, r = 0.5, b = 0.5, l = 0.5), "cm"),
                                  axis.text.y = element_text(size = 20),
                                  axis.title.y = element_text(size = 24),
                                  axis.text.x = element_text(size = 20, vjust = 0.5, hjust = 1, angle = 90),
                                  axis.title.x = element_blank(),
                                  strip.background = element_blank(),
                                  strip.text = element_text(size = 24))

ggsave(biasSen.pl, path = here("plots"), file = "Fig7_MARSS_Sen.pdf", width = 12, height = 14, units = "in")

# save.image("analysis/08_MKandSensSlope_Rdat")
# load("analysis/08_MKandSensSlope_Rdat")

#########################
### Seasonal MK & Sen ###
#########################

### Import data used in MARSS models
df.seas <- readr::read_csv(here::here("data", "tbl_solutes_unmanaged_mon_v2.csv")) 
  
## Make regular time series for compatibility with Mann-Kendall function
## Log & center FWMC 
df.seas <- df.seas %>% mutate(date = as.Date(round_date(date_decimal(dec_water_yr), "month"), format = "%Y-%m")) %>%
                       group_by(site, catchment) %>%
                       complete(date = seq.Date(from = as.Date("1986-01-01"), to = as.Date("2020-12-01"), by = "month")) %>%
                       mutate(year = as.numeric(format(date, "%Y"))) %>%
                       mutate(month = as.numeric(format(date, "%m"))) %>%
                       pivot_longer(cols = Ca:TDP, names_to = "solute", values_to = "FWMC") %>% 
                       ungroup() %>%
                       # log and center on mean...check whether MARSS demeaned by catchment or across all data
                       group_by(solute, catchment) %>%
                       mutate(FWMC = c(scale(log(FWMC), scale = FALSE)))
                  
### Test seasonal MK for single site
# Independent.obs = FALSE requires contiguous data 
kseas <- df.seas %>% filter(catchment == "EIF" & solute == "NO3") %>%
                     filter(!(month %in% c(3,4,5))) %>% 
                     kendallSeasonalTrendTest(FWMC ~ month + year, 
                                               data = .,
                                               alternative = "two.sided",
                                               ci.slope = TRUE,
                                               conf.level = 0.95, 
                                               independent.obs = FALSE)

kseas$p.value[2] # p-value for trend
kseas$parameter # df
kseas$statistic # test statistics for heterogeneity and trend
kseas$interval["limits"][[1]]
kseas$interval$limits[[1]]
kseas$estimate["slope"] # sen's slope

### Seasonal MK all catchments and solutes                                       

## Remove sites*solutes lacking data
df.seas <- df.seas %>% filter(!(solute == "TDP" & site %in% c("BBWM", "HJA", "MEF", "SLP"))) %>%
                       filter(!(solute == "NH4" & site %in% c("BBWM", "HJA", "MEF", "SLP"))) %>%
                       filter(!(solute == "NO3" & site %in% c("MEF"))) %>%
                       filter(!(solute == "NH4" & catchment %in% c("C32", "C35", "WS6"))) %>%
                       filter(!(solute == "NO3" & catchment %in% c("WS6"))) %>%
                       filter(!(solute == "TDP" & catchment %in% c("WS6", "WS7", "WS8", "WS9"))) %>% 
                       # Sites/solutes lacks data (n <= 3) in dropped months
                       filter(!(catchment == "EIF" & solute %in% c("Ca", "DOC", "NH4", "SO4", "TDP") & month %in% c(4,5))) %>% 
                      filter(!(catchment == "EIF" & solute %in% c("NO3") & month %in% c(3,4,5))) %>% 
                      filter(!(catchment == "NEIF" & solute %in% c("Ca", "DOC", "NH4", "NO3", "SO4", "TDP") & month %in% c(3,4,5)))  %>% 
                      filter(!(catchment == "NWIF" & solute %in% c("Ca", "DOC", "NH4", "NO3", "SO4", "TDP") & month %in% c(3,4,5))) %>% 
                      filter(!(catchment == "S2" & solute %in% c("Ca", "DOC", "NH4", "NO3", "SO4", "TDP") & month %in% c(4,5))) %>% 
                      filter(!(catchment == "S5" & solute %in% c("Ca", "DOC", "NH4", "NO3", "SO4", "TDP") & month %in% c(5,6))) 
  
## Shouldn't need to rerun models for each stat. h.test object doesn't play with tidy.
df_seasmk <- df.seas %>% 
  group_by(catchment, solute) %>% 
  nest() %>% 
  mutate(mk_hetstat = map(data, ~ kendallSeasonalTrendTest(.x$FWMC ~ month + year, 
                                                           data = .,
                                                           alternative = "two.sided",
                                                           ci.slope = TRUE,
                                                           conf.level = 0.95, 
                                                           independent.obs = FALSE)$statistic[1])) %>%
  mutate(mk_trendzstat = map(data, ~ kendallSeasonalTrendTest(.x$FWMC ~ month + year, 
                                                           data = .,
                                                           alternative = "two.sided",
                                                           ci.slope = TRUE,
                                                           conf.level = 0.95, 
                                                           independent.obs = FALSE)$statistic[2])) %>%
  mutate(mk_het_pval = map(data, ~ kendallSeasonalTrendTest(.x$FWMC ~ month + year, 
                                                              data = .,
                                                              alternative = "two.sided",
                                                              ci.slope = TRUE,
                                                              conf.level = 0.95, 
                                                            independent.obs = FALSE)$p.value[1])) %>%
  mutate(mk_pval = map(data, ~ kendallSeasonalTrendTest(.x$FWMC ~ month + year, 
                                                            data = .,
                                                            alternative = "two.sided",
                                                            ci.slope = TRUE,
                                                            conf.level = 0.95, 
                                                        independent.obs = FALSE)$p.value[2])) %>%
  mutate(mk_df = map(data, ~ kendallSeasonalTrendTest(.x$FWMC ~ month + year, 
                                                        data = .,
                                                        alternative = "two.sided",
                                                        ci.slope = TRUE,
                                                        conf.level = 0.95, 
                                                      independent.obs = FALSE)$parameter)) %>%
  mutate(sen_sl = map(data, ~ kendallSeasonalTrendTest(.x$FWMC ~ month + year, 
                                                      data = .,
                                                      alternative = "two.sided",
                                                      ci.slope = TRUE,
                                                      conf.level = 0.95, 
                                                      independent.obs = FALSE)$estimate[["slope"]])) %>%
  mutate(sen_L95 = map(data, ~ kendallSeasonalTrendTest(.x$FWMC ~ month + year, 
                                                      data = .,
                                                      alternative = "two.sided",
                                                      ci.slope = TRUE,
                                                      conf.level = 0.95, 
                                                      independent.obs = FALSE)$interval$limits[[1]])) %>%
  mutate(sen_U95 = map(data, ~ kendallSeasonalTrendTest(.x$FWMC ~ month + year, 
                                                        data = .,
                                                        alternative = "two.sided",
                                                        ci.slope = TRUE,
                                                        conf.level = 0.95, 
                                                        independent.obs = FALSE)$interval$limits[[2]])) %>%
         
  unnest(c(mk_hetstat, mk_trendzstat, mk_het_pval, mk_pval, mk_df, sen_sl, sen_L95, sen_U95)) %>% 
  select(-data)

# combine
df_seasmk_c <- df_seasmk %>% mutate(Sig = ifelse(mk_pval < 0.05, "Sig", "NS"),
                                    s_slope = sen_sl*100,
                                    s_L95 = sen_L95*100,
                                    s_U95 = sen_U95*100) %>% 
                              # remove cases where tau is sig - monthly slopes are pos & neg
                              # this removes 28 site x solute combos
                              filter(mk_het_pval > 0.05) %>% 
                             select(solute, watershed = catchment, Bias = s_slope, Bias_L95 = s_L95, Bias_U95 = s_U95, pval = mk_pval, Sig) %>% 
                             mutate(analysis = "MKandSens",
                                    solute = ifelse(solute == "NH4","NH4N",
                                    ifelse(solute == "NO3", "NO3N", solute))) 

tbl_fit_bias_bs_c <- tbl_fit_bias_bs %>% 
  mutate(pval = as.numeric("NA"),
         analysis = "MARSS") %>% 
  select(solute, watershed, Bias = U_perChange_y, Bias_L95 = U_perChange_y_lowCI, 
         Bias_U95 = U_perChange_y_upCI, pval, Sig, analysis)


MARSSseasmkTab <- rbind(df_seasmk_c, tbl_fit_bias_bs_c)

ggplot(MARSSseasmkTab, aes(y = Bias, x = analysis, color = Sig)) +
  geom_pointrange(aes(ymin = Bias_L95, ymax = Bias_U95)) +
  facet_grid(solute ~ watershed, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90))


ggplot(MARSSseasmkTab %>% 
         ungroup() %>% 
         group_by(analysis) %>% 
         mutate(ID = as.factor(paste0(solute,"_", watershed)),
                ID = fct_reorder(ID, Bias)), aes(y = Bias, x = ID, shape = analysis, color = Sig)) +
  geom_point()+
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(vars(analysis))
# geom_pointrange(aes(ymin = Bias_L95, ymax = Bias_U95)) 

## FIG 7 ----
Fig7df <- MARSSseasmkTab %>%
                            ungroup() %>% 
                            mutate(watershed = as.factor(watershed),
                                   watershed = fct_reorder(watershed, Bias)) %>% 
                            mutate(watershed = fct_relevel(watershed,
                                 # BBWM
                                 "EB", 
                                 # DOR
                                 "HP3",    "HP3A",   "HP4",    "HP5",    "HP6",    "HP6A",
                                 # ELA
                                 "EIF",    "NEIF",   "NWIF",
                                 # HBEF
                                 "WS6",    "WS7",    "WS8",    "WS9",
                                 # MEF - NO NO3 DATA FOR MEF
                                 "S2",     "S5",
                                 # SLP
                                 "W9",
                                 # TLW
                                 "C32",    "C35",    "C38",
                                 # HJA
                                 "GSWS08", "GSWS09")) %>% 
                            mutate(Sig = case_when(Sig == "Sig" ~ "Yes", Sig == "NS" ~ "No"),
                                   analysis = case_when(analysis == "MARSS" ~ "MARSS",
                                   analysis == "MKandSens" ~ "Sens slope"),
                                   solute2 = fct_recode(solute, "Calcium" = "Ca", "DOC" = "DOC",
                              "Ammonium" = "NH4N", "Nitrate" = "NO3N", "TDP" = "TDP",
                              "Sulfate" = "SO4")) 
  # fig
  ggplot(Fig7df) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_pointrange(aes(y = Bias, x = watershed, ymin = Bias_L95, ymax = Bias_U95, 
                      fill = Sig, shape = analysis), 
                  position = position_jitter(w = 0.5), 
                  size = 1.5) +
  expand_limits(x= c(-0.1, length(unique(MARSSseasmkTab$watershed)) + 1.1)) +
  scale_fill_manual(values = c("grey90", "steelblue"), name = "P < 0.05") +
  scale_shape_manual(values = c(21,22), name = "Analysis") +
  facet_wrap(vars(solute2),
             nrow = 3,
             ncol = 2) +
  ylim(-25,25) +
  ylab(expression(paste("Bias ± 95% CI (% change ", y^-1,")"))) +
  theme_bw() +
  guides(fill=guide_legend(override.aes=list(shape=21))) +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill = NA, color = NA),
        legend.text = element_text(size = 26),
        legend.title = element_text(size = 28, face = "bold"),
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 2),
        plot.margin = unit(c(t = 0.5, r = 0.5, b = 0.5, l = 0.5), "cm"),
        axis.text.y = element_text(size = 24),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(size = 24, vjust = 0.5, hjust = 1, angle = 90),
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 30))
  
  ggsave(path = "plots", file = "Fig7_MARSS_seasSensSlopeComp.png", width = 16, height = 18, units = "in")
  
  
  # Jims attempt at fig 7
  Fig7df2 <- Fig7df %>% 
    mutate(analysis = ifelse(analysis == "Sens slope", "SS", analysis)) %>% 
    pivot_wider(id_cols = c(solute,watershed, solute2), names_from = analysis, values_from = Bias:Sig) %>% 
    mutate(Sig_SSMarss = paste0(Sig_SS, "_", Sig_MARSS)) %>% 
    filter(!is.na(Sig_SS)) %>% 
    droplevels()
  
  ggplot(Fig7df2, aes(y = Bias_SS, x = Bias_MARSS, fill = Sig_SSMarss)) +
    # geom_errorbar(data = Fig7df2, aes(y = Bias_SS, x = Bias_MARSS, ymin = Bias_L95_SS, ymax = Bias_U95_SS), color = "grey") +
    # geom_errorbarh(data = Fig7df2, aes(y = Bias_SS, x = Bias_MARSS, xmin = Bias_L95_MARSS, xmax = Bias_U95_MARSS), color = "grey") +
    geom_point(shape = 21, size = 3) +
    geom_abline(intercept = 0, slope = 1) +
    facet_wrap(~ solute2, scales = "free", nrow = 3) 

## export table ----
MARSSseasmkTab_2 <- MARSSseasmkTab %>% 
  pivot_wider(id_cols = c("solute", "watershed"), names_from = analysis, values_from = Bias:Sig) %>% 
  select(solute, watershed, Bias_MARSS, Bias_L95_MARSS, Bias_U95_MARSS, Sig_MARSS,
         Bias_MKandSens, Bias_L95_MKandSens, Bias_U95_MKandSens, pval_MKandSens, Sig_MKandSens) %>% 
  arrange(solute, watershed)

write.csv(MARSSseasmkTab_2, "Tables/08_MARSS_seasSensSlops_BiasTable.csv", row.names = FALSE)
