# mann kendal and sens slopes
# JMH Nov 2022
# JMH JUN 2023

# libraries
library(tidyverse)
library(trend)

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
  facet_wrap(vars(analysis), scales = "free_y")
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

write.csv(MARSSmkTab_2, "Tables/08_MARSS_SensSlops_BiasTable.csv")

# save.image("analysis/08_MKandSensSlope_Rdat")
# load("analysis/08_MKandSensSlope_Rdat")
