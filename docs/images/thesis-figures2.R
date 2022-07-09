############################
## Thesis Plot Generation ##
############################

library(here)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(gtable)
library(stringr)
library(forcats)
path <- paste0("~/GitHub/pragmatic-trial-sim/")

######################################
## Chapter 1: Trial Characteristics ##
######################################


## Trial size & Treatment Effect ########
TSRange <- c("N = 200", "N = 500", "N = 750", "N = 1000", "N = 1500", "N = 2000")
TSTE <- list()
for (i in 1:length(TSRange)){
  temp_res <- read.csv(paste0(path, "Trial Characteristics/Sweeping Trial Size and Treatment Effect/",
                              TSRange[i], "/OR ResultsSweeping Trial Size and Treatment Effect.csv"))
  temp_chars <- read.csv(paste0(path, "Trial Characteristics/Sweeping Trial Size and Treatment Effect/",
                                TSRange[i], "/Sim Char-Sweeping Trial Size and Treatment Effect.csv"))
  
  TSTE[[i]] <- left_join(temp_res, temp_chars, by = c("Desc", "SubDesc"))
}

TSTE_res <- bind_rows(TSTE)

TSTE_bias <- TSTE_res %>% filter(Estimate %in% c("modelITTOR", "naiveModPPOR", 
                                                 "s.OR", "naiveModPPORB") & 
                                   Assessment == "bias") %>%
  filter(n %in% c(200, 1000, 2000)) %>% 
  mutate(Estimate = recode_factor(Estimate, `modelITTOR` = "ITT", `naiveModPPOR` = "Naive PP", 
                                  `s.OR` = "SIPW Adj. PP", `naiveModPPORB` = "B Adj. PP"),
         `Trial Size` = factor(n)) %>% 
  ggplot(aes(x = theta2, y = Property)) +
  geom_point(aes(shape = `Trial Size`)) +
  #geom_errorbar(aes(ymin = Property - PropertySE, 
  #										ymax = Property + PropertySE)) +
  facet_wrap(~Estimate) +
  geom_hline(yintercept = 0) + 
  labs(x = "Log(OR) Effect of Treatment", y = "Bias") +
  ggtitle("A") +
  theme_bw()

TSTE_power <- TSTE_res %>% filter(Estimate %in% c("modelITTOR", "naiveModPPOR", 
                                                  "s.OR", "naiveModPPORB") & 
                                    Assessment == "power") %>%
  filter(n %in% c(200, 1000, 2000)) %>% 
  mutate(Estimate = recode_factor(Estimate, `modelITTOR` = "ITT", `naiveModPPOR` = "Naive PP", 
                                  `s.OR` = "SIPW Adj. PP", `naiveModPPORB` = "B Adj. PP"),
         `Trial Size` = factor(n)) %>% 
  ggplot(aes(x = theta2, y = Property)) +
  geom_point(aes(shape = `Trial Size`)) +
  geom_line(aes(shape = `Trial Size`)) +
  #geom_errorbar(aes(ymin = Property - PropertySE, 
  #										ymax = Property + PropertySE)) +
  facet_wrap(~Estimate) +
  labs(x = "Log(OR) Effect of Treatment", y = "Power") +
  ggtitle("B") +
  theme_bw()

TSTE_legend <- gtable_filter(ggplot_gtable(ggplot_build(TSTE_bias)), "guide-box")

TSTE_plot <- grid.arrange(TSTE_bias + theme(legend.position="none"), 
                          TSTE_power+ theme(legend.position="none"), 
                          TSTE_legend, widths = c(3.75, 3.75, 1))

ggsave(paste0(path, "NewSims/base/Thesis Figures/", "Trial Size and Treatment Effect", ".png"), 
       plot = TSTE_plot, device = "png", height = 4, width = 8, units = "in", dpi = 300)



TSTE_res %>% filter(Estimate %in% c("modelITTOR", "naiveModPPOR",	"s.OR", "naiveModPPORB") & 
                      Assessment %in% c("empiricalSE", "modelSE")) %>%
  filter(n %in% c(200, 1000, 2000)) %>% 
  mutate(Estimate = recode_factor(Estimate, `modelITTOR` = "ITT", `naiveModPPOR` = "Naive PP", 
                                  `s.OR` = "SIPW Adj. PP", `naiveModPPORB` = "B Adj. PP"),
         Assessment = recode_factor(Assessment, `empiricalSE` = "Empirical SE", 
                                    `modelSE` = "Model SE"),
         `Trial Size` = factor(n)) %>% 
  ggplot(aes(x = theta2, y = Property)) +
  geom_point(aes(shape = `Trial Size`)) +
  geom_line(aes(shape = `Trial Size`)) +
  facet_grid(Estimate ~ Assessment) +
  #geom_errorbar(aes(ymin = Property - PropertySE, 
  #										ymax = Property + PropertySE)) +
  labs(x = "Log(OR) Effect of Treatment", y = "Standard Error") +
  theme_bw()


ggsave(paste0(path, "NewSims/base/Thesis Figures/", "Trial Size and Treatment Effect SE", ".png"), 
       plot = last_plot(), device = "png", height = 5, width = 5, units = "in", dpi = 300)


TSTE_res %>% filter(Estimate %in% c("modelITTOR", "naiveModPPOR", 
                                    "s.OR", "naiveModPPORB") & 
                      Assessment %in% c("unbiasedCoverage", "coverage")) %>%
  filter(n %in% c(200, 1000, 2000)) %>% 
  mutate(Estimate = recode_factor(Estimate, `modelITTOR` = "ITT", `naiveModPPOR` = "Naive PP", 
                                  `s.OR` = "SIPW Adj. PP", `naiveModPPORB` = "B Adj. PP"),
         Assessment = recode_factor(Assessment, `coverage` = "Coverage",
                                    `unbiasedCoverage` = "Unbiased Coverage"),
         `Trial Size` = factor(n)) %>% 
  ggplot(aes(x = theta2, y = Property)) +
  geom_point(aes(shape = `Trial Size`)) +
  geom_line(aes(shape = `Trial Size`)) +
  facet_grid(Estimate~Assessment) +
  geom_hline(yintercept = 0.95) +
  #geom_errorbar(aes(ymin = Property - PropertySE, 
  #										ymax = Property + PropertySE)) +
  labs(x = "Log(OR) Effect of Treatment", y = "Percent") +
  theme_bw()


ggsave(paste0(path, "NewSims/base/Thesis Figures/", 
              "Trial Size and Treatment Effect Coverage", ".png"), 
       plot = last_plot(), device = "png", height = 5, width = 5, units = "in", dpi = 300)


## Event Rate ###########################
ER_res <- read.csv(paste0(path, "Trial Characteristics/Overall Event Rate/",
                          "OR ResultsOverall Event Rate.csv"))
ER_chars <- read.csv(paste0(path, "Trial Characteristics/Overall Event Rate/",
                            "Sim Char-Overall Event Rate.csv"))

ER_res <- left_join(ER_res, ER_chars, by = c("Desc", "SubDesc"))

ER_bias <- ER_res %>% filter(Estimate %in% c("modelITTOR", "naiveModPPOR", "s.OR", "naiveModPPORB") & 
                               Assessment == "bias") %>% 
  mutate(Estimate = recode_factor(Estimate, `modelITTOR` = "ITT", `naiveModPPOR` = "Naive PP", 
                                  `s.OR` = "SIPW Adj. PP", `naiveModPPORB` = "B Adj. PP")) %>% 
  ggplot(aes(x = meanEventRate, y = Property)) +
  geom_point() +
  geom_errorbar(aes(ymin = Property - PropertySE, 
                    ymax = Property + PropertySE)) +
  facet_wrap(~Estimate) +
  geom_hline(yintercept = 0) + 
  ggtitle("A") + 
  labs(x = "Average Event Rate", y = "Bias") +
  theme_bw()

ER_modSE <- ER_res %>% filter(Estimate %in% c("modelITTOR", "naiveModPPOR", "s.OR", "naiveModPPORB") & 
                                Assessment == "modelSE") %>% 
  mutate(Estimate = recode_factor(Estimate, `modelITTOR` = "ITT", `naiveModPPOR` = "Naive PP", 
                                  `s.OR` = "SIPW Adj. PP", `naiveModPPORB` = "B Adj. PP")) %>% 
  ggplot(aes(x = meanEventRate, y = Property)) +
  geom_point() +
  #geom_errorbar(aes(ymin = Property - PropertySE, 
  # 									ymax = Property + PropertySE)) +
  facet_wrap(~Estimate) +
  ggtitle("B") + 
  labs(x = "Average Event Rate", y = "Mean Model Standard Error") +
  theme_bw()

ER_both <- grid.arrange(ER_bias, ER_modSE, widths = c(1,1))

ggsave(paste0(path, "NewSims/base/Thesis Figures/", "Event Rate - Both", ".png"), 
       plot = ER_both, device = "png", height = 3.75, width = 7, units = "in", dpi = 300)


## Effect of Time on Y ##################
TonY_res <- read.csv(paste0(path, "Trial Characteristics/Strength of the Effect of Time on Y/",
                            "OR ResultsStrength of the Effect of Time on Y.csv"))
TonY_chars <- read.csv(paste0(path, "Trial Characteristics/Strength of the Effect of Time on Y/",
                              "Sim Char-Strength of the Effect of Time on Y.csv"))

TonY_res <- left_join(TonY_res, TonY_chars, by = c("Desc", "SubDesc"))

TonY_bias <- TonY_res %>% filter(Estimate %in% c("modelITTOR", "naiveModPPOR", "s.OR", "naiveModPPORB") & 
                                   Assessment == "bias") %>% 
  mutate(Estimate = recode_factor(Estimate, `modelITTOR` = "ITT", `naiveModPPOR` = "Naive PP", 
                                  `s.OR` = "SIPW Adj. PP", `naiveModPPORB` = "B Adj. PP"),
         changeEvent = 100 * (round(meanEventRate, 2) - 0.03)) %>% 
  ggplot(aes(x = changeEvent, y = Property)) +
  geom_point() +
  geom_errorbar(aes(ymin = Property - PropertySE, 
                    ymax = Property + PropertySE)) +
  facet_wrap(~Estimate) +
  scale_x_continuous(labels = function(x) paste0("+", x, "%")) +
  geom_hline(yintercept = 0) + 
  ggtitle("A") + 
  labs(x = "Change in Event Rate Due to Effect of Time", y = "Bias") +
  theme_bw()

TonY_modSE <- TonY_res %>% filter(Estimate %in% c("modelITTOR", "naiveModPPOR", "s.OR", "naiveModPPORB") & 
                                    Assessment == "modelSE") %>% 
  mutate(Estimate = recode_factor(Estimate, `modelITTOR` = "ITT", `naiveModPPOR` = "Naive PP", 
                                  `s.OR` = "SIPW Adj. PP", `naiveModPPORB` = "B Adj. PP"),
         changeEvent = 100 * (round(meanEventRate, 2) - 0.03)) %>% View()
ggplot(aes(x = changeEvent, y = Property)) +
  geom_point() +
  geom_errorbar(aes(ymin = Property - PropertySE, 
                    ymax = Property + PropertySE)) +
  scale_x_continuous(labels = function(x) paste0("+", x, "%")) +
  facet_wrap(~Estimate) +
  ggtitle("B") + 
  labs(x = "Change in Event Rate Due to Effect of Time", y = "Mean Model Standard Error") +
  theme_bw()

TonY_both <- grid.arrange(TonY_bias, TonY_modSE, widths = c(1,1))

ggsave(paste0(path, "NewSims/base/Thesis Figures/", "Effect of Time on Y - Both", ".png"), 
       plot = TonY_both, device = "png", height = 3.75, width = 7, units = "in", dpi = 300)

## Non-Adherence ########################

NA_res <- read.csv(paste0(path, "Trial Characteristics/Equal Rates of Non-Adherence/", 
                          "B Beta 2,2 Distributed/", "OR ResultsEqual Rates of Non-Adherence.csv"))
NA_chars <- read.csv(paste0(path, "Trial Characteristics/Equal Rates of Non-Adherence/",
                            "B Beta 2,2 Distributed/", "Sim Char-Equal Rates of Non-Adherence.csv"))

NA_res <- left_join(NA_res, NA_chars, by = c("Desc", "SubDesc"))

unequalRange <- c("Treated + 10 Percent Non-Adherence", "Treated + 20 Percent Non-Adherence",
                  "Treated + 30 Percent Non-Adherence",
                  "Treated - 10 Percent Non-Adherence", "Treated - 20 Percent Non-Adherence",
                  "Treated - 30 Percent Non-Adherence")
UENA <- list()
for (i in 1:length(unequalRange)){
  if ( i <= 3){
    temp_res <- read.csv(paste0(path, "Trial Characteristics/Un-Equal Rates of Non-Adherence/",
                                unequalRange[i], "/OR ResultsUn-Equal Rates of Non-Adherence.csv"))
    temp_chars <- read.csv(paste0(path, "Trial Characteristics/Un-Equal Rates of Non-Adherence/",
                                  unequalRange[i], "/Sim Char-Un-Equal Rates of Non-Adherence.csv"))
    
    UENA[[i]] <- left_join(temp_res, temp_chars, by = c("Desc", "SubDesc"))
  }
  else{
    temp_res <- read.csv(paste0(path, "Trial Characteristics/Un-Equal Rates of Non-Adherence/",
                                unequalRange[i], "/OR Results", unequalRange[i], ".csv"))
    temp_chars <- read.csv(paste0(path, "Trial Characteristics/Un-Equal Rates of Non-Adherence/",
                                  unequalRange[i], "/Sim Char-", unequalRange[i], ".csv"))
    
    UENA[[i]] <- left_join(temp_res, temp_chars, by = c("Desc", "SubDesc"))
  }
}

NA_res <- bind_rows(NA_res, bind_rows(UENA))

NA_bias <- NA_res %>% filter(Estimate %in% c("modelITTOR", "naiveModPPOR", 
                                             "s.OR", "naiveModPPORB") & 
                               Assessment == "bias" & Property < 5) %>%
  mutate(Estimate = recode_factor(Estimate, `modelITTOR` = "ITT", `naiveModPPOR` = "Naive PP", 
                                  `s.OR` = "SIPW Adj. PP", `naiveModPPORB` = "B Adj. PP"),
         `Control Non-Adherence` = meanNonAdherenceArm0,
         diffAdhere = paste(100 * round(meanNonAdherenceArm1 - meanNonAdherenceArm0, 1), "%"),
         diffAdhere = factor(diffAdhere, levels = c("30 %", "20 %", "10 %", "0 %", "-10 %", "-20 %", "-30 %"))) %>% 
  ggplot(aes(x = 100 * meanNonAdherenceArm0, y = Property)) +
  geom_point(aes(shape = diffAdhere)) +
  geom_hline(yintercept = 0) +
  #geom_errorbar(aes(ymin = Property - PropertySE, 
  #										ymax = Property + PropertySE)) +
  facet_wrap(~Estimate) +
  scale_color_viridis_d() +
  ggtitle("A") + 
  labs(x = "Non-Adherence in Control Arm (%)", y = "Bias", col = "Relative Treat. \nNon-Adherence") +
  theme_bw()

NA_modSE <- NA_res %>% filter(Estimate %in% c("modelITTOR", "naiveModPPOR", 
                                              "s.OR", "naiveModPPORB") & 
                                Assessment == "modelSE" & Property < 5) %>%
  mutate(Estimate = recode_factor(Estimate, `modelITTOR` = "ITT", `naiveModPPOR` = "Naive PP", 
                                  `s.OR` = "SIPW Adj. PP", `naiveModPPORB` = "B Adj. PP"),
         `Control Non-Adherence` = meanNonAdherenceArm0,
         diffAdhere = paste(100 * round(meanNonAdherenceArm1 - meanNonAdherenceArm0, 1), "%"),
         diffAdhere = factor(diffAdhere, levels = c("30 %", "20 %", "10 %", "0 %", "-10 %", "-20 %", "-30 %"))) %>% 
  ggplot(aes(x = 100 * meanNonAdherenceArm0, y = Property)) +
  geom_point(aes(shape = diffAdhere)) +
  #geom_errorbar(aes(ymin = Property - PropertySE, 
  #										ymax = Property + PropertySE)) +
  facet_wrap(~Estimate) +
  ggtitle("B") + 
  scale_color_viridis_d() +
  labs(x = "Non-Adherence in Control Arm (%)", y = "Average Model SE", col = "Relative Treat. \nNon-Adherence") +
  theme_bw()

NA_legend <- gtable_filter(ggplot_gtable(ggplot_build(NA_bias)), "guide-box")

NA_plot <- grid.arrange(NA_bias + theme(legend.position="none"), 
                        NA_modSE+ theme(legend.position="none"), 
                        NA_legend, widths = c(3.15, 3.15, 1))

ggsave(paste0(path, "NewSims/base/Thesis Figures/", "Non-Adherence Both", ".png"), 
       plot = NA_plot, device = "png", height = 4, width = 8, units = "in", dpi = 300)


######################################
## Chapter 3: Sparse Follow-Up      ##
######################################

## Measurement Schedule vs What's Missing #####################

MSImpRange <- c(rep("CC", 3), rep("LOCF", 3))

MSPathRange <- rep(c("A Missing", "A, L1, and L2 Missing", "L1 and L2 Missing"), 2)

MSFileRange <- c("Missing VS CC A", "Missing VS CC L1 L2 A", "Missing VS CC L1 L2",
                 "Missing VS LOCF A", "Missing VS LOCF L1 L2 A", "Missing VS LOCF L1 L2")
MS <- list()
for (i in 1:length(MSImpRange)){
  temp_res <- read.csv(paste0(path, "Missing Measurements/Varying Measurement Schedule/",
                              MSImpRange[i], "/", MSPathRange[i], "/OR Results",
                              MSFileRange[i], ".csv"))
  temp_chars <- read.csv(paste0(path, "Missing Measurements/Varying Measurement Schedule/",
                                MSImpRange[i], "/", MSPathRange[i], "/Sim Char-",
                                MSFileRange[i], ".csv"))
  
  MS[[i]] <- left_join(temp_res, temp_chars, by = c("Desc", "SubDesc"))
}

MS_res <- bind_rows(MS)

MS_res %>% filter(Estimate %in% c("ITT.OR.est", "naive.PP.est", 
                                  "s.PP.est", "naive.PP.B.est") & 
                    Assessment == "bias") %>%
  mutate(Estimate = recode_factor(Estimate, `ITT.OR.est` = "ITT", `naive.PP.est` = "Naive PP", 
                                  `s.PP.est` = "SIPW Adj. PP", `naive.PP.B.est` = "B Adj. PP"),
         SparseVars = case_when(Desc %in% c("Missing VS CC A", "Missing VS LOCF A") ~ "A",
                                Desc %in% c("Missing VS CC L1 L2", "Missing VS LOCF L1 L2") ~ "L1 and L2",
                                Desc %in% c("Missing VS CC L1 L2 A", "Missing VS LOCF L1 L2 A") ~ "L1, L2, and A"),
         ImpMeth = if_else(Desc %in% c("Missing VS CC A", "Missing VS CC L1 L2", 
                                       "Missing VS CC L1 L2 A"), "CC", "LOCF")) %>% 
  #filter(ImpMeth == "LOCF") %>% 
  ggplot(aes(x = m, y = Property)) +
  geom_point(aes(shape = ImpMeth)) +
  geom_line(aes(shape = ImpMeth)) +
  geom_hline(yintercept = 0) + 
  geom_errorbar(aes(ymin = Property - PropertySE, 
                    ymax = Property + PropertySE, shape = ImpMeth)) +
  facet_grid(Estimate~SparseVars) +
  labs(x = "Frequency of Measurements", y = "Bias", col = "Missing Data\nMethod") +
  theme_bw()

ggsave(paste0(path, "NewSims/base/Thesis Figures/", "Varying Measurement Schedule - Bias", ".png"), 
       plot = last_plot(), device = "png", height = 4.5, width = 5.75, units = "in", dpi = 300)


MS_res %>% filter(Estimate %in% c("ITT.OR.est", "naive.PP.est", 
                                  "s.PP.est", "naive.PP.B.est") & 
                    Assessment == "modelSE") %>%
  mutate(Estimate = recode_factor(Estimate, `ITT.OR.est` = "ITT", `naive.PP.est` = "Naive PP", 
                                  `s.PP.est` = "SIPW Adj. PP", `naive.PP.B.est` = "B Adj. PP"),
         SparseVars = case_when(Desc %in% c("Missing VS CC A", "Missing VS LOCF A") ~ "A",
                                Desc %in% c("Missing VS CC L1 L2", "Missing VS LOCF L1 L2") ~ "L1 and L2",
                                Desc %in% c("Missing VS CC L1 L2 A", "Missing VS LOCF L1 L2 A") ~ "L1, L2, and A"),
         ImpMeth = if_else(Desc %in% c("Missing VS CC A", "Missing VS CC L1 L2", 
                                       "Missing VS CC L1 L2 A"), "CC", "LOCF")) %>% View()
#filter(ImpMeth == "LOCF") %>% 
ggplot(aes(x = m, y = Property)) +
  geom_point(aes(shape = ImpMeth)) +
  geom_line(aes(shape = ImpMeth)) +
  geom_errorbar(aes(ymin = Property - PropertySE, 
                    ymax = Property + PropertySE, shape = ImpMeth)) +
  coord_cartesian(ylim = c (0,2)) +
  facet_grid(Estimate~SparseVars) +
  labs(x = "Frequency of Measurements", y = "Average Model SE", col = "Missing Data\nMethod") +
  theme_bw()

ggsave(paste0(path, "NewSims/base/Thesis Figures/", "Varying Measurement Schedule - Mod SE", ".png"), 
       plot = last_plot(), device = "png", height = 4.5, width = 5.75, units = "in", dpi = 300)



## Treatment Effect Sparse Follow-Up #####################

SFTEImpRange <- c(rep("CC", 3), rep("LOCF", 3))

SFTEPathRange <- rep(c("A Missing", "A, L1, and L2 Missing", "L1 and L2 Missing"), 2)

SFTEFileRange <- c("Missing ES CC A", "Missing ES CC L1 L2 A", "Missing ES CC L1 L2",
                   "Missing ES LOCF A", "Missing ES LOCF L1 L2 A", "Missing ES LOCF L1 L2")
SFTE <- list()
for (i in 1:length(SFTEImpRange)){
  temp_res <- read.csv(paste0(path, "Missing Measurements/Varying Treatment Effect/",
                              SFTEImpRange[i], "/", SFTEPathRange[i], "/OR Results",
                              SFTEFileRange[i], ".csv"))
  temp_chars <- read.csv(paste0(path, "Missing Measurements/Varying Treatment Effect/",
                                SFTEImpRange[i], "/", SFTEPathRange[i], "/Sim Char-",
                                SFTEFileRange[i], ".csv"))
  
  SFTE[[i]] <- left_join(temp_res, temp_chars, by = c("Desc", "SubDesc"))
}

SFTE_res <- bind_rows(SFTE)

SFTE_res %>% filter(Estimate %in% c("ITT.OR.est", "naive.PP.est", 
                                    "s.PP.est", "naive.PP.B.est") & 
                      Assessment == "bias") %>%
  mutate(Estimate = recode_factor(Estimate, `ITT.OR.est` = "ITT", `naive.PP.est` = "Naive PP", 
                                  `s.PP.est` = "SIPW Adj. PP", `naive.PP.B.est` = "B Adj. PP"),
         SparseVars = case_when(Desc %in% c("Missing ES CC A", "Missing ES LOCF A") ~ "A",
                                Desc %in% c("Missing ES CC L1 L2", "Missing ES LOCF L1 L2") ~ "L1 and L2",
                                Desc %in% c("Missing ES CC L1 L2 A", "Missing ES LOCF L1 L2 A") ~ "L1, L2, and A"),
         ImpMeth = if_else(Desc %in% c("Missing ES CC A", "Missing ES CC L1 L2",
                                       "Missing ES CC L1 L2 A"), "CC", "LOCF")) %>% 
  ggplot(aes(x = theta2, y = Property)) +
  geom_point(aes(shape = ImpMeth)) +
  geom_line(aes(shape = ImpMeth)) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = Property - PropertySE, 
                    ymax = Property + PropertySE, shape = ImpMeth)) +
  facet_grid(Estimate~SparseVars) +
  scale_x_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5)) +
  labs(x = "Log(OR) Effect of Treatment", y = "Bias", col = "Missing Data\nMethod") +
  theme_bw() + theme(panel.spacing = unit(1, "lines"))

ggsave(paste0(path, "NewSims/base/Thesis Figures/", "Varying Treatment Effect - Bias", ".png"), 
       plot = last_plot(), device = "png", height = 4.75, width = 6.5, units = "in", dpi = 300)

SFTE_res %>% filter(Estimate %in% c("ITT.OR.est", "naive.PP.est", 
                                    "s.PP.est", "naive.PP.B.est") & 
                      Assessment == "power") %>%
  mutate(Estimate = recode_factor(Estimate, `ITT.OR.est` = "ITT", `naive.PP.est` = "Naive PP", 
                                  `s.PP.est` = "SIPW Adj. PP", `naive.PP.B.est` = "B Adj. PP"),
         SparseVars = case_when(Desc %in% c("Missing ES CC A", "Missing ES LOCF A") ~ "A",
                                Desc %in% c("Missing ES CC L1 L2", "Missing ES LOCF L1 L2") ~ "L1 and L2",
                                Desc %in% c("Missing ES CC L1 L2 A", "Missing ES LOCF L1 L2 A") ~ "L1, L2, and A"),
         ImpMeth = if_else(Desc %in% c("Missing ES CC A", "Missing ES CC L1 L2",
                                       "Missing ES CC L1 L2 A"), "CC", "LOCF")) %>% 
  filter(SparseVars == "L1, L2, and A") %>% 
  ggplot(aes(x = theta2, y = Property)) +
  geom_point(aes(shape = ImpMeth)) +
  geom_line(aes(shape = ImpMeth)) +
  geom_hline(yintercept = 0.05) +
  geom_errorbar(aes(ymin = Property - PropertySE, 
                    ymax = Property + PropertySE, shape = ImpMeth)) +
  facet_wrap(~Estimate) +
  scale_x_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5)) +
  labs(x = "Log(OR) Effect of Treatment", y = "Power", col = "Missing Data\nMethod") +
  theme_bw() + theme(panel.spacing = unit(1, "lines"))

ggsave(paste0(path, "NewSims/base/Thesis Figures/", "Varying Treatment Effect - Power", ".png"), 
       plot = last_plot(), device = "png", height = 4.75, width = 6.5, units = "in", dpi = 300)



## Varying Non-Adherence Sparse Follow-Up #####################

SFNAImpRange <- c(rep("A Missing", 5), rep("A, L1, and L2 Missing", 5), rep("L1 and L2 Missing", 5))

SFNAPathRange <- rep(c("m = 1", "m = 6", "m = 12", "m = 18", "m = 24"), 3)

SFNAFileRange <- c(rep("MM LOCF A Varying Non-Adherence", 5), rep("MM LOCF Varying Non-Adherence", 5),
                   rep("MM LOCF L1 L2 Varying Non-Adherence", 5))
SFNA <- list()
for (i in 1:length(SFNAImpRange)){
  temp_res <- read.csv(paste0(path, "Missing Measurements/Varying Non-Adherence/LOCF/",
                              SFNAImpRange[i], "/", SFNAPathRange[i], "/OR Results",
                              SFNAFileRange[i], ".csv"))
  temp_chars <- read.csv(paste0(path, "Missing Measurements/Varying Non-Adherence/LOCF/",
                                SFNAImpRange[i], "/", SFNAPathRange[i], "/Sim Char-",
                                SFNAFileRange[i], ".csv"))
  
  SFNA[[i]] <- left_join(temp_res, temp_chars, by = c("Desc", "SubDesc"))
}

SFNA_res <- bind_rows(SFNA)

SFNA_res %>% filter(Estimate %in% c("ITT.OR.est", "naive.PP.est", 
                                    "s.PP.est", "naive.PP.B.est") & 
                      Assessment == "bias") %>%
  mutate(Estimate = recode_factor(Estimate, `ITT.OR.est` = "ITT", `naive.PP.est` = "Naive PP", 
                                  `s.PP.est` = "SIPW Adj. PP", `naive.PP.B.est` = "B Adj. PP"),
         Interval = case_when(substr(SubDesc, start = 1, stop = 6) == "Monthl" ~ '1',
                              substr(SubDesc, start = 1, stop = 6) == "m = 6 " ~ '6',
                              substr(SubDesc, start = 1, stop = 6) == "Annual" ~ '12',
                              substr(SubDesc, start = 1, stop = 6) == "m = 18" ~ '18',
                              substr(SubDesc, start = 1, stop = 6) == "m = 24" ~ '24'),
         Interval = factor(Interval, levels = c("1", "6", "12", "18", "24")),
         MissingVars = case_when(Desc == "MM LOCF A Varying Non-Adherence" ~ "A",
                                 Desc == "MM LOCF Varying Non-Adherence" ~ "A, L1, and L2 Missing",
                                 Desc == "MM LOCF L1 L2 Varying Non-Adherence" ~ "L1 and L2 Missing")) %>% 
  ggplot(aes(x = meanNonAdherenceArm0, y = Property)) +
  geom_point(aes(shape = Interval)) +
  geom_line(aes(shape = Interval)) +
  geom_hline(yintercept = 0) +	
  scale_color_viridis_d() +
  geom_errorbar(aes(ymin = Property - PropertySE, 
                    ymax = Property + PropertySE, shape = Interval)) +
  facet_grid(Estimate~MissingVars) +
  labs(x = "Non-Adherence Rate in Arms 1 & 0", y = "Bias", col = "Measurement \nFrequency") +
  theme_bw()

ggsave(paste0(path, "NewSims/base/Thesis Figures/", "Varying Non-Adherence - Bias - Faceted", ".png"), 
       plot = last_plot(), device = "png", height = 4, width = 5.25, units = "in", dpi = 300)


######################################
## Chapter 3: Modifying the DAG     ##
######################################

## B as a risk factor ##################

BRisk_res <- read.csv(paste0(path, "Modifying the DAG/B as a Risk Factor/",
                             "OR Results", "B as a Risk Factor",  ".csv"))
BRisk_chars <- read.csv(paste0(path, "Modifying the DAG/B as a Risk Factor/",
                               "/Sim Char-B as a Risk Factor", ".csv"))

BRisk_res <- left_join(BRisk_res, BRisk_chars, by = c("Desc", "SubDesc"))

BRisk_res_bias <- BRisk_res %>% filter(Estimate %in% c("modelITTOR", "naiveModPPOR", "s.OR", "naiveModPPORB") & 
                                         Assessment == "bias") %>% 
  mutate(Estimate = recode_factor(Estimate, `modelITTOR` = "ITT", `naiveModPPOR` = "Naive PP", 
                                  `s.OR` = "SIPW Adj. PP", `naiveModPPORB` = "B Adj. PP")) %>% 
  ggplot(aes(x = theta1, y = Property)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0) +	
  geom_errorbar(aes(ymin = Property - PropertySE, 
                    ymax = Property + PropertySE)) +
  facet_wrap(~Estimate) +
  labs(x = "Log(OR) Effect of B on Y", y = "Bias") +
  ggtitle("A") + 
  theme_bw()

BRisk_res_modSE <- BRisk_res %>% filter(Estimate %in% c("modelITTOR", "naiveModPPOR", "s.OR", "naiveModPPORB") & 
                                          Assessment == "modelSE") %>% 
  mutate(Estimate = recode_factor(Estimate, `modelITTOR` = "ITT", `naiveModPPOR` = "Naive PP", 
                                  `s.OR` = "SIPW Adj. PP", `naiveModPPORB` = "B Adj. PP")) %>% 
  ggplot(aes(x = theta1, y = Property)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0) +	
  geom_errorbar(aes(ymin = Property - PropertySE, 
                    ymax = Property + PropertySE)) +
  facet_wrap(~Estimate) +
  labs(x = "Log(OR) Effect of B on Y", y = "Model Standard Error") +
  ggtitle("B") + 
  theme_bw()

BRisk_res_plot <- grid.arrange(BRisk_res_bias, BRisk_res_modSE, widths = c(1,1))

ggsave(paste0(path, "NewSims/base/Thesis Figures/", "B as a Risk Factor - Both", ".png"), 
       plot = BRisk_res_plot, device = "png", height = 3.8, width = 7, units = "in", dpi = 300)

## B Unmeasured ##########################

BUnMBC_res <- read.csv(paste0(path, "Modifying the DAG/B Unmeasured/",
                              "OR Results", "B Unmeasured ",  ".csv"))
BUnMBC_chars <- read.csv(paste0(path, "Modifying the DAG/B Unmeasured/",
                                "/Sim Char-B Unmeasured ", ".csv"))

BUnMBC_res <- left_join(BUnMBC_res, BUnMBC_chars, by = c("Desc", "SubDesc"))

BUnMBC_bias <- BUnMBC_res %>% filter(Estimate %in% c("modelITTOR", "naiveModPPOR", "u.OR", "tu.OR") & 
                                       Assessment == "bias") %>% 
  mutate(Estimate = recode_factor(Estimate, `modelITTOR` = "ITT", `naiveModPPOR` = "Naive PP", 
                                  `u.OR` = "WSIPW Adj. PP", `tu.OR` = "TWSIPW Adj. PP")) %>% 
  ggplot(aes(x = theta2, y = Property)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0) +	
  geom_errorbar(aes(ymin = Property - PropertySE, 
                    ymax = Property + PropertySE)) +
  facet_wrap(~Estimate) +
  ggtitle("A") +
  labs(x = "Log(OR) Effect of Treatment", y = "Bias") +
  theme_bw()

BUnMBC_empSE <- BUnMBC_res %>% filter(Estimate %in% c("modelITTOR", "naiveModPPOR", "u.OR", "tu.OR") & 
                                        Assessment == "modelSE") %>% 
  mutate(Estimate = recode_factor(Estimate, `modelITTOR` = "ITT", `naiveModPPOR` = "Naive PP", 
                                  `u.OR` = "WSIPW Adj. PP", `tu.OR` = "TWSIPW Adj. PP")) %>% 
  ggplot(aes(x = theta2, y = Property)) +
  geom_point() +
  geom_errorbar(aes(ymin = Property - PropertySE, 
                    ymax = Property + PropertySE)) +
  facet_wrap(~Estimate) +
  ggtitle("B") +
  labs(x = "Log(OR) Effect of Treatment", y = "Average Model SE") +
  theme_bw()

BUnMBC_plot <- grid.arrange(BUnMBC_bias, BUnMBC_empSE, widths = c(1,1))

ggsave(paste0(path, "NewSims/base/Thesis Figures/", "B Unmeasured Base Case - Both", ".png"), 
       plot = BUnMBC_plot, device = "png", height = 3.8, width = 7, units = "in", dpi = 300)

## B Unmeasured Only Affects L at t0 ######################

BUnMLT_res <- read.csv(paste0(path, "Modifying the DAG/B Unmeasured Only Affects L at t0/",
                              "OR Results", "B Unmeasured Only Affects L at t0",  ".csv"))
BUnMLT_chars <- read.csv(paste0(path, "Modifying the DAG/B Unmeasured Only Affects L at t0/",
                                "/Sim Char-B Unmeasured Only Affects L at t0", ".csv"))

BUnMLT_res <- left_join(BUnMLT_res, BUnMLT_chars, by = c("Desc", "SubDesc"))

BUnMLT_bias <- BUnMLT_res %>% filter(Estimate %in% c("modelITTOR", "naiveModPPOR", "u.OR", "tu.OR") & 
                                       Assessment == "bias") %>% 
  mutate(Estimate = recode_factor(Estimate, `modelITTOR` = "ITT", `naiveModPPOR` = "Naive PP", 
                                  `u.OR` = "WSIPW Adj. PP", `tu.OR` = "TWSIPW Adj. PP")) %>% 
  ggplot(aes(x = theta2, y = Property)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0) +	
  geom_errorbar(aes(ymin = Property - PropertySE, 
                    ymax = Property + PropertySE)) +
  facet_wrap(~Estimate) +
  ggtitle("A") +
  labs(x = "Log(OR) Effect of Treatment", y = "Bias") +
  theme_bw()

BUnMLT_empSE <- BUnMLT_res %>% filter(Estimate %in% c("modelITTOR", "naiveModPPOR", "u.OR", "tu.OR") & 
                                        Assessment == "modelSE") %>% 
  mutate(Estimate = recode_factor(Estimate, `modelITTOR` = "ITT", `naiveModPPOR` = "Naive PP", 
                                  `u.OR` = "WSIPW Adj. PP", `tu.OR` = "TWSIPW Adj. PP")) %>% 
  ggplot(aes(x = theta2, y = Property)) +
  geom_point() +
  geom_errorbar(aes(ymin = Property - PropertySE, 
                    ymax = Property + PropertySE)) +
  facet_wrap(~Estimate) +
  ggtitle("B") +
  labs(x = "Log(OR) Effect of Treatment", y = "Average Model SE") +
  theme_bw()

BUnMLT_plot <- grid.arrange(BUnMLT_bias, BUnMLT_empSE, widths = c(1,1))

ggsave(paste0(path, "NewSims/base/Thesis Figures/", "B Unmeasured B Only Affect L t0 - Both", ".png"), 
       plot = BUnMLT_plot, device = "png", height = 3.8, width = 7, units = "in", dpi = 300)

# Time-varying Covariates Affect on A ###############

LonA_res <- read.csv(paste0(path, "Modifying the DAG/L Effect on Adherence/Minimal B on A/",
                            "OR Results", "L Effect on Adherence Minimal B on A",  ".csv"))
LonA_chars <- read.csv(paste0(path, "Modifying the DAG/L Effect on Adherence/Minimal B on A/",
                              "/Sim Char-L Effect on Adherence Minimal B on A", ".csv"))

LonA_res <- left_join(LonA_res, LonA_chars, by = c("Desc", "SubDesc"))

LonA_bias <- LonA_res %>% filter(Estimate %in% c("modelITTOR", "naiveModPPOR", "s.OR", "naiveModPPORB") & 
                                   Assessment == "bias") %>% 
  mutate(Estimate = recode_factor(Estimate, `modelITTOR` = "ITT", `naiveModPPOR` = "Naive PP", 
                                  `s.OR` = "SIPW Adj. PP", `naiveModPPORB` = "B Adj. PP"),
         deltaEventRate = as.factor(100 * (round(meanNonAdherenceArm0, 1) - 0.4))) %>% 
  ggplot(aes(x = deltaEventRate, y = Property)) +
  geom_point() +
  geom_errorbar(aes(ymin = Property - PropertySE, 
                    ymax = Property + PropertySE)) +
  facet_wrap(~Estimate) +
  geom_hline(yintercept = 0) +
  ggtitle("A") +
  labs(x = "Change in Non-Adherence Due to L (%)", y = "Bias") +
  theme_bw()

LonA_empSE <-LonA_res %>% filter(Estimate %in% c("modelITTOR", "naiveModPPOR", "s.OR", "naiveModPPORB") & 
                                   Assessment == "modelSE") %>% 
  mutate(Estimate = recode_factor(Estimate, `modelITTOR` = "ITT", `naiveModPPOR` = "Naive PP", 
                                  `s.OR` = "SIPW Adj. PP", `naiveModPPORB` = "B Adj. PP"),
         deltaEventRate = as.factor(100 * (round(meanNonAdherenceArm0, 1) - 0.4))) %>% 
  ggplot(aes(x = deltaEventRate, y = Property)) +
  geom_point() +
  geom_errorbar(aes(ymin = Property - PropertySE, 
                    ymax = Property + PropertySE)) +
  facet_wrap(~Estimate) +
  ggtitle("B") +
  labs(x = "Change in Non-Adherence Due to L (%)", y = "Model SE") +
  theme_bw()

LonA_plot <- grid.arrange(LonA_bias, LonA_empSE, widths = c(1,1))

ggsave(paste0(path, "NewSims/base/Thesis Figures/", "L Effect on A", ".png"), 
       plot = LonA_plot, device = "png", height = 3.8, width = 7, units = "in", dpi = 300)


# Time-varying Covariates Affect on Y ###############

LonY_res <- read.csv(paste0(path, "Modifying the DAG/Time-Varying Confounders Effecting Y/",
                            "OR Results", "Time-Varying Confounders Effecting Y",  ".csv"))
LonY_chars <- read.csv(paste0(path, "Modifying the DAG/Time-Varying Confounders Effecting Y/",
                              "/Sim Char-Time-Varying Confounders Effecting Y", ".csv"))

LonY_res <- left_join(LonY_res, LonY_chars, by = c("Desc", "SubDesc"))

LonY_bias <- LonY_res %>% filter(Estimate %in% c("modelITTOR", "naiveModPPOR", "s.OR", "naiveModPPORB") & 
                                   Assessment == "bias") %>% 
  mutate(Estimate = recode_factor(Estimate, `modelITTOR` = "ITT", `naiveModPPOR` = "Naive PP", 
                                  `s.OR` = "SIPW Adj. PP", `naiveModPPORB` = "B Adj. PP"),
         deltaEventRate = 100 * (round(meanEventRate * 2, 1)/2 - 0.3)) %>% 
  ggplot(aes(x = deltaEventRate, y = Property)) +
  geom_point() +
  geom_errorbar(aes(ymin = Property - PropertySE, 
                    ymax = Property + PropertySE)) +
  facet_wrap(~Estimate) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = seq(-20, 20, by = 10)) +
  ggtitle("A") +
  labs(x = "Change in Event Rate Due to L (%)", y = "Bias") +
  theme_bw()

LonY_empSE <- LonY_res %>% filter(Estimate %in% c("modelITTOR", "naiveModPPOR", "s.OR", "naiveModPPORB") & 
                                    Assessment == "modelSE") %>% 
  mutate(Estimate = recode_factor(Estimate, `modelITTOR` = "ITT", `naiveModPPOR` = "Naive PP", 
                                  `s.OR` = "SIPW Adj. PP", `naiveModPPORB` = "B Adj. PP"),
         deltaEventRate = (100 * (round(meanEventRate * 2, 1)/2 - 0.3))) %>% 
  ggplot(aes(x = deltaEventRate, y = Property)) +
  geom_point() +
  geom_errorbar(aes(ymin = Property - PropertySE, 
                    ymax = Property + PropertySE)) +
  facet_wrap(~Estimate) +
  scale_x_continuous(breaks = seq(-20, 20, by = 10)) +
  ggtitle("B") +
  labs(x = "Change in Event Rate due to L (%)", y = "Model SE") +
  theme_bw()

LonY_plot <- grid.arrange(LonY_bias, LonY_empSE, widths = c(1,1))

ggsave(paste0(path, "NewSims/base/Thesis Figures/", "L Effect on Y", ".png"), 
       plot = LonY_plot, device = "png", height = 3.8, width = 7, units = "in", dpi = 300)
