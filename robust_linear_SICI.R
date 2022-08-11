# libraries
library(cmdstanr)
library(ggplot2)
library(ggdist)
library(bayesplot)
library(posterior)
library(tidyverse)
library(mcmcse)

# prep -------------------------------------------------------------------------
source("load_SICI.R")
source("robust_linear_utils.R")

# model
model <- cmdstan_model("./models/linear_robust.stan")

# sham and real ----------------------------------------------------------------
df_s_real_stim <- fit_and_compare(df_real_stim$AUC3_baseline, df_real_stim$AUC3_diff, "Real stim")
summary_stats(df_s_real_stim$b)

df_s_sham_stim <- fit_and_compare(df_sham_stim$AUC3_baseline, df_sham_stim$AUC3_diff, "Sham stim")
summary_stats(df_s_sham_stim$b)
