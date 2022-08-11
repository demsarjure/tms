# libraries
library(cmdstanr)
library(ggplot2)
library(ggdist)
library(bayesplot)
library(posterior)
library(tidyverse)
library(mcmcse)
library(cowplot)

# prep -------------------------------------------------------------------------
source("load_RMT.R")
source("robust_linear_utils.R")

# model
model <- cmdstan_model("./models/linear_robust.stan")

# RMT real ---------------------------------------------------------------------
df_s_real_stim <- fit_and_compare(df_real_stim$RMT_baseline, df_real_stim$RMT_diff, "Real stim")
summary_stats(df_s_real_stim$b)

# RMT sham --------------------------------------------------------------------
df_s_sham_stim <- fit_and_compare(df_sham_stim$RMT_baseline, df_sham_stim$RMT_diff, "Sham stim")
summary_stats(df_s_sham_stim$b)

# AMT real ---------------------------------------------------------------------
df_s_real_stim <- fit_and_compare(df_real_stim$AMT_baseline, df_real_stim$AMT_diff, "Real stim")
summary_stats(df_s_real_stim$b)

# RMT sham --------------------------------------------------------------------
df_s_sham_stim <- fit_and_compare(df_sham_stim$AMT_baseline, df_sham_stim$AMT_diff, "Sham stim")
summary_stats(df_s_sham_stim$b)
