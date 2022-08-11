# libraries
library(cmdstanr)
library(ggplot2)
library(ggdist)
library(bayesplot)
library(posterior)
library(tidyverse)
library(mcmcse)

# prep -------------------------------------------------------------------------
source("load_SAI.R")
source("robust_linear_utils.R")

# model
model <- cmdstan_model("./models/linear.stan")

# sham and real ----------------------------------------------------------------
df_s_real_stim <- fit_and_compare(df_real_stim$min_ISI_baseline, df_real_stim$max_ISI_diff, "Real stim")
summary_stats(df_s_real_stim$b)

df_s_sham_stim <- fit_and_compare(df_sham_stim$min_ISI_baseline, df_sham_stim$max_ISI_diff, "Sham stim")
summary_stats(df_s_sham_stim$b)
