# libraries
library(cmdstanr)
library(ggplot2)
library(ggdist)
library(bayesplot)
library(posterior)
library(tidyverse)
library(mcmcse)

# prep -------------------------------------------------------------------------
source("load_RMT.R")
source("load_RMT_no.R")
source("normal_utils.R")

# model
model <- cmdstan_model("./models/cauchy.stan")

# RMT --------------------------------------------------------------------------
r <- fit_and_compare(df_real_stim$RMT_diff, "(Real stim - real pre)")
df_samples <- data.frame(mu = r$mu,
                         diff = 1,
                         condition = "taVNS")

r <- fit_and_compare(df_real_post$RMT_diff, "(Real post - real pre)")
df_samples <- df_samples %>% add_row(data.frame(mu = r$mu,
                                     diff = 2,
                                     condition = "taVNS"))

r <- fit_and_compare(df_sham_stim$RMT_diff, "(Sham stim - sham pre)")
df_samples <- df_samples %>% add_row(data.frame(mu = r$mu,
                                     diff = 1,
                                     condition = "sVNS"))

r <- fit_and_compare(df_sham_post$RMT_diff, "(Sham post - sham pre)")
df_samples <- df_samples %>% add_row(data.frame(mu = r$mu,
                                     diff = 2,
                                     condition = "sVNS"))

r <- fit_and_compare(df_no_stim$RMT_diff, "(No stim - no pre)")
df_samples <- df_samples %>% add_row(data.frame(mu = r$mu,
                                     diff = 1,
                                     condition = "xVNS"))

r <- fit_and_compare(df_no_post$RMT_diff, "(No post - no pre)")
df_samples <- df_samples %>% add_row(data.frame(mu = r$mu,
                                     diff = 2,
                                     condition = "xVNS"))

# add dummy entries
df_samples <- df_samples %>%
  add_row(data.frame(mu = 0, diff = 0, condition = "taVNS"))
df_samples <- df_samples %>%
  add_row(data.frame(mu = 0, diff = 0, condition = "sVNS"))
df_samples <- df_samples %>%
  add_row(data.frame(mu = 0, diff = 0, condition = "xVNS"))

# set factors
df_samples$condition <-
  factor(df_samples$condition, levels = c("taVNS", "sVNS", "xVNS"))

# store
df_samples_rmt <- df_samples

# comparisons
r <- fit_and_compare2(df_real_stim$RMT_diff, "(Real stim - real pre)",
                      df_sham_stim$RMT_diff, "(Sham stim - sham pre)")
r <- fit_and_compare2(df_real_post$RMT_diff, "(Real post - real pre)",
                      df_sham_post$RMT_diff, "(Sham post - sham pre)")

r <- fit_and_compare2(df_real_stim$RMT_diff, "(Real stim - real pre)",
                      df_no_stim$RMT_diff, "(No stim - no pre)")
r <- fit_and_compare2(df_real_post$RMT_diff, "(Real post - real pre)",
                      df_no_post$RMT_diff, "(No post - no pre)")

r <- fit_and_compare2(df_sham_stim$RMT_diff, "(Sham stim - sham pre)",
                      df_no_stim$RMT_diff, "(No stim - no pre)")
r <- fit_and_compare2(df_sham_post$RMT_diff, "(Sham post - sham pre)",
                      df_no_post$RMT_diff, "(No post - no pre)")

r <- fit_and_compare2(df_real_pre$RMT, "Real", df_sham_pre$RMT, "Sham")
# Real > Sham: 26.62 +/- 0.8%

# AMT --------------------------------------------------------------------------
r <- fit_and_compare(df_real_stim$AMT_diff, "(Real stim - real pre)")
df_samples <- data.frame(mu = r$mu,
                         diff = 1,
                         condition = "taVNS")

r <- fit_and_compare(df_real_post$AMT_diff, "(Real post - real pre)")
df_samples <- df_samples %>% add_row(data.frame(mu = r$mu,
                                     diff = 2,
                                     condition = "taVNS"))

r <- fit_and_compare(df_sham_stim$AMT_diff, "(Sham stim - sham pre)")
df_samples <- df_samples %>% add_row(data.frame(mu = r$mu,
                                     diff = 1,
                                     condition = "sVNS"))

r <- fit_and_compare(df_sham_post$AMT_diff, "(Sham post - sham pre)")
df_samples <- df_samples %>% add_row(data.frame(mu = r$mu,
                                     diff = 2,
                                     condition = "sVNS"))

r <- fit_and_compare(df_no_stim$AMT_diff, "(No stim - no pre)")
df_samples <- df_samples %>% add_row(data.frame(mu = r$mu,
                                     diff = 1,
                                     condition = "xVNS"))

r <- fit_and_compare(df_no_post$AMT_diff, "(No post - no pre)")
df_samples <- df_samples %>% add_row(data.frame(mu = r$mu,
                                     diff = 2,
                                     condition = "xVNS"))

# add dummy entries
df_samples <- df_samples %>%
  add_row(data.frame(mu = 0, diff = 0, condition = "taVNS"))
df_samples <- df_samples %>%
  add_row(data.frame(mu = 0, diff = 0, condition = "sVNS"))
df_samples <- df_samples %>%
  add_row(data.frame(mu = 0, diff = 0, condition = "xVNS"))

# set factors
df_samples$condition <-
  factor(df_samples$condition, levels = c("taVNS", "sVNS", "xVNS"))

# store
df_samples_amt <- df_samples

# comparisons
r <- fit_and_compare2(df_real_stim$AMT_diff, "(Real stim - real pre)",
                      df_sham_stim$AMT_diff, "(Sham stim - sham pre)")
r <- fit_and_compare2(df_real_post$AMT_diff, "(Real post - real pre)",
                      df_sham_post$AMT_diff, "(Sham post - sham pre)")

r <- fit_and_compare2(df_real_stim$AMT_diff, "(Real stim - real pre)",
                      df_no_stim$AMT_diff, "(No stim - no pre)")
r <- fit_and_compare2(df_real_post$AMT_diff, "(Real post - real pre)",
                      df_no_post$AMT_diff, "(No post - no pre)")

r <- fit_and_compare2(df_sham_stim$AMT_diff, "(Sham stim - sham pre)",
                      df_no_stim$AMT_diff, "(No stim - no pre)")
r <- fit_and_compare2(df_sham_post$AMT_diff, "(Sham post - sham pre)",
                      df_no_post$AMT_diff, "(No post - no pre)")

r <- fit_and_compare2(df_real_pre$AMT, "Real", df_sham_pre$AMT, "Sham")
# Real > Sham: 61.98 +/- 0.8%
