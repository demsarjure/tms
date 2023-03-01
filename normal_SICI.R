# libraries
library(cmdstanr)
library(ggplot2)
library(ggdist)
library(bayesplot)
library(posterior)
library(tidyverse)
library(mcmcse)

# prep -------------------------------------------------------------------------
# load the data
source("load_SICI.R")
source("load_SICI_no.R")
source("normal_utils.R")

# model
model <- cmdstan_model("./models/cauchy.stan")

# real -------------------------------------------------------------------------
r <- fit_and_compare(df_real_stim$AMT70_diff, "(Real stim - real pre)")
r <- fit_and_compare(df_real_stim$AMT80_diff, "(Real stim - real pre)")
r <- fit_and_compare(df_real_stim$AMT90_diff, "(Real stim - real pre)")
r <- fit_and_compare(df_real_stim$AMT100_diff, "(Real stim - real pre)")

r <- fit_and_compare(df_real_post$AMT70_diff, "(Real post - real pre)")
r <- fit_and_compare(df_real_post$AMT80_diff, "(Real post - real pre)")
r <- fit_and_compare(df_real_post$AMT90_diff, "(Real post - real pre)")
r <- fit_and_compare(df_real_post$AMT100_diff, "(Real post - real pre)")

# sham -------------------------------------------------------------------------
r <- fit_and_compare(df_sham_stim$AMT70_diff, "(Sham stim - sham pre)")
r <- fit_and_compare(df_sham_stim$AMT80_diff, "(Sham stim - sham pre)")
r <- fit_and_compare(df_sham_stim$AMT90_diff, "(Sham stim - sham pre)")
r <- fit_and_compare(df_sham_stim$AMT100_diff, "(Sham stim - sham pre)")

r <- fit_and_compare(df_sham_post$AMT70_diff, "(Sham post - sham pre)")
r <- fit_and_compare(df_sham_post$AMT80_diff, "(Sham post - sham pre)")
r <- fit_and_compare(df_sham_post$AMT90_diff, "(Sham post - sham pre)")
r <- fit_and_compare(df_sham_post$AMT100_diff, "(Sham post - sham pre)")

# no -------------------------------------------------------------------------
r <- fit_and_compare(df_no_stim$AMT70_diff, "(No stim - no pre)")
r <- fit_and_compare(df_no_stim$AMT80_diff, "(No stim - no pre)")
r <- fit_and_compare(df_no_stim$AMT90_diff, "(No stim - no pre)")
r <- fit_and_compare(df_no_stim$AMT100_diff, "(No stim - no pre)")

r <- fit_and_compare(df_no_post$AMT70_diff, "(No post - no pre)")
r <- fit_and_compare(df_no_post$AMT80_diff, "(No post - no pre)")
r <- fit_and_compare(df_no_post$AMT90_diff, "(No post - no pre)")
r <- fit_and_compare(df_no_post$AMT100_diff, "(No post - no pre)")

# sham vs real -----------------------------------------------------------------
r <- fit_and_compare2(
  df_real_stim$AMT70_diff, "(Real stim - real pre)",
  df_sham_stim$AMT70_diff, "(Sham stim - sham pre)"
)
r <- fit_and_compare2(
  df_real_stim$AMT80_diff, "(Real stim - real pre)",
  df_sham_stim$AMT80_diff, "(Sham stim - sham pre)"
)
r <- fit_and_compare2(
  df_real_stim$AMT90_diff, "(Real stim - real pre)",
  df_sham_stim$AMT90_diff, "(Sham stim - sham pre)"
)
r <- fit_and_compare2(
  df_real_stim$AMT100_diff, "(Real stim - real pre)",
  df_sham_stim$AMT100_diff, "(Sham stim - sham pre)"
)

r <- fit_and_compare2(
  df_real_post$AMT70_diff, "(Real post - real pre)",
  df_sham_post$AMT70_diff, "(Sham post - sham pre)"
)
r <- fit_and_compare2(
  df_real_post$AMT80_diff, "(Real post - real pre)",
  df_sham_post$AMT80_diff, "(Sham post - sham pre)"
)
r <- fit_and_compare2(
  df_real_post$AMT90_diff, "(Real post - real pre)",
  df_sham_post$AMT90_diff, "(Sham post - sham pre)"
)
r <- fit_and_compare2(
  df_real_post$AMT100_diff, "(Real post - real pre)",
  df_sham_post$AMT100_diff, "(Sham post - sham pre)"
)

# no vs real -----------------------------------------------------------------
r <- fit_and_compare2(
  df_real_stim$AMT70_diff, "(Real stim - real pre)",
  df_no_stim$AMT70_diff, "(No stim - no pre)"
)
r <- fit_and_compare2(
  df_real_stim$AMT80_diff, "(Real stim - real pre)",
  df_no_stim$AMT80_diff, "(No stim - no pre)"
)
r <- fit_and_compare2(
  df_real_stim$AMT90_diff, "(Real stim - real pre)",
  df_no_stim$AMT90_diff, "(No stim - no pre)"
)
r <- fit_and_compare2(
  df_real_stim$AMT100_diff, "(Real stim - real pre)",
  df_no_stim$AMT100_diff, "(No stim - no pre)"
)

r <- fit_and_compare2(
  df_real_post$AMT70_diff, "(Real post - real pre)",
  df_no_post$AMT70_diff, "(No post - no pre)"
)
r <- fit_and_compare2(
  df_real_post$AMT80_diff, "(Real post - real pre)",
  df_no_post$AMT80_diff, "(No post - no pre)"
)
r <- fit_and_compare2(
  df_real_post$AMT90_diff, "(Real post - real pre)",
  df_no_post$AMT90_diff, "(No post - no pre)"
)
r <- fit_and_compare2(
  df_real_post$AMT100_diff, "(Real post - real pre)",
  df_no_post$AMT100_diff, "(No post - no pre)"
)

# no vs sham -----------------------------------------------------------------
r <- fit_and_compare2(
  df_sham_stim$AMT70_diff, "(Sham stim - sham pre)",
  df_no_stim$AMT70_diff, "(No stim - no pre)"
)
r <- fit_and_compare2(
  df_sham_stim$AMT80_diff, "(Sham stim - sham pre)",
  df_no_stim$AMT80_diff, "(No stim - no pre)"
)
r <- fit_and_compare2(
  df_sham_stim$AMT90_diff, "(Sham stim - sham pre)",
  df_no_stim$AMT90_diff, "(No stim - no pre)"
)
r <- fit_and_compare2(
  df_sham_stim$AMT100_diff, "(Sham stim - sham pre)",
  df_no_stim$AMT100_diff, "(No stim - no pre)"
)

r <- fit_and_compare2(
  df_sham_post$AMT70_diff, "(Sham post - sham pre)",
  df_no_post$AMT70_diff, "(No post - no pre)"
)
r <- fit_and_compare2(
  df_sham_post$AMT80_diff, "(Sham post - sham pre)",
  df_no_post$AMT80_diff, "(No post - no pre)"
)
r <- fit_and_compare2(
  df_sham_post$AMT90_diff, "(Sham post - sham pre)",
  df_no_post$AMT90_diff, "(No post - no pre)"
)
r <- fit_and_compare2(
  df_sham_post$AMT100_diff, "(Sham post - sham pre)",
  df_no_post$AMT100_diff, "(No post - no pre)"
)

# AUC3 -------------------------------------------------------------------------
r <- fit_and_compare(df_real_stim$AUC3_diff, "Real stim")
df_samples <- data.frame(
  mu = r$mu,
  diff = 1,
  condition = "taVNS"
)

r <- fit_and_compare(df_real_post$AUC3_diff, "Real post")
df_samples <- df_samples %>% add_row(data.frame(
  mu = r$mu,
  diff = 2,
  condition = "taVNS"
))

r <- fit_and_compare(df_sham_stim$AUC3_diff, "Sham stim")
df_samples <- df_samples %>% add_row(data.frame(
  mu = r$mu,
  diff = 1,
  condition = "sVNS"
))

r <- fit_and_compare(df_sham_post$AUC3_diff, "Sham post")
df_samples <- df_samples %>% add_row(data.frame(
  mu = r$mu,
  diff = 2,
  condition = "sVNS"
))

r <- fit_and_compare(df_no_stim$AUC3_diff, "No stim")
df_samples <- df_samples %>% add_row(data.frame(
  mu = r$mu,
  diff = 1,
  condition = "xVNS"
))

r <- fit_and_compare(df_no_post$AUC3_diff, "No post")
df_samples <- df_samples %>% add_row(data.frame(
  mu = r$mu,
  diff = 2,
  condition = "xVNS"
))

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
df_samples_sici <- df_samples

# additional comparisons
r <- fit_and_compare2(
  df_real_stim$AUC3_diff, "Real stim",
  df_sham_stim$AUC3_diff, "Sham stim"
)

r <- fit_and_compare2(
  df_real_post$AUC3_diff, "Real post",
  df_sham_post$AUC3_diff, "Sham post"
)

r <- fit_and_compare2(
  df_real_stim$AUC3_diff, "Real stim",
  df_no_stim$AUC3_diff, "No stim"
)

r <- fit_and_compare2(
  df_real_post$AUC3_diff, "Real post",
  df_no_post$AUC3_diff, "No post"
)

r <- fit_and_compare2(
  df_sham_stim$AUC3_diff, "Sham stim",
  df_no_stim$AUC3_diff, "No stim"
)

r <- fit_and_compare2(
  df_sham_post$AUC3_diff, "Sham post",
  df_no_post$AUC3_diff, "No post"
)

# pre comparison ---------------------------------------------------------------
r <- fit_and_compare2(df_real_pre$AUC3, "Real", df_sham_pre$AUC3, "Sham")
# Real < Sham: 26.65 +/- 0.9%

r <- fit_and_compare2(df_real_pre$AMT70, "Real", df_sham_pre$AMT70, "Sham")
# Real < Sham: 66.35 +/- 0.8%

r <- fit_and_compare2(df_real_pre$AMT80, "Real", df_sham_pre$AMT80, "Sham")
# Real < Sham: 91.35 +/- 0.5%

r <- fit_and_compare2(df_real_pre$AMT90, "Real", df_sham_pre$AMT90, "Sham")
# Real < Sham: 17.05 +/- 0.7%

r <- fit_and_compare2(df_real_pre$AMT100, "Real", df_sham_pre$AMT100, "Sham")
# Real < Sham: 20.4 +/- 0.8%
