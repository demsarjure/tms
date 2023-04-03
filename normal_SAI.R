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
source("load_SAI_no.R")
source("normal_utils.R")

# model
model <- cmdstan_model("./models/cauchy.stan")

# ISIs -------------------------------------------------------------------------
# real
r <- fit_and_compare(df_real_stim$ISI20_diff, "(Real stim - real pre)")
df_samples_isi20 <- data.frame(
  mu = r$mu,
  diff = 1,
  condition = "taVNS"
)

r <- fit_and_compare(df_real_stim$ISI22_diff, "(Real stim - real pre)")
df_samples_isi22 <- data.frame(
  mu = r$mu,
  diff = 1,
  condition = "taVNS"
)

r <- fit_and_compare(df_real_stim$ISI24_diff, "(Real stim - real pre)")
df_samples_isi24 <- data.frame(
  mu = r$mu,
  diff = 1,
  condition = "taVNS"
)

r <- fit_and_compare(df_real_post$ISI20_diff, "(Real post - real pre)")
df_samples_isi20 <- df_samples_isi20 %>% add_row(data.frame(
  mu = r$mu,
  diff = 2,
  condition = "taVNS"
))

r <- fit_and_compare(df_real_post$ISI22_diff, "(Real post - real pre)")
df_samples_isi22 <- df_samples_isi22 %>% add_row(data.frame(
  mu = r$mu,
  diff = 2,
  condition = "taVNS"
))

r <- fit_and_compare(df_real_post$ISI24_diff, "(Real post - real pre)")
df_samples_isi24 <- df_samples_isi24 %>% add_row(data.frame(
  mu = r$mu,
  diff = 2,
  condition = "taVNS"
))

# sham
r <- fit_and_compare(df_sham_stim$ISI20_diff, "(Sham stim - sham pre)")
df_samples_isi20 <- df_samples_isi20 %>% add_row(data.frame(
  mu = r$mu,
  diff = 1,
  condition = "sVNS"
))

r <- fit_and_compare(df_sham_stim$ISI22_diff, "(Sham stim - sham pre)")
df_samples_isi22 <- df_samples_isi22 %>% add_row(data.frame(
  mu = r$mu,
  diff = 1,
  condition = "sVNS"
))

r <- fit_and_compare(df_sham_stim$ISI24_diff, "(Sham stim - sham pre)")
df_samples_isi24 <- df_samples_isi24 %>% add_row(data.frame(
  mu = r$mu,
  diff = 1,
  condition = "sVNS"
))

r <- fit_and_compare(df_sham_post$ISI20_diff, "(Sham post - sham pre)")
df_samples_isi20 <- df_samples_isi20 %>% add_row(data.frame(
  mu = r$mu,
  diff = 2,
  condition = "sVNS"
))

r <- fit_and_compare(df_sham_post$ISI22_diff, "(Sham post - sham pre)")
df_samples_isi22 <- df_samples_isi22 %>% add_row(data.frame(
  mu = r$mu,
  diff = 2,
  condition = "sVNS"
))

r <- fit_and_compare(df_sham_post$ISI24_diff, "(Sham post - sham pre)")
df_samples_isi24 <- df_samples_isi24 %>% add_row(data.frame(
  mu = r$mu,
  diff = 2,
  condition = "sVNS"
))

# no
r <- fit_and_compare(df_no_stim$ISI20_diff, "(No stim - no pre)")
df_samples_isi20 <- df_samples_isi20 %>% add_row(data.frame(
  mu = r$mu,
  diff = 1,
  condition = "xVNS"
))


r <- fit_and_compare(df_no_stim$ISI22_diff, "(No stim - no pre)")
df_samples_isi22 <- df_samples_isi22 %>% add_row(data.frame(
  mu = r$mu,
  diff = 1,
  condition = "xVNS"
))

r <- fit_and_compare(df_no_stim$ISI24_diff, "(No stim - no pre)")
df_samples_isi24 <- df_samples_isi24 %>% add_row(data.frame(
  mu = r$mu,
  diff = 1,
  condition = "xVNS"
))

r <- fit_and_compare(df_no_post$ISI20_diff, "(No post - no pre)")
df_samples_isi20 <- df_samples_isi20 %>% add_row(data.frame(
  mu = r$mu,
  diff = 2,
  condition = "xVNS"
))

r <- fit_and_compare(df_no_post$ISI22_diff, "(No post - no pre)")
df_samples_isi22 <- df_samples_isi22 %>% add_row(data.frame(
  mu = r$mu,
  diff = 2,
  condition = "xVNS"
))

r <- fit_and_compare(df_no_post$ISI24_diff, "(No post - no pre)")
df_samples_isi24 <- df_samples_isi24 %>% add_row(data.frame(
  mu = r$mu,
  diff = 2,
  condition = "xVNS"
))

# ISI22 ------------------------------------------------------------------------
# add dummy entries
df_samples_isi20 <- df_samples_isi20 %>%
  add_row(data.frame(mu = 0, diff = 0, condition = "taVNS"))
df_samples_isi20 <- df_samples_isi20 %>%
  add_row(data.frame(mu = 0, diff = 0, condition = "sVNS"))
df_samples_isi20 <- df_samples_isi20 %>%
  add_row(data.frame(mu = 0, diff = 0, condition = "xVNS"))
df_samples_isi20$condition <-
  factor(df_samples_isi20$condition, levels = c("taVNS", "sVNS", "xVNS"))

df_samples_isi22 <- df_samples_isi22 %>%
  add_row(data.frame(mu = 0, diff = 0, condition = "taVNS"))
df_samples_isi22 <- df_samples_isi22 %>%
  add_row(data.frame(mu = 0, diff = 0, condition = "sVNS"))
df_samples_isi22 <- df_samples_isi22 %>%
  add_row(data.frame(mu = 0, diff = 0, condition = "xVNS"))
df_samples_isi22$condition <-
  factor(df_samples_isi22$condition, levels = c("taVNS", "sVNS", "xVNS"))

df_samples_isi24 <- df_samples_isi24 %>%
  add_row(data.frame(mu = 0, diff = 0, condition = "taVNS"))
df_samples_isi24 <- df_samples_isi24 %>%
  add_row(data.frame(mu = 0, diff = 0, condition = "sVNS"))
df_samples_isi24 <- df_samples_isi24 %>%
  add_row(data.frame(mu = 0, diff = 0, condition = "xVNS"))
df_samples_isi24$condition <-
  factor(df_samples_isi24$condition, levels = c("taVNS", "sVNS", "xVNS"))

# sham vs real -----------------------------------------------------------------
r <- fit_and_compare2(
  df_real_stim$ISI20_diff, "(Real stim - real pre)",
  df_sham_stim$ISI20_diff, "(Sham stim - sham pre)"
)
r <- fit_and_compare2(
  df_real_stim$ISI22_diff, "(Real stim - real pre)",
  df_sham_stim$ISI22_diff, "(Sham stim - sham pre)"
)
r <- fit_and_compare2(
  df_real_stim$ISI24_diff, "(Real stim - real pre)",
  df_sham_stim$ISI24_diff, "(Sham stim - sham pre)"
)

r <- fit_and_compare2(
  df_real_post$ISI20_diff, "(Real post - real pre)",
  df_sham_post$ISI20_diff, "(Sham post - sham pre)"
)
r <- fit_and_compare2(
  df_real_post$ISI22_diff, "(Real post - real pre)",
  df_sham_post$ISI22_diff, "(Sham post - sham pre)"
)
r <- fit_and_compare2(
  df_real_post$ISI24_diff, "(Real post - real pre)",
  df_sham_post$ISI24_diff, "(Sham post - sham pre)"
)

# no vs real -------------------------------------------------------------------
r <- fit_and_compare2(
  df_real_stim$ISI20_diff, "(Real stim - real pre)",
  df_no_stim$ISI20_diff, "(No stim - no pre)"
)
r <- fit_and_compare2(
  df_real_stim$ISI22_diff, "(Real stim - real pre)",
  df_no_stim$ISI22_diff, "(No stim - no pre)"
)
r <- fit_and_compare2(
  df_real_stim$ISI24_diff, "(Real stim - real pre)",
  df_no_stim$ISI24_diff, "(No stim - no pre)"
)

r <- fit_and_compare2(
  df_real_post$ISI20_diff, "(Real post - real pre)",
  df_no_post$ISI20_diff, "(No post - no pre)"
)
r <- fit_and_compare2(
  df_real_post$ISI22_diff, "(Real post - real pre)",
  df_no_post$ISI22_diff, "(No post - no pre)"
)
r <- fit_and_compare2(
  df_real_post$ISI24_diff, "(Real post - real pre)",
  df_no_post$ISI24_diff, "(No post - no pre)"
)

# no vs sham -------------------------------------------------------------------
r <- fit_and_compare2(
  df_sham_stim$ISI20_diff, "(Sham stim - sham pre)",
  df_no_stim$ISI20_diff, "(No stim - no pre)"
)
r <- fit_and_compare2(
  df_sham_stim$ISI22_diff, "(Sham stim - sham pre)",
  df_no_stim$ISI22_diff, "(No stim - no pre)"
)
r <- fit_and_compare2(
  df_sham_stim$ISI24_diff, "(Sham stim - sham pre)",
  df_no_stim$ISI24_diff, "(No stim - no pre)"
)

r <- fit_and_compare2(
  df_sham_post$ISI20_diff, "(Sham post - sham pre)",
  df_no_post$ISI20_diff, "(No post - no pre)"
)
r <- fit_and_compare2(
  df_sham_post$ISI22_diff, "(Sham post - sham pre)",
  df_no_post$ISI22_diff, "(No post - no pre)"
)
r <- fit_and_compare2(
  df_sham_post$ISI24_diff, "(Sham post - sham pre)",
  df_no_post$ISI24_diff, "(No post - no pre)"
)

# pre comparison ---------------------------------------------------------------
r <- fit_and_compare2(df_real_pre$ISI20, "Real", df_sham_pre$ISI20, "Sham")
# Real > Sham: 5.7 +/- 0.4%

r <- fit_and_compare2(df_real_pre$ISI22, "Real", df_sham_pre$ISI22, "Sham")
# Real > Sham: 31.8 +/- 0.8%

r <- fit_and_compare2(df_real_pre$ISI24, "Real", df_sham_pre$ISI24, "Sham")
# Real > Sham: 22.73 +/- 0.7%
