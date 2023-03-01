# libraries
library(readxl)
library(tidyverse)

# load the data
df <- read_excel("./data/SICI_GLMM.xlsx")

# create difference columns for each visit
df$AMT70_baseline <- 0
df$AMT70_diff <- 0
df$AMT70_diff2 <- 0
df$AMT80_baseline <- 0
df$AMT80_diff <- 0
df$AMT80_diff2 <- 0
df$AMT90_baseline <- 0
df$AMT90_diff <- 0
df$AMT90_diff2 <- 0
df$AMT100_baseline <- 0
df$AMT100_diff <- 0
df$AMT100_diff2 <- 0
df$AUC3_baseline <- 0
df$AUC3_diff <- 0
df$AUC3_diff2 <- 0

# AUC3
df$AUC3 <- 1 / 3 * (df$AMT70 + df$AMT90) + 1 / 3 * (df$AMT80 + df$AMT100) + 1 / 3 * (df$AMT90 + df$AMT100)

n_visits <- max(df$ID) * 2
for (i in 1:n_visits) {
  ix <- 1 + ((i - 1) * 3)

  # AMT70 baseline
  df[ix:(ix + 2), ]$AMT70_baseline <- df[ix, ]$AMT70

  # AMT70 differences
  df[ix + 1, ]$AMT70_diff <- df[ix + 1, ]$AMT70 - df[ix, ]$AMT70
  df[ix + 2, ]$AMT70_diff <- df[ix + 2, ]$AMT70 - df[ix, ]$AMT70
  df[ix + 2, ]$AMT70_diff2 <- df[ix + 2, ]$AMT70 - df[ix + 1, ]$AMT70

  # AMT80 baseline
  df[ix:(ix + 2), ]$AMT80_baseline <- df[ix, ]$AMT80

  # AMT80 differences
  df[ix + 1, ]$AMT80_diff <- df[ix + 1, ]$AMT80 - df[ix, ]$AMT80
  df[ix + 2, ]$AMT80_diff <- df[ix + 2, ]$AMT80 - df[ix, ]$AMT80
  df[ix + 2, ]$AMT80_diff2 <- df[ix + 2, ]$AMT80 - df[ix + 1, ]$AMT80

  # AMT90 baseline
  df[ix:(ix + 2), ]$AMT90_baseline <- df[ix, ]$AMT90

  # AMT90 differences
  df[ix + 1, ]$AMT90_diff <- df[ix + 1, ]$AMT90 - df[ix, ]$AMT90
  df[ix + 2, ]$AMT90_diff <- df[ix + 2, ]$AMT90 - df[ix, ]$AMT90
  df[ix + 2, ]$AMT90_diff2 <- df[ix + 2, ]$AMT90 - df[ix + 1, ]$AMT90

  # AMT100 baseline
  df[ix:(ix + 2), ]$AMT100_baseline <- df[ix, ]$AMT100

  # AMT100 differences
  df[ix + 1, ]$AMT100_diff <- df[ix + 1, ]$AMT100 - df[ix, ]$AMT100
  df[ix + 2, ]$AMT100_diff <- df[ix + 2, ]$AMT100 - df[ix, ]$AMT100
  df[ix + 2, ]$AMT100_diff2 <- df[ix + 2, ]$AMT100 - df[ix + 1, ]$AMT100

  # AUC3 baseline
  df[ix:(ix + 2), ]$AUC3_baseline <- df[ix, ]$AUC3

  # AUC3 differences
  df[ix + 1, ]$AUC3_diff <- df[ix + 1, ]$AUC3 - df[ix, ]$AUC3
  df[ix + 2, ]$AUC3_diff <- df[ix + 2, ]$AUC3 - df[ix, ]$AUC3
  df[ix + 2, ]$AUC3_diff2 <- df[ix + 2, ]$AUC3 - df[ix + 1, ]$AUC3
}

# remove NAs
df <- df %>% drop_na()

# split into subgroups ---------------------------------------------------------
# sham, real
df_sham <- df %>% filter(`Sham1-Real2` == 1)
df_real <- df %>% filter(`Sham1-Real2` == 2)

# pre, stim, post
df_sham_pre <- df_sham %>% filter(Time == 1)
df_sham_stim <- df_sham %>% filter(Time == 2)
df_sham_post <- df_sham %>% filter(Time == 3)
df_real_pre <- df_real %>% filter(Time == 1)
df_real_stim <- df_real %>% filter(Time == 2)
df_real_post <- df_real %>% filter(Time == 3)
