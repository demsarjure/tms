# libraries
library(readxl)
library(tidyverse)

# load the data
df <- read_excel("./data/RMT_GLMM.xlsx")

# create difference and response columns for each visit
df$RMT_baseline <- 0
df$RMT_diff <- 0
df$RMT_diff2 <- 0
df$RMT_response <- 0
df$AMT_baseline <- 0
df$AMT_diff <- 0
df$AMT_diff2 <- 0
df$AMT_response <- 0

n_visits <- max(df$ID) * 2
for (i in 1:n_visits) {
  ix <- 1 + ((i - 1) * 3)

  # RMT baseline
  df[ix:(ix+2), ]$RMT_baseline <- df[ix, ]$RMT

  # RMT differences
  df[ix + 1, ]$RMT_diff <- df[ix + 1, ]$RMT - df[ix, ]$RMT
  df[ix + 2, ]$RMT_diff <- df[ix + 2, ]$RMT - df[ix, ]$RMT
  df[ix + 2, ]$RMT_diff2 <- df[ix + 2, ]$RMT - df[ix + 1, ]$RMT

  # AMT baseline
  df[ix:(ix + 2), ]$AMT_baseline <- df[ix, ]$AMT

  # AMT differences
  df[ix + 1, ]$AMT_diff <- df[ix + 1, ]$AMT - df[ix, ]$AMT
  df[ix + 2, ]$AMT_diff <- df[ix + 2, ]$AMT - df[ix, ]$AMT
  df[ix + 2, ]$AMT_diff2 <- df[ix + 2, ]$AMT - df[ix + 1, ]$AMT
}

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
