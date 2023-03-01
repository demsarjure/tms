# libraries
library(readxl)
library(tidyverse)

# load the data
df_no <- read_excel("./data/SICI_GLMM_no.xlsx")

# create difference columns for each visit
df_no$AMT70_baseline <- 0
df_no$AMT70_diff <- 0
df_no$AMT70_diff2 <- 0
df_no$AMT80_baseline <- 0
df_no$AMT80_diff <- 0
df_no$AMT80_diff2 <- 0
df_no$AMT90_baseline <- 0
df_no$AMT90_diff <- 0
df_no$AMT90_diff2 <- 0
df_no$AMT100_baseline <- 0
df_no$AMT100_diff <- 0
df_no$AMT100_diff2 <- 0
df_no$AUC3_baseline <- 0
df_no$AUC3_diff <- 0
df_no$AUC3_diff2 <- 0

# AUC3
df_no$AUC3 <- 1 / 3 * (df_no$AMT70 + df_no$AMT90) + 1 / 3 * (df_no$AMT80 + df_no$AMT100) + 1 / 3 * (df_no$AMT90 + df_no$AMT100)

n_visits <- max(df_no$ID)
for (i in 1:n_visits) {
  ix <- 1 + ((i - 1) * 3)

  # AMT70 baseline
  df_no[ix:(ix + 2), ]$AMT70_baseline <- df_no[ix, ]$AMT70

  # AMT70 differences
  df_no[ix + 1, ]$AMT70_diff <- df_no[ix + 1, ]$AMT70 - df_no[ix, ]$AMT70
  df_no[ix + 2, ]$AMT70_diff <- df_no[ix + 2, ]$AMT70 - df_no[ix, ]$AMT70
  df_no[ix + 2, ]$AMT70_diff2 <- df_no[ix + 2, ]$AMT70 - df_no[ix + 1, ]$AMT70

  # AMT80 baseline
  df_no[ix:(ix + 2), ]$AMT80_baseline <- df_no[ix, ]$AMT80

  # AMT80 differences
  df_no[ix + 1, ]$AMT80_diff <- df_no[ix + 1, ]$AMT80 - df_no[ix, ]$AMT80
  df_no[ix + 2, ]$AMT80_diff <- df_no[ix + 2, ]$AMT80 - df_no[ix, ]$AMT80
  df_no[ix + 2, ]$AMT80_diff2 <- df_no[ix + 2, ]$AMT80 - df_no[ix + 1, ]$AMT80

  # AMT90 baseline
  df_no[ix:(ix + 2), ]$AMT90_baseline <- df_no[ix, ]$AMT90

  # AMT90 differences
  df_no[ix + 1, ]$AMT90_diff <- df_no[ix + 1, ]$AMT90 - df_no[ix, ]$AMT90
  df_no[ix + 2, ]$AMT90_diff <- df_no[ix + 2, ]$AMT90 - df_no[ix, ]$AMT90
  df_no[ix + 2, ]$AMT90_diff2 <- df_no[ix + 2, ]$AMT90 - df_no[ix + 1, ]$AMT90

  # AMT100 baseline
  df_no[ix:(ix + 2), ]$AMT100_baseline <- df_no[ix, ]$AMT100

  # AMT90 differences
  df_no[ix + 1, ]$AMT100_diff <- df_no[ix + 1, ]$AMT100 - df_no[ix, ]$AMT100
  df_no[ix + 2, ]$AMT100_diff <- df_no[ix + 2, ]$AMT100 - df_no[ix, ]$AMT100
  df_no[ix + 2, ]$AMT100_diff2 <- df_no[ix + 2, ]$AMT100 - df_no[ix + 1, ]$AMT100

  # AUC3 baseline
  df_no[ix:(ix + 2), ]$AUC3_baseline <- df_no[ix, ]$AUC3

  # AUC3 differences
  df_no[ix + 1, ]$AUC3_diff <- df_no[ix + 1, ]$AUC3 - df_no[ix, ]$AUC3
  df_no[ix + 2, ]$AUC3_diff <- df_no[ix + 2, ]$AUC3 - df_no[ix, ]$AUC3
  df_no[ix + 2, ]$AUC3_diff2 <- df_no[ix + 2, ]$AUC3 - df_no[ix + 1, ]$AUC3
}

# remove NAs
df_no <- df_no %>% drop_na()

# split into subgroups ---------------------------------------------------------
# by time
df_no_pre <- df_no %>% filter(Time == 1)
df_no_stim <- df_no %>% filter(Time == 2)
df_no_post <- df_no %>% filter(Time == 3)
