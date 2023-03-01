# libraries
library(readxl)
library(tidyverse)

# load the data
df <- read_excel("./data/SAI_GLMM.xlsx")

# create difference columns for each visit
df$ISI20_diff <- 0
df$ISI22_diff <- 0
df$ISI24_diff <- 0
df$ISI20_diff2 <- 0
df$ISI22_diff2 <- 0
df$ISI24_diff2 <- 0

n_visits <- max(df$ID) * 2
for (i in 1:n_visits) {
  ix <- 1 + ((i - 1) * 3)

  # ISI20
  df[ix + 1, ]$ISI20_diff <- df[ix + 1, ]$ISI20 - df[ix, ]$ISI20
  df[ix + 2, ]$ISI20_diff <- df[ix + 2, ]$ISI20 - df[ix, ]$ISI20
  df[ix + 2, ]$ISI20_diff2 <- df[ix + 2, ]$ISI20 - df[ix + 1, ]$ISI20

  # ISI22
  df[ix + 1, ]$ISI22_diff <- df[ix + 1, ]$ISI22 - df[ix, ]$ISI22
  df[ix + 2, ]$ISI22_diff <- df[ix + 2, ]$ISI22 - df[ix, ]$ISI22
  df[ix + 2, ]$ISI22_diff2 <- df[ix + 2, ]$ISI22 - df[ix + 1, ]$ISI22

  # ISI24
  df[ix + 1, ]$ISI24_diff <- df[ix + 1, ]$ISI24 - df[ix, ]$ISI24
  df[ix + 2, ]$ISI24_diff <- df[ix + 2, ]$ISI24 - df[ix, ]$ISI24
  df[ix + 2, ]$ISI24_diff2 <- df[ix + 2, ]$ISI24 - df[ix + 1, ]$ISI24
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
