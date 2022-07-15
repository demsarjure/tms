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

n_visits = max(df$ID) * 2
for (i in 1:n_visits) {
  ix = 1 + ((i - 1) * 3)
  
  # RMT baseline
  df[ix:(ix+2), ]$RMT_baseline <- df[ix, ]$RMT
  
  # RMT differences
  df[ix + 1, ]$RMT_diff <- df[ix + 1, ]$RMT - df[ix, ]$RMT
  df[ix + 2, ]$RMT_diff <- df[ix + 2, ]$RMT - df[ix, ]$RMT
  df[ix + 2, ]$RMT_diff2 <- df[ix + 2, ]$RMT - df[ix + 1, ]$RMT
 
  # AMT baseline
  df[ix:(ix+2), ]$AMT_baseline <- df[ix, ]$AMT
   
  # AMT differences
  df[ix + 1, ]$AMT_diff <- df[ix + 1, ]$AMT - df[ix, ]$AMT
  df[ix + 2, ]$AMT_diff <- df[ix + 2, ]$AMT - df[ix, ]$AMT
  df[ix + 2, ]$AMT_diff2 <- df[ix + 2, ]$AMT - df[ix + 1, ]$AMT
}

# mark responders
n_participants = max(df$ID)
for (i in 1:n_participants) {
  # index
  ix = 1 + ((i - 1) * 6)
  
  # RMT
  if (df[ix + 4, ]$RMT_diff < 0) {
    df[ix:(ix+5), ]$RMT_response <- 1
  }
  
  # AMT
  if (df[ix + 4, ]$AMT_diff < 0) {
    df[ix:(ix+5), ]$AMT_response <- 1
  }
}


# split into subgroups ---------------------------------------------------------
# by time
df_pre <- df %>% filter(Time == 1)
df_stim <- df %>% filter(Time == 2)
df_post <- df %>% filter(Time == 3)

# sham, real
df_sham <- df %>% filter(`Sham1-Real2` == 1)
df_real <- df %>% filter(`Sham1-Real2` == 2)

# male, female
df_female <- df %>% filter(Spol == 0)
df_male <- df %>% filter(Spol == 1)

# pre, stim, post
df_sham_pre <- df_sham %>% filter(Time == 1)
df_sham_stim <- df_sham %>% filter(Time == 2)
df_sham_post <- df_sham %>% filter(Time == 3)
df_real_pre <- df_real %>% filter(Time == 1)
df_real_stim <- df_real %>% filter(Time == 2)
df_real_post <- df_real %>% filter(Time == 3)

# male vs female
df_sham_male <- df_sham %>% filter(Spol == 0)
df_real_male <- df_real %>% filter(Spol == 0)
df_sham_female <- df_sham %>% filter(Spol == 1)
df_real_female <- df_real %>% filter(Spol == 1)

# by time
df_male_pre <- df_male %>% filter(Time == 1)
df_male_stim <- df_male %>% filter(Time == 2)
df_male_post <- df_male %>% filter(Time == 3)
df_female_pre <- df_female %>% filter(Time == 1)
df_female_stim <- df_female %>% filter(Time == 2)
df_female_post <- df_female %>% filter(Time == 3)

# pre, stim, post male
df_sham_male_pre <- df_sham_male %>% filter(Time == 1)
df_sham_male_stim <- df_sham_male %>% filter(Time == 2)
df_sham_male_post <- df_sham_male %>% filter(Time == 3)
df_real_male_pre <- df_real_male %>% filter(Time == 1)
df_real_male_stim <- df_real_male %>% filter(Time == 2)
df_real_male_post <- df_real_male %>% filter(Time == 3)

# pre, stim, post female
df_sham_female_pre <- df_sham_female %>% filter(Time == 1)
df_sham_female_stim <- df_sham_female %>% filter(Time == 2)
df_sham_female_post <- df_sham_female %>% filter(Time == 3)
df_real_female_pre <- df_real_female %>% filter(Time == 1)
df_real_female_stim <- df_real_female %>% filter(Time == 2)
df_real_female_post <- df_real_female %>% filter(Time == 3)

# responders vs non responders
df_sham_r <- df_sham %>% filter(RMT_response == 1)
df_sham_nr <- df_sham %>% filter(RMT_response == 0)
df_real_r <- df_real %>% filter(RMT_response == 1)
df_real_nr <- df_real %>% filter(RMT_response == 0)

df_response_rmt <- df %>% group_by(ID) %>%
  summarize(RMT=mean(RMT_response))

# pre, stim, post r
df_sham_r_pre <- df_sham_r %>% filter(Time == 1)
df_sham_r_stim <- df_sham_r %>% filter(Time == 2)
df_sham_r_post <- df_sham_r %>% filter(Time == 3)
df_real_r_pre <- df_real_r %>% filter(Time == 1)
df_real_r_stim <- df_real_r %>% filter(Time == 2)
df_real_r_post <- df_real_r %>% filter(Time == 3)

# pre, stim, post nr
df_sham_nr_pre <- df_sham_nr %>% filter(Time == 1)
df_sham_nr_stim <- df_sham_nr %>% filter(Time == 2)
df_sham_nr_post <- df_sham_nr %>% filter(Time == 3)
df_real_nr_pre <- df_real_nr %>% filter(Time == 1)
df_real_nr_stim <- df_real_nr %>% filter(Time == 2)
df_real_nr_post <- df_real_nr %>% filter(Time == 3)

# baseline
df_baseline <- df %>%
  filter(Time == 1)

df_baseline_r <- df_baseline %>% filter(RMT_response == 1)
df_baseline_nr <- df_baseline %>% filter(RMT_response == 0)
df_baseline_male <- df_baseline %>% filter(Spol == 0)
df_baseline_female <- df_baseline %>% filter(Spol == 1)