# libraries
library(readxl)
library(tidyverse)

# load the data
df <- read_excel("./data/SICI_GLMM.xlsx")

# create difference and response columns for each visit
df$AMT70_baseline <- 0
df$AMT70_diff <- 0
df$AMT70_diff2 <- 0
df$AMT70_response <- 0
df$AMT80_baseline <- 0
df$AMT80_diff <- 0
df$AMT80_diff2 <- 0
df$AMT80_response <- 0
df$AMT90_baseline <- 0
df$AMT90_diff <- 0
df$AMT90_diff2 <- 0
df$AMT90_response <- 0
df$AMT100_baseline <- 0
df$AMT100_diff <- 0
df$AMT100_diff2 <- 0
df$AMT100_response <- 0

n_visits = max(df$ID) * 2
for (i in 1:n_visits) {
  ix = 1 + ((i - 1) * 3)
  
  # AMT70 baseline
  df[ix:(ix+2), ]$AMT70_baseline <- df[ix, ]$AMT70
  
  # AMT70 differences
  df[ix + 1, ]$AMT70_diff <- df[ix + 1, ]$AMT70 - df[ix, ]$AMT70
  df[ix + 2, ]$AMT70_diff <- df[ix + 2, ]$AMT70 - df[ix, ]$AMT70
  df[ix + 2, ]$AMT70_diff2 <- df[ix + 2, ]$AMT70 - df[ix + 1, ]$AMT70
  
  # AMT80 baseline
  df[ix:(ix+2), ]$AMT80_baseline <- df[ix, ]$AMT80
  
  # AMT80 differences
  df[ix + 1, ]$AMT80_diff <- df[ix + 1, ]$AMT80 - df[ix, ]$AMT80
  df[ix + 2, ]$AMT80_diff <- df[ix + 2, ]$AMT80 - df[ix, ]$AMT80
  df[ix + 2, ]$AMT80_diff2 <- df[ix + 2, ]$AMT80 - df[ix + 1, ]$AMT80
  
  # AMT90 baseline
  df[ix:(ix+2), ]$AMT90_baseline <- df[ix, ]$AMT90
  
  # AMT90 differences
  df[ix + 1, ]$AMT90_diff <- df[ix + 1, ]$AMT90 - df[ix, ]$AMT90
  df[ix + 2, ]$AMT90_diff <- df[ix + 2, ]$AMT90 - df[ix, ]$AMT90
  df[ix + 2, ]$AMT90_diff2 <- df[ix + 2, ]$AMT90 - df[ix + 1, ]$AMT90
 
  # AMT100 baseline
  df[ix:(ix+2), ]$AMT100_baseline <- df[ix, ]$AMT100
  
  # AMT90 differences
  df[ix + 1, ]$AMT100_diff <- df[ix + 1, ]$AMT100 - df[ix, ]$AMT100
  df[ix + 2, ]$AMT100_diff <- df[ix + 2, ]$AMT100 - df[ix, ]$AMT100
  df[ix + 2, ]$AMT100_diff2 <- df[ix + 2, ]$AMT100 - df[ix + 1, ]$AMT100
}

# mark responders
n_participants = max(df$ID)
for (i in 1:n_participants) {
  # index
  ix = 1 + ((i - 1) * 6)
  
  # AMT70
  if (df[ix + 4, ]$AMT70_diff < 0) {
    df[ix:(ix+5), ]$AMT70_response <- 1
  }
  
  # AMT80
  if (df[ix + 4, ]$AMT80_diff < 0) {
    df[ix:(ix+5), ]$AMT80_response <- 1
  }
  
  # AMT90
  if (df[ix + 4, ]$AMT90_diff < 0) {
    df[ix:(ix+5), ]$AMT90_response <- 1
  }
  
  # AMT1000
  if (df[ix + 4, ]$AMT100_diff < 0) {
    df[ix:(ix+5), ]$AMT100_response <- 1
  }
}

# remove NAs
df <- df %>% drop_na()

# AUC
df$AUC <- df$AMT70 + df$AMT80 + df$AMT90 + df$AMT100
df$AUC_diff <- df$AMT70_diff + df$AMT80_diff + df$AMT90_diff + df$AMT100_diff

# speacilised data frame for this analysis -------------------------------------
df_amt <- data.frame(AMT=character(),
                     Sham=numeric(),
                     Time=numeric(),
                     Value=numeric(),
                     Diff=numeric(),
                     Diff2=numeric(),
                     Baseline=numeric(),
                     Response=numeric())

df_amt <- df_amt %>% add_row(data.frame(AMT="AMT70",
                                        Sham=df$`Sham1-Real2`,
                                        Time=df$Time,
                                        Value=df$AMT70,
                                        Diff=df$AMT70_diff,
                                        Diff2=df$AMT70_diff2,
                                        Baseline=df$AMT70_baseline,
                                        Response=df$AMT70_response))
                             

df_amt <- df_amt %>% add_row(data.frame(AMT="AMT80",
                                        Sham=df$`Sham1-Real2`,
                                        Time=df$Time,
                                        Value=df$AMT80,
                                        Diff=df$AMT80_diff,
                                        Diff2=df$AMT70_diff2,
                                        Baseline=df$AMT80_baseline,
                                        Response=df$AMT80_response))

df_amt <- df_amt %>% add_row(data.frame(AMT="AMT90",
                                        Sham=df$`Sham1-Real2`,
                                        Time=df$Time,
                                        Value=df$AMT90,
                                        Diff=df$AMT90_diff,
                                        Diff2=df$AMT90_diff2,
                                        Baseline=df$AMT90_baseline,
                                        Response=df$AMT90_response))                         

df_amt <- df_amt %>% add_row(data.frame(AMT="AMT100",
                                        Sham=df$`Sham1-Real2`,
                                        Time=df$Time,
                                        Value=df$AMT100,
                                        Diff=df$AMT100_diff,
                                        Diff2=df$AMT100_diff2,
                                        Baseline=df$AMT100_baseline,
                                        Response=df$AMT100_response))
# as factor and levels
df_amt$AMT <- factor(df_amt$AMT, levels = c("AMT70", "AMT80", "AMT90", "AMT100"))


# split into subgroups ---------------------------------------------------------
# by time
df_pre <- df %>% filter(Time == 1)
df_stim <- df %>% filter(Time == 2)
df_post <- df %>% filter(Time == 3)

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


# responders -------------------------------------------------------------------
df_amt70_r <- df %>% filter(AMT70_response == 1) %>%
  group_by(ID) %>%
  summarize(Baseline=mean(AMT70_baseline))
df_amt70_nr <- df %>% filter(AMT70_response == 0) %>%
  group_by(ID) %>%
  summarize(Baseline=mean(AMT70_baseline))

df_amt80_r <- df %>% filter(AMT80_response == 1) %>%
  group_by(ID) %>%
  summarize(Baseline=mean(AMT80_baseline))
df_amt80_nr <- df %>% filter(AMT80_response == 0) %>%
  group_by(ID) %>%
  summarize(Baseline=mean(AMT80_baseline))

df_amt90_r <- df %>% filter(AMT90_response == 1) %>%
  group_by(ID) %>%
  summarize(Baseline=mean(AMT90_baseline))
df_amt90_nr <- df %>% filter(AMT90_response == 0) %>%
  group_by(ID) %>%
  summarize(Baseline=mean(AMT90_baseline))

df_amt100_r <- df %>% filter(AMT100_response == 1) %>%
  group_by(ID) %>%
  summarize(Baseline=mean(AMT100_baseline))
df_amt100_nr <- df %>% filter(AMT100_response == 0) %>%
  group_by(ID) %>%
  summarize(Baseline=mean(AMT100_baseline))

df_response_sici <- df %>% group_by(ID) %>%
  summarize(AMT70=mean(AMT70_response),
            AMT80=mean(AMT80_response),
            AMT90=mean(AMT90_response),
            AMT100=mean(AMT100_response))
