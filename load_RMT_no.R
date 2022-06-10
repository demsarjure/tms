# libraries
library(readxl)
library(tidyverse)

# load the data
df_no <- read_excel("./data/SAI_GLMM_NO.xlsx")

# create difference and response columns for each visit
df_no$RMT_baseline <- 0
df_no$RMT_diff <- 0
df_no$AMT_baseline <- 0
df_no$AMT_diff <- 0

n_visits <- max(df_no$ID)
for (i in 1:n_visits) {
  ix <- 1 + ((i - 1) * 3)
  
  # RMT baseline
  df_no[ix:(ix+2), ]$RMT_baseline <- df_no[ix, ]$RMT
  
  # RMT differences
  df_no[ix + 1, ]$RMT_diff <- df_no[ix + 1, ]$RMT - df_no[ix, ]$RMT
  df_no[ix + 2, ]$RMT_diff <- df_no[ix + 2, ]$RMT - df_no[ix, ]$RMT
 
  # AMT baseline
  df_no[ix:(ix+2), ]$AMT_baseline <- df_no[ix, ]$AMT
   
  # AMT differences
  df_no[ix + 1, ]$AMT_diff <- df_no[ix + 1, ]$AMT - df_no[ix, ]$AMT
  df_no[ix + 2, ]$AMT_diff <- df_no[ix + 2, ]$AMT - df_no[ix, ]$AMT
}

# split into subgroups ---------------------------------------------------------
# by time
df_no_pre <- df_no %>% filter(Time == 1)
df_no_stim <- df_no %>% filter(Time == 2)
df_no_post <- df_no %>% filter(Time == 3)
