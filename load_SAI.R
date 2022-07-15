# libraries
library(readxl)
library(tidyverse)

# load the data
df <- read_excel("./data/SAI_GLMM.xlsx")

# create difference and response columns for each visit
df$ISI20_diff <- 0
df$ISI22_diff <- 0
df$ISI24_diff <- 0
df$ISI20_diff2 <- 0
df$ISI22_diff2 <- 0
df$ISI24_diff2 <- 0

n_visits = max(df$ID) * 2
for (i in 1:n_visits) {
  ix = 1 + ((i - 1) * 3)
  
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

# max ISI
df$max_ISI <- 0
df$max_ISI_baseline <- 0
df$max_ISI_response <- 0 # label as non responder at first
df$max_ISI_response_sham <- 0
df$max_ISI_diff <- 0
df$max_ISI_diff2 <- 0
df$max_ISI_relative <- 0
df$max_ISI_column <- ""
df$max_ISI_match <- 0

# min_ISI
df$min_ISI <- 0
df$min_ISI_baseline <- 0
df$min_ISI_response <- 0 # label as non responder at first
df$min_ISI_response_sham <- 0
df$min_ISI_diff <- 0
df$min_ISI_diff2 <- 0
df$min_ISI_relative <- 0
df$min_ISI_column <- ""
df$min_ISI_match <- 0

n_participants = max(df$ID)
for (i in 1:n_participants) {
  # index
  ix = 1 + ((i - 1) * 6)
  
  # get min ISI
  sham_baseline <- df[ix, ]
  real_baseline <- df[ix + 3, ]

  baseline_ISI20 <- real_baseline$ISI20 + sham_baseline$ISI20
  baseline_ISI22 <- real_baseline$ISI22 + sham_baseline$ISI22
  baseline_ISI24 <- real_baseline$ISI24 + sham_baseline$ISI24

  # find max ISI
  max_real_ISI = ""
  if (baseline_ISI20 < baseline_ISI22 && baseline_ISI20 < baseline_ISI24) {
     # set
    max_real_ISI = "ISI20"
    df[ix:(ix+5), ]$max_ISI <- df[ix:(ix+5), ]$ISI20
    df[ix:(ix+2), ]$max_ISI_baseline <- sham_baseline$`ISI20-pred`
    df[(ix+3):(ix+5), ]$max_ISI_baseline <- real_baseline$`ISI20-pred`
    df[ix:(ix+5), ]$max_ISI_diff <- df[ix:(ix+5), ]$ISI20_diff
    df[ix:(ix+5), ]$max_ISI_diff2 <- df[ix:(ix+5), ]$ISI20_diff2
    df[ix:(ix+5), ]$max_ISI_column <- "ISI20"
  } else if (baseline_ISI22 < baseline_ISI20 && baseline_ISI22 < baseline_ISI24) {
    # set
    max_real_ISI = "ISI22"
    df[ix:(ix+5), ]$max_ISI <- df[ix:(ix+5), ]$ISI22
    df[ix:(ix+2), ]$max_ISI_baseline <- sham_baseline$`ISI22-pred`
    df[(ix+3):(ix+5), ]$max_ISI_baseline <- real_baseline$`ISI22-pred`
    df[ix:(ix+5), ]$max_ISI_diff <- df[ix:(ix+5), ]$ISI22_diff
    df[ix:(ix+5), ]$max_ISI_diff2 <- df[ix:(ix+5), ]$ISI22_diff2
    df[ix:(ix+5), ]$max_ISI_column <- "ISI22"
  } else {
    # set
    max_real_ISI = "ISI24"
    df[ix:(ix+5), ]$max_ISI <- df[ix:(ix+5), ]$ISI24
    df[ix:(ix+2), ]$max_ISI_baseline <- sham_baseline$`ISI24-pred`
    df[(ix+3):(ix+5), ]$max_ISI_baseline <- real_baseline$`ISI24-pred`
    df[ix:(ix+5), ]$max_ISI_diff <- df[ix:(ix+5), ]$ISI24_diff
    df[ix:(ix+5), ]$max_ISI_diff2 <- df[ix:(ix+5), ]$ISI24_diff2
    df[ix:(ix+5), ]$max_ISI_column <- "ISI24"
  }
   
  # relative
  df[ix:(ix+5), ]$max_ISI_relative <- df[ix:(ix+5), ]$max_ISI / df[ix:(ix+5), ]$max_ISI_baseline
  
  # responder if max_ISI < max_ISI_baseline at real stimulation
  if (df[ix+4, ]$max_ISI_diff < 0)
    df[ix:(ix+5), ]$max_ISI_response <- 1

  # same for sham
  if (df[ix+1, ]$max_ISI_diff < 0)
    df[ix:(ix+5), ]$max_ISI_response_sham <- 1

  # responder if max_ISI < max_ISI_baseline at real stimulation
  if (df[ix+4, ]$max_ISI_diff < 0)
    df[ix:(ix+5), ]$max_ISI_response <- 1

  # do sham and real max ISI match?
  max_sham_ISI = ""
  if (sham_baseline$ISI20 < sham_baseline$ISI22 &&
      sham_baseline$ISI20 < sham_baseline$ISI24) {
    max_sham_ISI = "ISI20"
  } else if (sham_baseline$ISI22 < sham_baseline$ISI20 &&
             sham_baseline$ISI22 < sham_baseline$ISI24) {
    max_sham_ISI = "ISI22"
  } else {
    max_sham_ISI = "ISI24"
  }
  
  # set the match flag
  if (max_real_ISI == max_sham_ISI) {
    df[ix:(ix+5), ]$max_ISI_match <- 1
  }
  
  
  # find min ISI
  min_real_ISI = ""
  if (baseline_ISI20 > baseline_ISI22 && baseline_ISI20 > baseline_ISI24) {
    # set
    min_real_ISI = "ISI20"
    df[ix:(ix+5), ]$min_ISI <- df[ix:(ix+5), ]$ISI20
    df[ix:(ix+2), ]$min_ISI_baseline <- sham_baseline$`ISI20-pred`
    df[(ix+3):(ix+5), ]$min_ISI_baseline <- real_baseline$`ISI20-pred`
    df[ix:(ix+5), ]$min_ISI_diff <- df[ix:(ix+5), ]$ISI20_diff
    df[ix:(ix+5), ]$min_ISI_diff2 <- df[ix:(ix+5), ]$ISI20_diff2
    df[ix:(ix+5), ]$min_ISI_column <- "ISI20"
  } else if (baseline_ISI22 > baseline_ISI20 && baseline_ISI22 > baseline_ISI24) {
    # set
    min_real_ISI = "ISI22"
    df[ix:(ix+5), ]$min_ISI <- df[ix:(ix+5), ]$ISI22
    df[ix:(ix+2), ]$min_ISI_baseline <- sham_baseline$`ISI22-pred`
    df[(ix+3):(ix+5), ]$min_ISI_baseline <- real_baseline$`ISI22-pred`
    df[ix:(ix+5), ]$min_ISI_diff <- df[ix:(ix+5), ]$ISI22_diff
    df[ix:(ix+5), ]$min_ISI_diff2 <- df[ix:(ix+5), ]$ISI20_diff2
    df[ix:(ix+5), ]$min_ISI_column <- "ISI22"
  } else {
    # set
    min_real_ISI = "ISI24"
    df[ix:(ix+5), ]$min_ISI <- df[ix:(ix+5), ]$ISI24
    df[ix:(ix+2), ]$min_ISI_baseline <- sham_baseline$`ISI24-pred`
    df[(ix+3):(ix+5), ]$min_ISI_baseline <- real_baseline$`ISI24-pred`
    df[ix:(ix+5), ]$min_ISI_diff <- df[ix:(ix+5), ]$ISI24_diff
    df[ix:(ix+5), ]$min_ISI_diff2 <- df[ix:(ix+5), ]$ISI20_diff2
    df[ix:(ix+5), ]$min_ISI_column <- "ISI24"
  }
  
  # relative
  df[ix:(ix+5), ]$min_ISI_relative <- df[ix:(ix+5), ]$min_ISI / df[ix:(ix+5), ]$min_ISI_baseline
  
  # responder if min_ISI < min_ISI_baseline at real stimulation
  if (df[ix+4, ]$min_ISI_diff < 0)
    df[ix:(ix+5), ]$min_ISI_response <- 1

  # same for sham
  if (df[ix+1, ]$min_ISI_diff < 0)
    df[ix:(ix+5), ]$min_ISI_response_sham <- 1

  # do sham and real min ISI match?
  min_sham_ISI = ""
  if (sham_baseline$ISI20 > sham_baseline$ISI22 &&
      sham_baseline$ISI20 > sham_baseline$ISI24) {
    min_sham_ISI = "ISI20"
  } else if (sham_baseline$ISI22 > sham_baseline$ISI20 &&
             sham_baseline$ISI22 > sham_baseline$ISI24) {
    min_sham_ISI = "ISI22"
  } else {
    min_sham_ISI = "ISI24"
  }
  
  # set the match flag
  if (min_real_ISI == min_sham_ISI) {
    df[ix:(ix+5), ]$min_ISI_match <- 1
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
df_sham_r <- df_sham %>% filter(min_ISI_response == 1)
df_sham_nr <- df_sham %>% filter(min_ISI_response == 0)
df_real_r <- df_real %>% filter(min_ISI_response == 1)
df_real_nr <- df_real %>% filter(min_ISI_response == 0)

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

df_baseline_r <- df_baseline %>% filter(min_ISI_response == 1)
df_baseline_nr <- df_baseline %>% filter(min_ISI_response == 0)
df_baseline_male <- df_baseline %>% filter(Spol == 0)
df_baseline_female <- df_baseline %>% filter(Spol == 1)

# responders
df_responders <- df %>%
  group_by(ID) %>%
  summarize(Starost = mean(Starost),
            Spol = mean(Spol),
            Responder = mean(min_ISI_response))

df_responders_male <- df_responders %>% filter(Spol == 0)
df_responders_female <- df_responders %>% filter(Spol == 1)

df_response_sai <- df %>% group_by(ID) %>%
  summarize(min_ISI=mean(min_ISI_response))
