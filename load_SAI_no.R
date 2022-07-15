# libraries
library(readxl)
library(tidyverse)

# load the data
df_no <- read_excel("./data/SAI_GLMM_no.xlsx")

# create difference and response columns for each visit
df_no$ISI20_diff <- 0
df_no$ISI22_diff <- 0
df_no$ISI24_diff <- 0

# max ISI
df_no$max_ISI <- 0
df_no$max_ISI_baseline <- 0
df_no$max_ISI_diff <- 0
df_no$max_ISI_column <- ""

n_visits <- max(df_no$ID)
for (i in 1:n_visits) {
  ix <- 1 + ((i - 1) * 3)

  # ISI20
  df_no[ix + 1, ]$ISI20_diff <- df_no[ix + 1, ]$ISI20 - df_no[ix, ]$ISI20
  df_no[ix + 2, ]$ISI20_diff <- df_no[ix + 2, ]$ISI20 - df_no[ix, ]$ISI20

  # ISI22
  df_no[ix + 1, ]$ISI22_diff <- df_no[ix + 1, ]$ISI22 - df_no[ix, ]$ISI22
  df_no[ix + 2, ]$ISI22_diff <- df_no[ix + 2, ]$ISI22 - df_no[ix, ]$ISI22

  # ISI24
  df_no[ix + 1, ]$ISI24_diff <- df_no[ix + 1, ]$ISI24 - df_no[ix, ]$ISI24
  df_no[ix + 2, ]$ISI24_diff <- df_no[ix + 2, ]$ISI24 - df_no[ix, ]$ISI24

  # get baselines
  baseline_ISI20 <- df_no[ix, ]$ISI20
  baseline_ISI22 <- df_no[ix, ]$ISI22
  baseline_ISI24 <- df_no[ix, ]$ISI24

  # find max ISI
  max_real_ISI <- ""
  if (baseline_ISI20 < baseline_ISI22 && baseline_ISI20 < baseline_ISI24) {
     # set
    df_no[ix:(ix+2), ]$max_ISI <- df_no[ix:(ix+2), ]$ISI20
    df_no[ix:(ix+2), ]$max_ISI_baseline <- baseline_ISI20
    df_no[ix:(ix+2), ]$max_ISI_diff <- df_no[ix:(ix+2), ]$ISI20_diff
    df_no[ix:(ix+2), ]$max_ISI_column <- "ISI20"
  } else if (baseline_ISI22 < baseline_ISI20 && baseline_ISI22 < baseline_ISI24) {
    # set
    df_no[ix:(ix+2), ]$max_ISI <- df_no[ix:(ix+2), ]$ISI22
    df_no[ix:(ix+2), ]$max_ISI_baseline <- baseline_ISI22
    df_no[ix:(ix+2), ]$max_ISI_diff <- df_no[ix:(ix+2), ]$ISI22_diff
    df_no[ix:(ix+2), ]$max_ISI_column <- "ISI22"
  } else {
    # set
    df_no[ix:(ix+2), ]$max_ISI <- df_no[ix:(ix+2), ]$ISI24
    df_no[ix:(ix+2), ]$max_ISI_baseline <- baseline_ISI24
    df_no[ix:(ix+2), ]$max_ISI_diff <- df_no[ix:(ix+2), ]$ISI224
    df_no[ix:(ix+2), ]$max_ISI_column <- "ISI24"
  }
}


# split into subgroups ---------------------------------------------------------
# by time
df_no_pre <- df_no %>% filter(Time == 1)
df_no_stim <- df_no %>% filter(Time == 2)
df_no_post <- df_no %>% filter(Time == 3)
