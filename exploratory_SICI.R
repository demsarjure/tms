# libraries
library(ggplot2)
library(HDInterval)
library(tidyverse)


# prep -------------------------------------------------------------------------
# load the data
source("load_SICI.R")


# summary statistics -----------------------------------------------------------
summary_stats <- function(data) {
  mean_data <- round(mean(data), 2)
  hdi_data <- round(hdi(data), 2)

  cat(paste0(mean_data, " [", hdi_data[1], ", ", hdi_data[2], "]"))
}


# sham
summary_stats(df_sham_stim$AMT70_diff)
summary_stats(df_sham_stim$AMT80_diff)
summary_stats(df_sham_stim$AMT90_diff)
summary_stats(df_sham_stim$AMT100_diff)
summary_stats(df_sham_stim$AUC3_diff)
summary_stats(df_sham_post$AMT70_diff)
summary_stats(df_sham_post$AMT80_diff)
summary_stats(df_sham_post$AMT90_diff)
summary_stats(df_sham_post$AMT100_diff)
summary_stats(df_sham_post$AUC3_diff)

# real
summary_stats(df_real_stim$AMT70_diff)
summary_stats(df_real_stim$AMT80_diff)
summary_stats(df_real_stim$AMT90_diff)
summary_stats(df_real_stim$AMT100_diff)
summary_stats(df_real_stim$AUC3_diff)
summary_stats(df_real_post$AMT70_diff)
summary_stats(df_real_post$AMT80_diff)
summary_stats(df_real_post$AMT90_diff)
summary_stats(df_real_post$AMT100_diff)
summary_stats(df_real_post$AUC3_diff)


# distributions ----------------------------------------------------------------
ggplot(df_real, aes(x=Value)) +
  geom_density(fill="skyblue", alpha=0.75, color=NA) +
  facet_grid(AMT ~ Time)

ggplot(df_sham, aes(x=Value)) +
  geom_density(fill="skyblue", alpha=0.75, color=NA) +
  facet_grid(AMT ~ Time)

ggplot(df_real, aes(x=Diff)) +
  geom_density(fill="skyblue", alpha=0.75, color=NA) +
  facet_grid(AMT ~ Time)

ggplot(df_sham, aes(x=Diff)) +
  geom_density(fill="skyblue", alpha=0.75, color=NA) +
  facet_grid(AMT ~ Time)


# plot AMT through phases ------------------------------------------------------
# median
df_median <- df_amt %>%
  group_by(Sham, Time, AMT) %>%
  summarize(median_AMT = median(Value))

# AMT
ggplot(df_amt, aes(x=AMT, y=Value)) +
  geom_jitter(width=0.05, height=0, size=2, alpha=0.1, shape=16) +
  geom_line(data=df_median, aes(x=AMT, y=median_AMT, group=1), size=1) +
  facet_grid(Sham ~ Time)

# Time
ggplot(df_amt, aes(x=Time, y=Value)) +
  geom_jitter(width=0.05, height=0, size=2, alpha=0.1, shape=16) +
  geom_line(data=df_median, aes(x=Time, y=median_AMT, group=1), size=1) +
  facet_grid(Sham ~ AMT) +
  scale_x_continuous(breaks=c(1, 2, 3), labels=c("Pre", "Stim", "Post"))


# plot Diff through phases ------------------------------------------------------
# median
df_median_diff <- df_amt %>%
  group_by(Sham, Time, AMT) %>%
  summarize(median_Diff = median(Diff))

# AMT
ggplot(df_amt, aes(x=AMT, y=Diff)) +
  geom_jitter(width=0.05, height=0, size=2, alpha=0.1, shape=16) +
  geom_line(data=df_median_diff, aes(x=AMT, y=median_Diff, group=1), size=1) +
  facet_grid(Sham ~ Time)

# Time
ggplot(df_amt, aes(x=Time, y=Diff)) +
  geom_jitter(width=0.05, height=0, size=2, alpha=0.1, shape=16) +
  geom_line(data=df_median_diff, aes(x=Time, y=median_Diff, group=1), size=1) +
  facet_grid(Sham ~ AMT)
