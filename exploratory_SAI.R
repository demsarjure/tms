# libraries
library(ggplot2)
library(HDInterval)
library(tidyverse)


# prep -------------------------------------------------------------------------
# load the data
source("load_SAI.R")


# summary statistics -----------------------------------------------------------
summary_stats <- function(data) {
  mean_data <- round(mean(data), 2)
  hdi_data <- round(hdi(data), 2)
  
  cat(paste0(mean_data, " [", hdi_data[1], ", ", hdi_data[2], "]"))
}

# sham
summary_stats(df_sham_pre$min_ISI)
summary_stats(df_sham_stim$min_ISI)
summary_stats(df_sham_post$min_ISI)
summary_stats(df_sham_stim$min_ISI_diff)
summary_stats(df_sham_post$min_ISI_diff)
summary_stats(df_sham_post$min_ISI_diff2)
summary_stats(df_sham_stim$min_ISI_relative)
summary_stats(df_sham_post$min_ISI_relative)

# real
summary_stats(df_real_pre$min_ISI)
summary_stats(df_real_stim$min_ISI)
summary_stats(df_real_post$min_ISI)
summary_stats(df_real_stim$min_ISI_diff)
summary_stats(df_real_post$min_ISI_diff)
summary_stats(df_real_post$min_ISI_diff2)
summary_stats(df_real_stim$min_ISI_relative)
summary_stats(df_real_post$min_ISI_relative)


# plot min_ISI_diff through phases ---------------------------------------------
df_median <- df %>%
  group_by(`Sham1-Real2`, Time) %>%
  summarize(median_diff = median(min_ISI_diff))
  
ggplot(df, aes(x=Time, y=min_ISI_diff)) +
  geom_jitter(width=0.05, height=0, size=2, alpha=0.25, shape=16) +
  geom_line(data=df_median,
            aes(x = Time, y = median_diff),
            size = 1) +
  ylim(-0.75, 0.75) +
  facet_grid(. ~ `Sham1-Real2`) +
  scale_x_continuous(breaks=c(1, 2, 3), labels=c("Pre", "Stim", "Post"))


# plot min_ISI_diff through phases R vs NR -------------------------------------
df_median <- df %>%
  group_by(`Sham1-Real2`, Time, min_ISI_response) %>%
  summarize(median_diff = median(min_ISI_diff))

ggplot(df, aes(x=Time, y=min_ISI_diff)) +
  geom_jitter(width=0.05, height=0, size=2, alpha=0.25, shape=16) +
  geom_line(data=df_median,
            aes(x = Time, y = median_diff),
            size = 1) +
  ylim(-0.75, 0.75) +
  facet_grid(min_ISI_response ~ `Sham1-Real2`) +
  scale_x_continuous(breaks=c(1, 2, 3), labels=c("Pre", "Stim", "Post"))


# plot min_ISI_diff through phases per person ----------------------------------
df_sort <- df %>%
  group_by(ID) %>%
  summarize(min_baseline = max(min_ISI_baseline)) %>%
  arrange(min_baseline)

# apply ordering
df$ID <- factor(df$ID, levels = df_sort$ID)            

ggplot(df, aes(x=Time, y=min_ISI_diff, color=min_ISI_response)) +
  geom_line(size = 1) +
  facet_grid(`Sham1-Real2` ~ ID) +
  scale_x_continuous(breaks=c(1, 2, 3), labels=c("1", "2", "3")) +
  theme(legend.position = "none")


# linear models ----------------------------------------------------------------
ggplot(df %>% filter(Time != 1), aes(x=min_ISI_baseline, y=min_ISI_diff)) +
  geom_point() +
  geom_smooth(method="lm")  +
  facet_grid(Time ~ `Sham1-Real2`)
