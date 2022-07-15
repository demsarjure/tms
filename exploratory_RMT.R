# libraries
library(ggplot2)
library(HDInterval)
library(tidyverse)


# prep -------------------------------------------------------------------------
# load the data
source("load_RMT.R")


# summary statistics -----------------------------------------------------------
summary_stats <- function(data) {
  mean_data <- round(mean(data), 2)
  q025 <- round(quantile(data, 0.025), 2)
  q975 <- round(quantile(data, 0.975), 2)

  cat(paste0(mean_data, " [", q025, ", ", q975, "]"))
}


# sham RMT
summary_stats(df_sham_stim$RMT_diff)
summary_stats(df_sham_post$RMT_diff)

# real RMT
summary_stats(df_real_stim$RMT_diff)
summary_stats(df_real_post$RMT_diff)

# sham AMT
summary_stats(df_sham_stim$AMT_diff)
summary_stats(df_sham_post$AMT_diff)

# real AMT
summary_stats(df_real_stim$AMT_diff)
summary_stats(df_real_post$AMT_diff)


# plot RMT_diff through phases ---------------------------------------------
df_mean <- df %>%
  group_by(`Sham1-Real2`, Time) %>%
  summarize(mean_diff = mean(RMT_diff))
  
ggplot(df, aes(x=Time, y=RMT_diff)) +
  geom_jitter(width=0.05, height=0, size=2, alpha=0.25, shape=16) +
  geom_line(data=df_mean,
            aes(x = Time, y = mean_diff),
            size = 1) +
  facet_grid(. ~ `Sham1-Real2`) +
  scale_x_continuous(breaks=c(1, 2, 3), labels=c("Pre", "Stim", "Post"))


# plot RMT_diff through phases R vs NR -------------------------------------
df_mean <- df %>%
  group_by(`Sham1-Real2`, Time, RMT_response) %>%
  summarize(mean_diff = mean(RMT_diff))

ggplot(df, aes(x=Time, y=RMT_diff)) +
  geom_jitter(width=0.05, height=0, size=2, alpha=0.25, shape=16) +
  geom_line(data=df_mean,
            aes(x = Time, y = mean_diff),
            size = 1) +
  facet_grid(RMT_response ~ `Sham1-Real2`) +
  scale_x_continuous(breaks=c(1, 2, 3), labels=c("Pre", "Stim", "Post"))


# plot RMT_diff through phases per person ----------------------------------
df_sort <- df %>%
  group_by(ID) %>%
  summarize(min_baseline = max(RMT_baseline)) %>%
  arrange(min_baseline)

# apply ordering
df$ID <- factor(df$ID, levels = df_sort$ID)            

ggplot(df, aes(x=Time, y=RMT_diff, color=RMT_response)) +
  geom_line(size = 1) +
  facet_grid(`Sham1-Real2` ~ ID) +
  scale_x_continuous(breaks=c(1, 2, 3), labels=c("1", "2", "3")) +
  theme(legend.position = "none")


# linear models ----------------------------------------------------------------
ggplot(df %>% filter(Time != 1), aes(x=RMT_baseline, y=RMT_diff)) +
  geom_point() +
  geom_smooth(method="lm")  +
  facet_grid(Time ~ `Sham1-Real2`)
