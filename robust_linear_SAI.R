# libraries
library(cmdstanr)
library(ggplot2)
library(ggdist)
library(bayesplot)
library(posterior)
library(tidyverse)
library(mcmcse)


# prep -------------------------------------------------------------------------
# load the data
source("load_SAI.R")

# model
model <- cmdstan_model("./models/linear.stan")


# fit and compare function -----------------------------------------------------
fit_and_compare <- function(x, y, name){
  stan_data <- list(n=length(x), x=x, y=y)

  # fit 1
  fit <- model$sample(
    data = stan_data,
    parallel_chains = 4,
    refresh = 0
  )

  # traceplot
  mcmc_trace(fit$draws())

  # summary
  fit$summary()

  # extract
  df_samples <- as_draws_df(fit$draws())

  # compare
  smaller <- mcse(df_samples$b < 0)
  bigger <- mcse(df_samples$b > 0) 

  cat("\n----------------------------------------\n")
  cat(name, "\n")
  cat("\tbeta > 0:", bigger[[1]], "+/-", bigger[[2]], "\n")
  cat("\tbeta < 0:", smaller[[1]], "+/-", smaller[[2]])
  cat("\n----------------------------------------\n")

  return(df_samples)
}


# summary stats ----------------------------------------------------------------
summary_stats <- function(data) {
  mean_data <- round(mean(data), 2)
  q025 <- round(quantile(data, 0.025), 2)
  q975 <- round(quantile(data, 0.975), 2)

  cat(paste0(mean_data, " [", q025, ", ", q975, "]"))
}


# plot fit ---------------------------------------------------------------------
plot_fit <- function(samples, data, n = 25) {
  # generate n distributions
  df_n <- slice_sample(samples, n = n)
  x <-
    seq(min(data$current0) - 500, max(data$current0) + 500, length.out = 1000)

  # data frame for storing generated data
  df_generated <- data.frame(x = numeric(), y = numeric(), id = numeric())
  for (i in 1:n) {
    # extract values
    beta <- df_n$b[i]
    alpha <- df_n$a[i]

    # calculate probability
    y <- alpha + beta * x

    # add to lines
    temp <- data.frame(x = x, y = y, id = i)
    df_generated <- rbind(df_generated, temp)
  }

  # fix x axis
  mean_c0 <- mean(data$`Jakost toka`)
  df_generated$x <- df_generated$x + mean_c0
  data$current0 <- data$current0 + mean_c0

  # plot
  g <- ggplot() +
    geom_point(data = data, aes(x = current0, y = max_ISI_diff),
                 fill = "skyblue", alpha = 0.75, shape = 16) +
    geom_line(data = df_generated,
              aes(x = x, y = y, group = id),
              alpha = 0.1,
              size = 1)

  # return
  return(g)
}


# sham and real ----------------------------------------------------------------
df_s_real_stim <- fit_and_compare(df_real_stim$min_ISI_baseline, df_real_stim$max_ISI_diff, "Real stim")
summary_stats(df_s_real_stim$b)

df_s_sham_stim <- fit_and_compare(df_sham_stim$min_ISI_baseline, df_sham_stim$max_ISI_diff, "Sham stim")
summary_stats(df_s_sham_stim$b)


# real current -----------------------------------------------------------------
# zero center x
df_real_stim$current0 <-
  df_real_stim$`Jakost toka` - mean(df_real_stim$`Jakost toka`)
df_real_current <- fit_and_compare(df_real_stim$current0, df_real_stim$max_ISI_diff, "Real stim")

plot_fit(df_real_current, df_real_stim)

ggsave("linear2_max_ISI_real.png",
       width = 1920,
       height = 960,
       dpi = 200,
       units = "px")


# sham current -----------------------------------------------------------------
# zero center x
df_sham_stim <- df_sham_stim %>% drop_na()
df_sham_stim$current0 <-
  df_sham_stim$`Jakost toka` - mean(df_sham_stim$`Jakost toka`)
df_sham_current <- fit_and_compare(df_sham_stim$current0, df_sham_stim$max_ISI_diff, "Real stim")

plot_fit(df_sham_current, df_sham_stim)

ggsave("linear2_max_ISI_sham.png",
       width = 1920,
       height = 960,
       dpi = 200,
       units = "px")
