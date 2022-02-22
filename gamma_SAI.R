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
model <- cmdstan_model("./models/gamma.stan")


# fit and compare function -----------------------------------------------------
fit_and_compare <- function(data1, name1, data2, name2) {
  stan_data_1 <- list(n=length(data1), y=data1)
  stan_data_2 <- list(n=length(data2), y=data2)
  
  # fit 1
  fit_1 <- model$sample(
    data = stan_data_1,
    parallel_chains = 4,
    refresh = 0
  )
  
  # traceplot
  mcmc_trace(fit_1$draws())
  
  # summary
  fit_1$summary()
  
  # extract
  df_samples_1 <- as_draws_df(fit_1$draws())
  
  # fit 2
  fit_2 <- model$sample(
    data = stan_data_2,
    parallel_chains = 4,
    refresh = 0
  )
  
  # traceplot
  mcmc_trace(fit_2$draws())
  
  # summary
  fit_2$summary()
  
  # extract
  df_samples_2 <- as_draws_df(fit_2$draws())
  
  # compare
  bigger <- mcse(df_samples_1$mean_isi > df_samples_2$mean_isi)
  smaller <- mcse(df_samples_1$mean_isi < df_samples_2$mean_isi)
  
  cat("\n----------------------------------------\n")
  cat(name1 , ">", name2, ":", bigger[[1]], "+/-", bigger[[2]], "\n")
  cat(name1 , "<", name2, ":", smaller[[1]], "+/-", smaller[[2]])
  cat("\n----------------------------------------\n")
  
  return(list(samples_1=df_samples_1, samples_2=df_samples_2)) 
}


# plot fit ---------------------------------------------------------------------
plot_fit <- function(samples, data, x_column, min_x = 0, max_x = 2, n = 20) {
  # generate n distributions
  df_n <- sample_n(samples, n)
  x <- seq(min_x, max_x, length.out=1000)
  
  # data frame for storing generated data
  df_generated <- data.frame(x=numeric(), y=numeric(), id=numeric())
  for (i in 1:n) {
    y <- dgamma(x, df_n$alpha[i], df_n$beta[i])
    
    # bind
    df_generated <- rbind(df_generated,
                          data.frame(x=x, y=y, id=i))
  }
  
  # plot
  g <- ggplot() +
    geom_density(data=data, aes_string(x=x_column),
                 fill="skyblue", alpha=0.75, color=NA) +
    geom_line(data=df_generated,
              aes(x=x, y=y, group=id), alpha=0.1, size=1) +
    xlim(min_x, max_x)
  
  # return
  return(g)
}


# real vs sham -----------------------------------------------------------------
# all
r <- fit_and_compare(df_real$min_ISI, "Real", df_sham$min_ISI, "Sham")
r <- fit_and_compare(df_real_pre$min_ISI, "Real pre", df_sham_pre$min_ISI, "Sham pre")
r <- fit_and_compare(df_real_stim$min_ISI, "Real stim", df_sham_stim$min_ISI, "Sham stim")
r <- fit_and_compare(df_real_post$min_ISI, "Real post", df_sham_post$min_ISI, "Sham post")

# by time
# real
r <- fit_and_compare(df_real_stim$min_ISI, "Real stim", df_real_pre$min_ISI, "Real pre")
r <- fit_and_compare(df_real_post$min_ISI, "Real post", df_real_pre$min_ISI, "Real pre")
r <- fit_and_compare(df_real_post$min_ISI, "Real post", df_real_stim$min_ISI, "Real stim")

# plot fit
plot_fit(r[[1]], df_sham_stim, "min_ISI")

# plot comparison
df_stats <- data.frame(mean_isi=r[[1]]$mean_isi, group=as.factor(1))
df_stats <- df_stats %>%
  add_row(data.frame(mean_isi=r[[2]]$mean_isi, group=as.factor(2)))
ggplot(data = df_stats, aes(x = mean_isi, y = group)) +
  stat_pointinterval(fill="skyblue", alpha = 0.75, .width = c(.5, .95)) +
  theme_minimal() +
  xlim(0.5, 1.1)

# sham
r <- fit_and_compare(df_sham_stim$min_ISI, "Sham stim", df_sham_pre$min_ISI, "Sham pre")
r <- fit_and_compare(df_sham_post$min_ISI, "Sham post", df_sham_pre$min_ISI, "Sham pre")
r <- fit_and_compare(df_sham_post$min_ISI, "Sham post", df_sham_stim$min_ISI, "Sham stim")


# male vs female baseline ------------------------------------------------------
r <- fit_and_compare(df_baseline_male$min_ISI_baseline, "Baseline male", df_baseline_female$min_ISI_baseline, "Baseline female")


# responders vs non responders baseline ----------------------------------------
r <- fit_and_compare(df_baseline_r$min_ISI_baseline, "Baseline R", df_baseline_nr$min_ISI_baseline, "Baseline NR")
