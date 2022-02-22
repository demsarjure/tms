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
source("load_RMT.R")

# model
model <- cmdstan_model("./models/linear_robust.stan")


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


# sham vs real -----------------------------------------------------------------
df_s_real_stim <- fit_and_compare(df_real_stim$RMT_baseline, df_real_stim$RMT_diff, "Real stim")
df_s_real_post <- fit_and_compare(df_real_post$RMT_baseline, df_real_post$RMT_diff, "Real post")
mcse(df_s_real_stim$b < df_s_real_post$b)

df_s_sham_stim <- fit_and_compare(df_sham_stim$RMT_baseline, df_sham_stim$RMT_diff, "Sham stim")
df_s_sham_post <- fit_and_compare(df_sham_post$RMT_baseline, df_sham_post$RMT_diff, "Sham post")
mcse(df_s_sham_stim$b < df_s_sham_post$b)