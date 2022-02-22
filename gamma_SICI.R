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
source("load_SICI.R")

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


# real stim vs real pre --------------------------------------------------------
r <- fit_and_compare(df_real_stim$AMT70, "Real stim", df_real_pre$AMT70, "Real pre")
r <- fit_and_compare(df_real_stim$AMT80, "Real stim", df_real_pre$AMT80, "Real pre")
r <- fit_and_compare(df_real_stim$AMT90, "Real stim", df_real_pre$AMT90, "Real pre")
r <- fit_and_compare(df_real_stim$AMT100, "Real stim", df_real_pre$AMT100, "Real pre")

plot_fit(r[[1]], df_sham_stim, "AMT100")


# real stim vs real post -------------------------------------------------------
r <- fit_and_compare(df_real_stim$AMT70, "Real stim", df_real_post$AMT70, "Real post")
r <- fit_and_compare(df_real_stim$AMT80, "Real stim", df_real_post$AMT80, "Real post")
r <- fit_and_compare(df_real_stim$AMT90, "Real stim", df_real_post$AMT90, "Real post")
r <- fit_and_compare(df_real_stim$AMT100, "Real stim", df_real_post$AMT100, "Real post")


# sham stim vs sham pre --------------------------------------------------------
r <- fit_and_compare(df_sham_stim$AMT70, "Sham stim", df_sham_pre$AMT70, "Sham pre")
r <- fit_and_compare(df_sham_stim$AMT80, "Sham stim", df_sham_pre$AMT80, "Sham pre")
r <- fit_and_compare(df_sham_stim$AMT90, "Sham stim", df_sham_pre$AMT90, "Sham pre")
r <- fit_and_compare(df_sham_stim$AMT100, "Sham stim", df_sham_pre$AMT100, "Sham pre")


# sham stim vs sham post -------------------------------------------------------
r <- fit_and_compare(df_sham_stim$AMT70, "Sham stim", df_sham_post$AMT70, "Sham post")
r <- fit_and_compare(df_sham_stim$AMT80, "Sham stim", df_sham_post$AMT80, "Sham post")
r <- fit_and_compare(df_sham_stim$AMT90, "Sham stim", df_sham_post$AMT90, "Sham post")
r <- fit_and_compare(df_sham_stim$AMT100, "Sham stim", df_sham_post$AMT100, "Sham post")


# sham vs real -----------------------------------------------------------------
r <- fit_and_compare(df_real_stim$AMT70, "Real stim", df_sham_stim$AMT70, "Sham stim")
r <- fit_and_compare(df_real_stim$AMT80, "Real stim", df_sham_stim$AMT80, "Sham stim")
r <- fit_and_compare(df_real_stim$AMT90, "Real stim", df_sham_stim$AMT90, "Sham stim")
r <- fit_and_compare(df_real_stim$AMT100, "Real stim", df_sham_stim$AMT100, "Sham stim")


# AUC --------------------------------------------------------------------------
r <- fit_and_compare(df_real_stim$AUC, "Real stim", df_real_pre$AUC, "Real pre")
r <- fit_and_compare(df_sham_stim$AUC, "Real stim", df_sham_pre$AUC, "Real pre")
