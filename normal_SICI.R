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
model <- cmdstan_model("./models/cauchy.stan")


# fit and compare function -----------------------------------------------------
fit_and_compare <- function(data, name) {
  stan_data <- list(n=length(data), y=data)
  
  # fit
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
  smaller <- mcse(df_samples$mu < 0)
  bigger <- mcse(df_samples$mu > 0) 
  cat("\n----------------------------------------\n")
  cat(name, "> 0:", bigger[[1]], "+/-", bigger[[2]], "\n")
  cat(name, "< 0:", smaller[[1]], "+/-", smaller[[2]])
  cat("\n----------------------------------------\n")
  
  # return
  return(df_samples)
}


# fit and compare two groups function ------------------------------------------
fit_and_compare2 <- function(data1, name1, data2, name2) {
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
  bigger <- mcse(df_samples_1$mu > df_samples_2$mu)
  smaller <- mcse(df_samples_1$mu < df_samples_2$mu)
  cat("\n----------------------------------------\n")
  cat(name1 , ">", name2, ":", bigger[[1]], "+/-", bigger[[2]], "\n")
  cat(name1 , "<", name2, ":", smaller[[1]], "+/-", smaller[[2]])
  cat("\n----------------------------------------\n")
  
  return(list(samples_1=df_samples_1, samples_2=df_samples_2)) 
}


# plot fit ---------------------------------------------------------------------
plot_fit <- function(samples, data, x_column, min_x = -1.5, max_x = 1.5, n = 20) {
  # generate n distributions
  df_n <- sample_n(samples, n)
  x <- seq(min_x, max_x, length.out=1000)
  
  # data frame for storing generated data
  df_generated <- data.frame(x=numeric(), y=numeric(), id=numeric())
  for (i in 1:n) {
    y <- dcauchy(x, df_n$mu[i], df_n$sigma[i])
    
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


# real -------------------------------------------------------------------------
r <- fit_and_compare(df_real_stim$AMT70_diff, "(Real stim - real pre)")
r <- fit_and_compare(df_real_stim$AMT80_diff, "(Real stim - real pre)")
r <- fit_and_compare(df_real_stim$AMT90_diff, "(Real stim - real pre)")
r <- fit_and_compare(df_real_stim$AMT100_diff, "(Real stim - real pre)")

r <- fit_and_compare(df_real_post$AMT70_diff, "(Real post - real pre)")
r <- fit_and_compare(df_real_post$AMT80_diff, "(Real post - real pre)")
r <- fit_and_compare(df_real_post$AMT90_diff, "(Real post - real pre)")
r <- fit_and_compare(df_real_post$AMT100_diff, "(Real post - real pre)")


# sham -------------------------------------------------------------------------
r <- fit_and_compare(df_sham_stim$AMT70_diff, "(Sham stim - sham pre)")
r <- fit_and_compare(df_sham_stim$AMT80_diff, "(Sham stim - sham pre)")
r <- fit_and_compare(df_sham_stim$AMT90_diff, "(Sham stim - sham pre)")
r <- fit_and_compare(df_sham_stim$AMT100_diff, "(Sham stim - sham pre)")

r <- fit_and_compare(df_sham_post$AMT70_diff, "(Sham post - sham pre)")
r <- fit_and_compare(df_sham_post$AMT80_diff, "(Sham post - sham pre)")
r <- fit_and_compare(df_sham_post$AMT90_diff, "(Sham post - sham pre)")
r <- fit_and_compare(df_sham_post$AMT100_diff, "(Sham post - sham pre)")


# sham vs real -----------------------------------------------------------------
r <- fit_and_compare2(df_real_stim$AMT70_diff, "(Real stim - real pre)",
                      df_sham_stim$AMT70_diff, "(Sham stim - sham pre)")
r <- fit_and_compare2(df_real_stim$AMT80_diff, "(Real stim - real pre)",
                      df_sham_stim$AMT80_diff, "(Sham stim - sham pre)")
r <- fit_and_compare2(df_real_stim$AMT90_diff, "(Real stim - real pre)",
                      df_sham_stim$AMT90_diff, "(Sham stim - sham pre)")
r <- fit_and_compare2(df_real_stim$AMT100_diff, "(Real stim - real pre)",
                      df_sham_stim$AMT100_diff, "(Sham stim - sham pre)")

r <- fit_and_compare2(df_real_post$AMT70_diff, "(Real stim - real pre)",
                      df_sham_post$AMT70_diff, "(Sham stim - sham pre)")
r <- fit_and_compare2(df_real_post$AMT80_diff, "(Real stim - real pre)",
                      df_sham_post$AMT80_diff, "(Sham stim - sham pre)")
r <- fit_and_compare2(df_real_post$AMT90_diff, "(Real stim - real pre)",
                      df_sham_post$AMT90_diff, "(Sham stim - sham pre)")
r <- fit_and_compare2(df_real_post$AMT100_diff, "(Real stim - real pre)",
                      df_sham_post$AMT100_diff, "(Sham stim - sham pre)")


# AUC --------------------------------------------------------------------------
r <- fit_and_compare(df_real_stim$AUC_diff, "Real stim")
r <- fit_and_compare(df_sham_stim$AUC_diff, "Sham stim")
r <- fit_and_compare2(df_real_stim$AUC_diff, "Real stim", df_sham_stim$AUC_diff, "Sham stim")


# AUC3 --------------------------------------------------------------------------
r <- fit_and_compare(df_real_stim$AUC3_diff, "Real stim")
r <- fit_and_compare(df_sham_stim$AUC3_diff, "Sham stim")
r <- fit_and_compare2(df_real_stim$AUC3_diff, "Real stim", df_sham_stim$AUC3_diff, "Sham stim")

r <- fit_and_compare(df_real_post$AUC3_diff, "Real post")
r <- fit_and_compare(df_sham_post$AUC3_diff, "Sham post")
r <- fit_and_compare2(df_real_post$AUC3_diff, "Real post", df_sham_post$AUC3_diff, "Sham post")


plot_fit(r)