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
model <- cmdstan_model("./models/poisson.stan")


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
  bigger <- mcse(df_samples_1$lambda > df_samples_2$lambda)
  smaller <- mcse(df_samples_1$lambda < df_samples_2$lambda)
  
  cat("\n----------------------------------------\n")
  cat(name1 , ">", name2, ":", bigger[[1]], "+/-", bigger[[2]], "\n")
  cat(name1 , "<", name2, ":", smaller[[1]], "+/-", smaller[[2]])
  cat("\n----------------------------------------\n")
  
  return(list(samples_1=df_samples_1, samples_2=df_samples_2)) 
}


# plot fit ---------------------------------------------------------------------
plot_fit <- function(samples, data, x_column, min_x = 0, max_x = 100, n = 20) {
  # generate n distributions
  df_n <- sample_n(samples, n)
  x <- seq(min_x, max_x, length.out=101)
  
  # data frame for storing generated data
  df_generated <- data.frame(x=numeric(), y=numeric(), id=numeric())
  for (i in 1:n) {
    y <- dpois(x, df_n$lambda[i])
    
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


# responders vs non responders baseline ----------------------------------------
r <- fit_and_compare(df_baseline_r$RMT_baseline, "Baseline R", df_baseline_nr$RMT_baseline, "Baseline NR")

plot_fit(r[[1]], df_baseline_r, "RMT_baseline")
plot_fit(r[[2]], df_baseline_nr, "RMT_baseline")
