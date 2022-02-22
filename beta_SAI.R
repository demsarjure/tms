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
model <- cmdstan_model("./models/beta.stan")


# fit and compare two groups function ------------------------------------------
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
  
  # stats
  mean1 <- mean(df_samples_1$theta)
  hdi1 <- hdi(df_samples_1$theta)
  mean2 <- mean(df_samples_2$theta)
  hdi2 <- hdi(df_samples_2$theta)
  
  # compare
  bigger <- mcse(df_samples_1$theta > df_samples_2$theta)
  smaller <- mcse(df_samples_1$theta < df_samples_2$theta)
  cat("\n----------------------------------------\n")
  cat(name1, mean1, "+/-", hdi1[1], "-", hdi1[2], "\n")
  cat(name2, mean2, "+/-", hdi2[1], "-", hdi2[2], "\n")
  cat(name1 , ">", name2, ":", bigger[[1]], "+/-", bigger[[2]], "\n")
  cat(name1 , "<", name2, ":", smaller[[1]], "+/-", smaller[[2]])
  cat("\n----------------------------------------\n")
  
  return(list(samples_1=df_samples_1, samples_2=df_samples_2)) 
}


# differences of responders between sexes differences --------------------------
r <- fit_and_compare(df_responders_male$Responder, "Male", df_responders_female$Responder, "Female")

sum(df_responders_male$Responder)
nrow(df_responders_male)
sum(df_responders_female$Responder)
nrow(df_responders_female)

