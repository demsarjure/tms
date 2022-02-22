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


# RMT --------------------------------------------------------------------------
r <- fit_and_compare(df_real_stim$RMT_diff, "(Real stim - real pre)")
df_stats <- data.frame(mean=r$mu,
                       diff="Stimulation - Pre",
                       condition="Real")

r <- fit_and_compare(df_real_post$RMT_diff, "(Real post - real pre)")
df_stats <- df_stats %>% add_row(data.frame(mean=r$mu,
                                            diff="Post - Pre",
                                            condition="Real"))

r <- fit_and_compare(df_real_post$RMT_diff2, "(Real post - real stim)")
df_stats <- df_stats %>% add_row(data.frame(mean=r$mu,
                                            diff="Post - Stimulation",
                                            condition="Real"))

r <- fit_and_compare(df_sham_stim$RMT_diff, "(Sham stim - sham pre)")
df_stats <- df_stats %>% add_row(data.frame(mean=r$mu,
                                            diff="Stimulation - Pre",
                                            condition="Sham"))

r <- fit_and_compare(df_sham_post$RMT_diff, "(Sham post - sham pre)")
df_stats <- df_stats %>% add_row(data.frame(mean=r$mu,
                                            diff="Post - Pre",
                                            condition="Sham"))

r <- fit_and_compare(df_sham_post$RMT_diff2, "(Sham post - sham stim)")
df_stats <- df_stats %>% add_row(data.frame(mean=r$mu,
                                            diff="Post - Stimulation",
                                            condition="Sham"))
# plot
plot_fit(r, df_sham_stim, "RMT_diff")

# sorting
df_stats$diff <- factor(df_stats$diff, levels=c("Post - Stimulation", "Post - Pre", "Stimulation - Pre"))

# plot comparison
ggplot(data = df_stats, aes(x = mean, y = diff)) +
  stat_halfeye(fill="skyblue", alpha = 0.75, .width = c(.5, .95)) +
  theme_minimal() +
  geom_vline(xintercept = 0, size = 1, alpha = 0.25) +
  facet_grid(. ~ condition) +
  theme(panel.spacing = unit(2, "lines"))
  

# sham vs real -----------------------------------------------------------------
r <- fit_and_compare2(df_real_stim$RMT_diff, "(Real stim - real pre)",
                 df_sham_stim$RMT_diff, "(Sham stim - sham pre)")


# male vs female ---------------------------------------------------------------
r <- fit_and_compare(df_sham_female_stim$RMT_diff, "Female (Sham stim - sham pre)")
r <- fit_and_compare(df_sham_female_post$RMT_diff, "Female (Sham post - sham pre)")
r <- fit_and_compare(df_sham_female_post$RMT_diff2, "Female (Sham post - sham stim)")

r <- fit_and_compare(df_sham_male_stim$RMT_diff, "Male (Sham stim - sham pre)")
r <- fit_and_compare(df_sham_male_post$RMT_diff, "Male (Sham post - sham pre)")
r <- fit_and_compare(df_sham_male_post$RMT_diff2, "Male (Sham post - sham stim)")

r <- fit_and_compare2(df_sham_female_stim$RMT_diff, "Female (Sham stim - sham pre)",
                      df_sham_male_stim$RMT_diff, "Male (Sham stim - sham pre)")

r <- fit_and_compare(df_real_female_stim$RMT_diff, "Female (Real stim - real pre)")
r <- fit_and_compare(df_real_female_post$RMT_diff, "Female (Real post - real pre)")
r <- fit_and_compare(df_real_female_post$RMT_diff2, "Female (Real post - real stim)")

r <- fit_and_compare(df_real_male_stim$RMT_diff, "Male (Real stim - real pre)")
r <- fit_and_compare(df_real_male_post$RMT_diff, "Male (Real post - real pre)")
r <- fit_and_compare(df_real_male_post$RMT_diff2, "Male (Real post - real stim)")

r <- fit_and_compare2(df_real_female_stim$RMT_diff, "Female (Real stim - real pre)",
                      df_real_male_stim$RMT_diff, "Male (Real stim - real pre)")
