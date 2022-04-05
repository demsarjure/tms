# libraries
library(ggplot2)
library(HDInterval)
library(bayesplot)
library(posterior)
library(cmdstanr)
library(tidyverse)


# prep -------------------------------------------------------------------------
# load the data
source("load_SAI.R")

# models
gamma_model <- cmdstan_model("./models/gamma.stan")
normal_model <- cmdstan_model("./models/normal.stan")

# data
stan_data <- list(n=nrow(df_real_stim), y=df_real_stim$min_ISI_baseline)


# gamma ------------------------------------------------------------------------
fit_gamma <- gamma_model$sample(
  data = stan_data,
  parallel_chains = 4,
  refresh = 0
)

# traceplot
mcmc_trace(fit_gamma$draws())

# summary
fit_gamma$summary()

# extract
df_samples_gamma <- as_draws_df(fit_gamma$draws())


# normal -----------------------------------------------------------------------
fit_normal <- normal_model$sample(
  data = stan_data,
  parallel_chains = 4,
  refresh = 0
)

# traceplot
mcmc_trace(fit_normal$draws())

# summary
fit_normal$summary()

# extract
df_samples_normal <- as_draws_df(fit_normal$draws())


# plot -------------------------------------------------------------------------
# x axis
x <- seq(0, 5, length.out=10000)
y_gamma <- dgamma(x, mean(df_samples_gamma$alpha), mean(df_samples_gamma$alpha))
y_normal <- dnorm(x, mean(df_samples_normal$mu), mean(df_samples_normal$sigma))
  
# bind
df_plot <- data.frame(x=x, y=y_gamma, Model="gamma")
df_plot <- df_plot %>%
  add_row(data.frame(x=x, y=y_normal, Model="normal"))

# plot
ggplot() +
  geom_density(data=df_real_stim, aes(x=min_ISI_baseline, y = ..density..),
               fill="skyblue", alpha=0.75, color=NA) +
  geom_line(data=df_plot,
            aes(x=x, y=y, color=Model), size=1) +
  



