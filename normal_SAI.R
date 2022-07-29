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
source("load_SAI_no.R")

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

  # report
  mean_data <- round(mean(df_samples$mu), 2)
  q025 <- round(quantile(df_samples$mu, 0.025), 2)
  q975 <- round(quantile(df_samples$mu, 0.975), 2)

  cat(paste0(mean_data, " [", q025, ", ", q975, "]"))

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


# ISIs -------------------------------------------------------------------------
# real
r <- fit_and_compare(df_real_stim$ISI20_diff, "(Real stim - real pre)")
r <- fit_and_compare(df_real_stim$ISI22_diff, "(Real stim - real pre)")
r <- fit_and_compare(df_real_stim$ISI24_diff, "(Real stim - real pre)")
r <- fit_and_compare(df_real_post$ISI20_diff, "(Real stim - real pre)")
r <- fit_and_compare(df_real_post$ISI22_diff, "(Real stim - real pre)")
r <- fit_and_compare(df_real_post$ISI24_diff, "(Real stim - real pre)")

# sham
r <- fit_and_compare(df_sham_stim$ISI20_diff, "(Sham stim - sham pre)")
r <- fit_and_compare(df_sham_stim$ISI22_diff, "(Sham stim - sham pre)")
r <- fit_and_compare(df_sham_stim$ISI24_diff, "(Sham stim - sham pre)")
r <- fit_and_compare(df_sham_post$ISI20_diff, "(Sham stim - sham pre)")
r <- fit_and_compare(df_sham_post$ISI22_diff, "(Sham stim - sham pre)")
r <- fit_and_compare(df_sham_post$ISI24_diff, "(Sham stim - sham pre)")

# no
r <- fit_and_compare(df_no_stim$ISI20_diff, "(No stim - no pre)")
r <- fit_and_compare(df_no_stim$ISI22_diff, "(No stim - no pre)")
r <- fit_and_compare(df_no_stim$ISI24_diff, "(No stim - no pre)")
r <- fit_and_compare(df_no_post$ISI20_diff, "(No stim - no pre)")
r <- fit_and_compare(df_no_post$ISI22_diff, "(No stim - no pre)")
r <- fit_and_compare(df_no_post$ISI24_diff, "(No stim - no pre)")


# sham vs real -----------------------------------------------------------------
r <- fit_and_compare2(df_real_stim$ISI20_diff, "(Real stim - real pre)",
                      df_sham_stim$ISI20_diff, "(Sham stim - sham pre)")
r <- fit_and_compare2(df_real_stim$ISI22_diff, "(Real stim - real pre)",
                      df_sham_stim$ISI22_diff, "(Sham stim - sham pre)")
r <- fit_and_compare2(df_real_stim$ISI24_diff, "(Real stim - real pre)",
                      df_sham_stim$ISI24_diff, "(Sham stim - sham pre)")

r <- fit_and_compare2(df_real_post$ISI20_diff, "(Real post - real pre)",
                      df_sham_post$ISI20_diff, "(Sham post - sham pre)")
r <- fit_and_compare2(df_real_post$ISI22_diff, "(Real post - real pre)",
                      df_sham_post$ISI22_diff, "(Sham post - sham pre)")
r <- fit_and_compare2(df_real_post$ISI24_diff, "(Real post - real pre)",
                      df_sham_post$ISI24_diff, "(Sham post - sham pre)")


# no vs real -------------------------------------------------------------------
r <- fit_and_compare2(df_real_stim$ISI20_diff, "(Real stim - real pre)",
                      df_no_stim$ISI20_diff, "(No stim - no pre)")
r <- fit_and_compare2(df_real_stim$ISI22_diff, "(Real stim - real pre)",
                      df_no_stim$ISI22_diff, "(No stim - no pre)")
r <- fit_and_compare2(df_real_stim$ISI24_diff, "(Real stim - real pre)",
                      df_no_stim$ISI24_diff, "(No stim - no pre)")

r <- fit_and_compare2(df_real_post$ISI20_diff, "(Real post - real pre)",
                      df_no_post$ISI20_diff, "(No post - no pre)")
r <- fit_and_compare2(df_real_post$ISI22_diff, "(Real post - real pre)",
                      df_no_post$ISI22_diff, "(No post - no pre)")
r <- fit_and_compare2(df_real_post$ISI24_diff, "(Real post - real pre)",
                      df_no_post$ISI24_diff, "(No post - no pre)")


# no vs sham -------------------------------------------------------------------
r <- fit_and_compare2(df_sham_stim$ISI20_diff, "(Sham stim - sham pre)",
                      df_no_stim$ISI20_diff, "(No stim - no pre)")
r <- fit_and_compare2(df_sham_stim$ISI22_diff, "(Sham stim - sham pre)",
                      df_no_stim$ISI22_diff, "(No stim - no pre)")
r <- fit_and_compare2(df_sham_stim$ISI24_diff, "(Sham stim - sham pre)",
                      df_no_stim$ISI24_diff, "(No stim - no pre)")

r <- fit_and_compare2(df_sham_post$ISI20_diff, "(Sham post - sham pre)",
                      df_no_post$ISI20_diff, "(No post - no pre)")
r <- fit_and_compare2(df_sham_post$ISI22_diff, "(Sham post - sham pre)",
                      df_no_post$ISI22_diff, "(No post - no pre)")
r <- fit_and_compare2(df_sham_post$ISI24_diff, "(Sham post - sham pre)",
                      df_no_post$ISI24_diff, "(No post - no pre)")


# ISI22 ------------------------------------------------------------------------
r <- fit_and_compare(df_real_stim$ISI22_diff, "(Real stim - real pre)")
df_samples <- data.frame(mu = r$mu,
                         diff = 1,
                         condition = "Real")

r <- fit_and_compare(df_real_post$ISI22_diff, "(Real post - real stim)")
df_samples <- df_samples %>% add_row(data.frame(mu = r$mu,
                                     diff = 2,
                                     condition = "Real"))

r <- fit_and_compare(df_sham_stim$ISI22_diff, "(Sham stim - sham pre)")
df_samples <- df_samples %>% add_row(data.frame(mu = r$mu,
                                     diff = 1,
                                     condition = "Sham"))

r <- fit_and_compare(df_sham_post$ISI22_diff, "(Sham post - sham pre)")
df_samples <- df_samples %>% add_row(data.frame(mu = r$mu,
                                     diff = 2,
                                     condition = "Sham"))

r <- fit_and_compare(df_no_stim$ISI22_diff, "(No stim - no pre)")
df_samples <- df_samples %>% add_row(data.frame(mu = r$mu,
                                     diff = 1,
                                     condition = "No"))

r <- fit_and_compare(df_no_post$ISI22_diff, "(No post - no pre)")
df_samples <- df_samples %>% add_row(data.frame(mu = r$mu,
                                     diff = 2,
                                     condition = "No"))

# add dummy entries
df_samples <- df_samples %>%
  add_row(data.frame(mu = 0, diff = 0, condition = "Real"))
df_samples <- df_samples %>%
  add_row(data.frame(mu = 0, diff = 0, condition = "Sham"))
df_samples <- df_samples %>%
  add_row(data.frame(mu = 0, diff = 0, condition = "No"))

df_samples$condition <-
  factor(df_samples$condition, levels = c("Real", "Sham", "No"))

df_samples_isi22 <- df_samples


# pre comparison ---------------------------------------------------------------
r <- fit_and_compare2(df_real_pre$ISI20, "Real", df_sham_pre$ISI20, "Sham")
# Real > Sham: 5.7 +/- 0.4%

r <- fit_and_compare2(df_real_pre$ISI22, "Real", df_sham_pre$ISI22, "Sham")
# Real > Sham: 31.8 +/- 0.8%

r <- fit_and_compare2(df_real_pre$ISI24, "Real", df_sham_pre$ISI24, "Sham")
# Real > Sham: 22.73 +/- 0.7%


# max_ISI ----------------------------------------------------------------------
# r <- fit_and_compare(df_real_stim$max_ISI_diff, "(Real stim - real pre)")
# df_samples <- data.frame(mu = r$mu,
#                          diff = 1,
#                          condition = "Real")

# r <- fit_and_compare(df_real_post$max_ISI_diff, "(Real post - real stim)")
# df_samples <- df_samples %>% add_row(data.frame(mu = r$mu,
#                                      diff = 2,
#                                      condition = "Real"))

# r <- fit_and_compare(df_sham_stim$max_ISI_diff, "(Sham stim - sham pre)")
# df_samples <- df_samples %>% add_row(data.frame(mu = r$mu,
#                                      diff = 1,
#                                      condition = "Sham"))

# r <- fit_and_compare(df_sham_post$max_ISI_diff, "(Sham post - sham pre)")
# df_samples <- df_samples %>% add_row(data.frame(mu = r$mu,
#                                      diff = 2,
#                                      condition = "Sham"))

# r <- fit_and_compare(df_no_stim$max_ISI_diff, "(No stim - no pre)")
# df_samples <- df_samples %>% add_row(data.frame(mu = r$mu,
#                                      diff = 1,
#                                      condition = "No"))

# r <- fit_and_compare(df_no_post$max_ISI_diff, "(No post - no pre)")
# df_samples <- df_samples %>% add_row(data.frame(mu = r$mu,
#                                      diff = 2,
#                                      condition = "No"))

# # add dummy entries
# df_samples <- df_samples %>%
#   add_row(data.frame(mu = 0, diff = 0, condition = "Real"))
# df_samples <- df_samples %>%
#   add_row(data.frame(mu = 0, diff = 0, condition = "Sham"))
# df_samples <- df_samples %>%
#   add_row(data.frame(mu = 0, diff = 0, condition = "No"))

# set factors
# df_samples$condition <-
#  factor(df_samples$condition, levels = c("Real", "Sham", "No"))

# ggplot(df_samples, aes(x = diff, y = mu)) +
#  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
#  stat_pointinterval() +
#  ylab("Difference") +
#  facet_grid(. ~ condition) +
#  scale_x_continuous(name = "",
#                    breaks = c(0, 1, 2),
#                    labels = c("Pre", "During", "Post"))

# ggsave("./fig/SAI_2.tiff",
#       width = 1920,
#       height = 960,
#       dpi = 300,
#       units = "px")

# r <- fit_and_compare2(df_real_stim$max_ISI_diff, "(Real stim - real pre)",
#                       df_sham_stim$max_ISI_diff, "(Sham stim - sham pre)")
# r <- fit_and_compare2(df_real_post$max_ISI_diff, "(Real stim - real pre)",
#                       df_sham_post$max_ISI_diff, "(Sham stim - sham pre)")
# r <- fit_and_compare2(df_real_stim$max_ISI_diff, "(Real stim - real pre)",
#                       df_no_stim$max_ISI_diff, "(No stim - no pre)")
# r <- fit_and_compare2(df_real_post$max_ISI_diff, "(Real stim - real pre)",
#                       df_no_post$max_ISI_diff, "(No stim - no pre)")
# r <- fit_and_compare2(df_sham_stim$max_ISI_diff, "(Sham stim - sham pre)",
#                       df_no_stim$max_ISI_diff, "(No stim - no pre)")
# r <- fit_and_compare2(df_sham_post$max_ISI_diff, "(Sham stim - sham pre)",
#                       df_no_post$max_ISI_diff, "(No stim - no pre)")
