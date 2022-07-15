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
source("load_SICI_no.R")

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


# no -------------------------------------------------------------------------
r <- fit_and_compare(df_no_stim$AMT70_diff, "(No stim - no pre)")
r <- fit_and_compare(df_no_stim$AMT80_diff, "(No stim - no pre)")
r <- fit_and_compare(df_no_stim$AMT90_diff, "(No stim - no pre)")
r <- fit_and_compare(df_no_stim$AMT100_diff, "(No stim - no pre)")

r <- fit_and_compare(df_no_post$AMT70_diff, "(No post - no pre)")
r <- fit_and_compare(df_no_post$AMT80_diff, "(No post - no pre)")
r <- fit_and_compare(df_no_post$AMT90_diff, "(No post - no pre)")
r <- fit_and_compare(df_no_post$AMT100_diff, "(No post - no pre)")


# sham vs real -----------------------------------------------------------------
r <- fit_and_compare2(df_real_stim$AMT70_diff, "(Real stim - real pre)",
                      df_sham_stim$AMT70_diff, "(Sham stim - sham pre)")
r <- fit_and_compare2(df_real_stim$AMT80_diff, "(Real stim - real pre)",
                      df_sham_stim$AMT80_diff, "(Sham stim - sham pre)")
r <- fit_and_compare2(df_real_stim$AMT90_diff, "(Real stim - real pre)",
                      df_sham_stim$AMT90_diff, "(Sham stim - sham pre)")
r <- fit_and_compare2(df_real_stim$AMT100_diff, "(Real stim - real pre)",
                      df_sham_stim$AMT100_diff, "(Sham stim - sham pre)")

r <- fit_and_compare2(df_real_post$AMT70_diff, "(Real post - real pre)",
                      df_sham_post$AMT70_diff, "(Sham post - sham pre)")
r <- fit_and_compare2(df_real_post$AMT80_diff, "(Real post - real pre)",
                      df_sham_post$AMT80_diff, "(Sham post - sham pre)")
r <- fit_and_compare2(df_real_post$AMT90_diff, "(Real post - real pre)",
                      df_sham_post$AMT90_diff, "(Sham post - sham pre)")
r <- fit_and_compare2(df_real_post$AMT100_diff, "(Real post - real pre)",
                      df_sham_post$AMT100_diff, "(Sham post - sham pre)")


# no vs real -----------------------------------------------------------------
r <- fit_and_compare2(df_real_stim$AMT70_diff, "(Real stim - real pre)",
                      df_no_stim$AMT70_diff, "(No stim - no pre)")
r <- fit_and_compare2(df_real_stim$AMT80_diff, "(Real stim - real pre)",
                      df_no_stim$AMT80_diff, "(No stim - no pre)")
r <- fit_and_compare2(df_real_stim$AMT90_diff, "(Real stim - real pre)",
                      df_no_stim$AMT90_diff, "(No stim - no pre)")
r <- fit_and_compare2(df_real_stim$AMT100_diff, "(Real stim - real pre)",
                      df_no_stim$AMT100_diff, "(No stim - no pre)")

r <- fit_and_compare2(df_real_post$AMT70_diff, "(Real post - real pre)",
                      df_no_post$AMT70_diff, "(No post - no pre)")
r <- fit_and_compare2(df_real_post$AMT80_diff, "(Real post - real pre)",
                      df_no_post$AMT80_diff, "(No post - no pre)")
r <- fit_and_compare2(df_real_post$AMT90_diff, "(Real post - real pre)",
                      df_no_post$AMT90_diff, "(No post - no pre)")
r <- fit_and_compare2(df_real_post$AMT100_diff, "(Real post - real pre)",
                      df_no_post$AMT100_diff, "(No post - no pre)")


# no vs sham -----------------------------------------------------------------
r <- fit_and_compare2(df_sham_stim$AMT70_diff, "(Sham stim - sham pre)",
                      df_no_stim$AMT70_diff, "(No stim - no pre)")
r <- fit_and_compare2(df_sham_stim$AMT80_diff, "(Sham stim - sham pre)",
                      df_no_stim$AMT80_diff, "(No stim - no pre)")
r <- fit_and_compare2(df_sham_stim$AMT90_diff, "(Sham stim - sham pre)",
                      df_no_stim$AMT90_diff, "(No stim - no pre)")
r <- fit_and_compare2(df_sham_stim$AMT100_diff, "(Sham stim - sham pre)",
                      df_no_stim$AMT100_diff, "(No stim - no pre)")

r <- fit_and_compare2(df_sham_post$AMT70_diff, "(Sham post - sham pre)",
                      df_no_post$AMT70_diff, "(No post - no pre)")
r <- fit_and_compare2(df_sham_post$AMT80_diff, "(Sham post - sham pre)",
                      df_no_post$AMT80_diff, "(No post - no pre)")
r <- fit_and_compare2(df_sham_post$AMT90_diff, "(Sham post - sham pre)",
                      df_no_post$AMT90_diff, "(No post - no pre)")
r <- fit_and_compare2(df_sham_post$AMT100_diff, "(Sham post - sham pre)",
                      df_no_post$AMT100_diff, "(No post - no pre)")

# AUC3 -------------------------------------------------------------------------
r <- fit_and_compare(df_real_stim$AUC3_diff, "Real stim")
df_samples <- data.frame(mu = r$mu,
                         diff = 1,
                         condition = "Real")

r <- fit_and_compare(df_real_post$AUC3_diff, "Real post")
df_samples <- df_samples %>% add_row(data.frame(mu = r$mu,
                                     diff = 2,
                                     condition = "Real"))

r <- fit_and_compare(df_sham_stim$AUC3_diff, "Sham stim")
df_samples <- df_samples %>% add_row(data.frame(mu = r$mu,
                                     diff = 1,
                                     condition = "Sham"))

r <- fit_and_compare(df_sham_post$AUC3_diff, "Sham post")
df_samples <- df_samples %>% add_row(data.frame(mu = r$mu,
                                     diff = 2,
                                     condition = "Sham"))

r <- fit_and_compare(df_no_stim$AUC3_diff, "No stim")
df_samples <- df_samples %>% add_row(data.frame(mu = r$mu,
                                     diff = 1,
                                     condition = "No"))

r <- fit_and_compare(df_no_post$AUC3_diff, "No post")
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

# set factors
df_samples$condition <-
  factor(df_samples$condition, levels = c("Real", "Sham", "No"))

df_samples_sici <- df_samples

# ggplot(df_samples, aes(x = diff, y = mu)) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
#   stat_pointinterval() +
#   ylab("Difference") +
#   facet_grid(. ~ condition) +
#   scale_x_continuous(name = "",
#                     breaks = c(0, 1, 2),
#                     labels = c("Pre", "During", "Post"))

# ggsave("./fig/SICI_2.tiff",
#        width = 1920,
#        height = 960,
#        dpi = 300,
#        units = "px")

# alternative plot
r <- fit_and_compare(df_real_stim$AUC3_diff, "Real stim")
df_samples <- data.frame(mu = r$mu,
                         diff = 0.9,
                         condition = "Real")

r <- fit_and_compare(df_real_post$AUC3_diff, "Real post")
df_samples <- df_samples %>% add_row(data.frame(mu = r$mu,
                                     diff = 1.9,
                                     condition = "Real"))

r <- fit_and_compare(df_sham_stim$AUC3_diff, "Sham stim")
df_samples <- df_samples %>% add_row(data.frame(mu = r$mu,
                                     diff = 1.1,
                                     condition = "Sham"))

r <- fit_and_compare(df_sham_post$AUC3_diff, "Sham post")
df_samples <- df_samples %>% add_row(data.frame(mu = r$mu,
                                     diff = 2.1,
                                     condition = "Sham"))

# add dummy entries
df_samples <- df_samples %>%
  add_row(data.frame(mu = 0, diff = -0.1, condition = "Real"))
df_samples <- df_samples %>%
  add_row(data.frame(mu = 0, diff = 0.1, condition = "Sham"))

ggplot(df_samples, aes(x = diff, y = mu, color = condition)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  stat_pointinterval() +
  ylab("Difference") +
  scale_x_continuous(name = "",
                    breaks = c(0, 1, 2),
                    labels = c("Pre", "During", "Post")) +
  scale_color_brewer(type = "qual", palette = 6)


r <- fit_and_compare2(df_real_stim$AUC3_diff, "Real stim",
                      df_sham_stim$AUC3_diff, "Sham stim")

r <- fit_and_compare2(df_real_post$AUC3_diff, "Real post",
                      df_sham_post$AUC3_diff, "Sham post")

r <- fit_and_compare2(df_real_stim$AUC3_diff, "Real stim",
                      df_no_stim$AUC3_diff, "No stim")

r <- fit_and_compare2(df_real_post$AUC3_diff, "Real post",
                      df_no_post$AUC3_diff, "No post")

r <- fit_and_compare2(df_sham_stim$AUC3_diff, "Sham stim",
                      df_no_stim$AUC3_diff, "No stim")

r <- fit_and_compare2(df_sham_post$AUC3_diff, "Sham post",
                      df_no_post$AUC3_diff, "No post")


# pre comparison ---------------------------------------------------------------
r <- fit_and_compare2(df_real_pre$AUC3, "Real", df_sham_pre$AUC3, "Sham")
r <- fit_and_compare2(df_real_pre$AUC3, "Real", df_no_pre$AUC3, "No")
r <- fit_and_compare2(df_sham_pre$AUC3, "Sham", df_no_pre$AUC3, "No")

# SICI pre:
# 	Real > Sham: 75.65 +/- 0.8 %
# 	Real > No: 40.93 +/- 0.8 %
# 	Sham > No: 21.05 +/- 0.8 %


# AUC --------------------------------------------------------------------------
r <- fit_and_compare(df_real_stim$AUC_diff, "Real stim")
r <- fit_and_compare(df_sham_stim$AUC_diff, "Sham stim")
r <- fit_and_compare2(df_real_stim$AUC_diff, "Real stim",
                      df_sham_stim$AUC_diff, "Sham stim")


# AUCAVG -----------------------------------------------------------------------
r <- fit_and_compare(df_real_stim$AUCAVG_diff, "Real stim")
r <- fit_and_compare(df_sham_stim$AUCAVG_diff, "Sham stim")
r <- fit_and_compare2(df_real_stim$AUCAVG_diff, "Real stim",
                      df_sham_stim$AUCAVG_diff, "Sham stim")

r <- fit_and_compare(df_real_post$AUCAVG_diff, "Real post")
r <- fit_and_compare(df_sham_post$AUCAVG_diff, "Sham post")
r <- fit_and_compare2(df_real_post$AUCAVG_diff, "Real post",
                      df_sham_post$AUCAVG_diff, "Sham post")
