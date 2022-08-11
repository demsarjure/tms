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

# plot fit ---------------------------------------------------------------------
plot_fit <- function(samples, data, n = 50, x_column, y_column, title) {
  # generate n distributions
  df_n <- slice_sample(samples, n = n)
  x <- seq(30, 65, length.out = 1000)

  # data frame for storing generated data
  df_generated <- data.frame(x = numeric(), y = numeric(), id = numeric())
  for (i in 1:n) {
    # extract values
    beta <- df_n$b[i]
    alpha <- df_n$a[i]

    # calculate probability
    y <- alpha + beta * x

    # add to lines
    temp <- data.frame(x = x, y = y, id = i)
    df_generated <- rbind(df_generated, temp)
  }

  # plot
  g <- ggplot() +
    geom_point(data = data, aes_string(x = x_column, y = y_column),
                 fill = "skyblue", alpha = 0.75, shape = 16) +
    geom_line(data = df_generated,
              aes(x = x, y = y, group = id),
              alpha = 0.1) +
    ylim(-7, 5) +
    xlab("Baseline") +
    ylab("Difference") +
    ggtitle(title)

  # return
  return(g)
}

# summary stats ----------------------------------------------------------------
summary_stats <- function(data) {
  mean_data <- round(mean(data), 2)
  q025 <- round(quantile(data, 0.025), 2)
  q975 <- round(quantile(data, 0.975), 2)

  cat(paste0(mean_data, " [", q025, ", ", q975, "]"))
}
