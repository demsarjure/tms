library(glue)

# first run all the normal fitting functions to get samples in appropriate dfs
# add type
df_samples_rmt$type <- "ΔRMT"
df_samples_amt$type <- "ΔAMT"
df_samples_isi24$type <- "ISI24"
df_samples_sici$type <- "ΔSICI[AUC]"

# rmt and amt ------------------------------------------------------------------
# merge rmt and amt
df_amt_rmt <- df_samples_rmt %>%
  add_row(df_samples_amt)

ggplot(df_amt_rmt, aes(x = diff, y = mu, color = condition)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  stat_pointinterval() +
  ylab("Difference") +
  facet_grid(type ~ condition, scales = "free_y", labeller = label_parsed) +
  scale_x_continuous(name = "",
                     breaks = c(0, 1, 2),
                     labels = c("Pre", "During", "Post")) +
  scale_color_manual(values = c("black", "black", "grey50")) +
  theme(legend.position = "none")

ggsave("./fig/AMT_RMT_FINAL.tiff",
       width = 1920,
       height = 960,
       dpi = 200,
       units = "px")


# sai --------------------------------------------------------------------------
ggplot(df_samples_isi24, aes(x = diff, y = mu, color = condition)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  stat_pointinterval() +
  ylab("Difference") +
  facet_grid(glue('Delta*SAI["{type}"]') ~ condition,
                  scales = "free_y", labeller = label_parsed) +
  scale_x_continuous(name = "",
                     breaks = c(0, 1, 2),
                     labels = c("Pre", "During", "Post")) +
  scale_color_manual(values = c("black", "black", "grey50")) +
  theme(legend.position = "none")

ggsave("./fig/SAI_FINAL.tiff",
       width = 1920,
       height = 480,
       dpi = 200,
       units = "px")


# sici -------------------------------------------------------------------------
ggplot(df_samples_sici, aes(x = diff, y = mu, color = condition)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  stat_pointinterval() +
  ylab("Difference") +
  facet_grid(type ~ condition, scales = "free_y", labeller = label_parsed) +
  scale_x_continuous(name = "",
                     breaks = c(0, 1, 2),
                     labels = c("Pre", "During", "Post")) +
  scale_color_manual(values = c("black", "black", "grey50")) +
  theme(legend.position = "none")

ggsave("./fig/SICI_FINAL.tiff",
       width = 1920,
       height = 480,
       dpi = 200,
       units = "px")
