# first run all the normal fitting functions to get samples in appropriate dfs
# add type
df_samples_isi22$type <- "ΔSAI[ISI22]"
df_samples_rmt$type <- "ΔRMT"
df_samples_amt$type <- "ΔAMT"
df_samples_sici$type <- "ΔSICI[AUC]"

# merge
df_plot <- df_samples_isi22 %>%
  add_row(df_samples_rmt) %>%
  add_row(df_samples_amt) %>%
  add_row(df_samples_sici)

# plot
ggplot(df_plot, aes(x = diff, y = mu)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  stat_pointinterval() +
  ylab("Difference") +
  facet_grid(type ~ condition, scales = "free_y", labeller = label_parsed) +
  scale_x_continuous(name = "",
                     breaks = c(0, 1, 2),
                     labels = c("Pre", "During", "Post"))

ggsave("./fig/merged_no.tiff",
       width = 1920,
       height = 1920,
       dpi = 250,
       units = "px")

# remove no
df_plot <- df_plot %>%
  filter(condition != "No")

# plot
ggplot(df_plot, aes(x = diff, y = mu)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  stat_pointinterval() +
  ylab("Difference") +
  facet_grid(type ~ condition, scales = "free_y", labeller = label_parsed) +
  scale_x_continuous(name = "",
                     breaks = c(0, 1, 2),
                     labels = c("Pre", "During", "Post"))

ggsave("./fig/merged.tiff",
       width = 1920,
       height = 1920,
       dpi = 250,
       units = "px")
