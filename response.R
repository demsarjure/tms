# libraries
library(tidyverse)
library(psych)

# load the data
source("load_SAI.R")
source("load_RMT.R")
source("load_SICI.R")

# merge
df_response <- df_response_sai %>% inner_join(df_response_rmt)
df_response <- df_response %>% inner_join(df_response_sici)

# prepare
df_long <- df_response %>%
  pivot_longer(!ID, names_to = "Name", values_to = "Value")

# as factor and levels
df_long$Name <- factor(df_long$Name, levels = c("min_ISI", "RMT", "AMT70", "AMT80", "AMT90", "AMT100"))
df_long$Value <- factor(df_long$Value, levels = c(0, 1))

# plot
ggplot(df_long, aes(x=Name, y=Value)) +
  geom_point() +
  theme_minimal() +
  facet_grid(ID ~ .)

# correlation
pairs.panels(df_response %>% select(-ID), 
             method = "pearson",
             hist.col = "skyblue",
             density = TRUE,
             ellipses = TRUE
)
