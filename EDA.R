# Analysis on MLB hitters to understand latent similarities between players
# Dataset only includes 2019 statistics for players who qualified for batting title (502 plate appearences or an average of 3.1 PAs a game)
# For some reason there is a min of 476 PAs in this dataset - are those batters worth excluding? How did they manage to get in here?
# Look into the above before publishing or exclude those players

# Library Import ----------------------------------------------------------

library(tidyverse)
library(corrplot)

# Data Import -------------------------------------------------------------

ds <- read_csv("MLB Hitter Statcast Stats 2019.csv") %>%
  select(last_name:sprint_speed)

# EDA ---------------------------------------------------------------------

head(ds)
summary(ds)  # no missing values
dim(ds)  # 156 players - 70 vars

# How many players with over 502 PAs: 135
ds %>% 
  filter(b_total_pa > 502)

# High correlation between related attributes (meatball % & z swing % for example)
# Probably shouldn't include all n variables of a set. Only include n-1 (because the inclusion of the first n-1 variables imply the final)
ds %>%
  select(exit_velocity_avg, launch_angle_avg, sweet_spot_percent, barrel_batted_rate, solidcontact_percent, z_swing_percent, oz_swing_percent,
         meatball_swing_percent, pull_percent:opposite_percent) %>% 
  scale(center = TRUE, scale = TRUE) %>% 
  cor(use = "complete.obs", method = "pearson") %>%
  corrplot.mixed(lower = "number", upper = "circle", tl.pos = "lt")


# K-means clustering ------------------------------------------------------

set.seed(2)
x <- ds %>% 
  select(exit_velocity_avg, launch_angle_avg, sweet_spot_percent, barrel_batted_rate, solidcontact_percent, 
         poorlyunder_percent:hard_hit_percent, z_swing_percent, oz_swing_percent, meatball_swing_percent, pull_percent:opposite_percent, 
         groundballs_percent:sprint_speed) %>% 
  scale(center = TRUE, scale = TRUE)
km_out <-kmeans(x, 6, nstart = 50) 
km_out
ds_5_cluster <- cbind(ds, km_out$cluster)
ds_5_cluster %>% 
  select(`km_out$cluster`, last_name, first_name, b_k_percent:b_total_bases) %>% 
  arrange(`km_out$cluster`, desc(on_base_plus_slg))


