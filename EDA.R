# Analysis on MLB hitters to understand latent similarities between players
# Dataset only includes 2019 statistics for players who qualified for batting title (502 plate appearences or an average of 3.1 PAs a game)
# For some reason there is a min of 476 PAs in this dataset - are those batters worth excluding? How did they manage to get in here?
# Look into the above before publishing or exclude those players

# Library Import ----------------------------------------------------------

library(tidyverse)
library(corrplot)
library(factoextra)
library(reactable)

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

# Should just use variables that hitter's swing can influence
set.seed(2)
y <- ds %>% 
  select(exit_velocity_avg, swing_percent, whiff_percent, pull_percent:opposite_percent, flyballs_percent, linedrives_percent, groundballs_percent,
         popups_percent, solidcontact_percent:hard_hit_percent, launch_angle_avg:barrel_batted_rate) %>% 
  scale(center = TRUE, scale = TRUE)

km_out_y <- kmeans(y, 7, nstart = 20)
km_out_y

fviz_cluster(km_out_y, data = y, stand = TRUE)

ds_5_cluster <- cbind(ds, km_out_y$cluster)
ds_5_cluster %>% 
  select(`km_out_y$cluster`, last_name, first_name, b_k_percent:b_total_bases) %>% 
  arrange(`km_out_y$cluster`, desc(on_base_plus_slg))

ds_5_cluster %>% 
  filter(last_name == "Fletcher")


# Making a table for cluster outputs to be able to easily read them
# This needs some extra work for getting the conditional formatting worked in

# Excel-inspired 3-color scale
GnYlRd <- function(x) rgb(colorRamp(c("#63be7b", "#ffeb84", "#f8696b"))(x), maxColorValue = 255)

reactable(t(round(km_out_y$centers,3)), showSortIcon = TRUE, defaultPageSize = 19)

dimnames <- list(start(nottem)[1]:end(nottem)[1], month.abb)
temps <- matrix(nottem, ncol = 12, byrow = TRUE, dimnames = dimnames)

# Excel-inspired 3-color scale
GnYlRd <- function(x) rgb(colorRamp(c("#63be7b", "#ffeb84", "#f8696b"))(x), maxColorValue = 255)

reactable(
  temps,
  defaultColDef = colDef(
    style = function(value) {
      if (!is.numeric(value)) return()
      normalized <- (value - min(nottem)) / (max(nottem) - min(nottem))
      color <- GnYlRd(normalized)
      list(background = color)
    },
    format = colFormat(digits = 1),
    minWidth = 50
  ),
  columns = list(
    .rownames = colDef(name = "Year", sortable = TRUE, align = "left")
  ),
  bordered = TRUE
)

# Hierarchical Clustering -------------------------------------------------

# David Fletcher makes this not work. He is a singleton when there are very few clusters (~5 in complete linkage). He separates out nearly
# immediately in hierarchical clustering methods other than complete linkage.
# He is a contact hitter with no power. He almost never strikes out of walks, swings roughly once an at bat, puts the ball in play with weak contact
# (2.5 sd above average for flares / burners)
# When removed, the clusters make more sense but I will not remove him because, although he is an outlier, there is nothing "wrong" about his data
hc.complete <- hclust(dist(y), method = "complete")
plot(hc.complete, main = "Complete Linkage", xlab = "", sub = "", cex = 0.9)
