library(ggplot2)
library(dplyr)
library('png')

shots = read.csv("data/data.csv", stringsAsFactors = T)

train = shots[!is.na(shots$shot_made_flag),]
test = shots[is.na(shots$shot_made_flag),]

mean_distance = tapply(train$shot_distance, train$shot_made_flag, mean)
sd_distance <- tapply(train$shot_distance, train$shot_made_flag, sd)

boxplot_distance = boxplot(train$shot_distance~train$shot_made_flag, varwidth = T, col = "bisque", staplewex = 1)

xi <- 0.3 + seq(boxplot_distance$n)
points(xi, mean_distance, col = "red", pch = 8)
arrows(xi, mean_distance - sd_distance, xi, mean_distance + sd_distance, code = 3, col = "red", angle = 75, length = .1)

train$shot_distance_type <- NA
train$shot_distance_type[train$shot_distance == 0] = "distance nulle"
train$shot_distance_type[train$shot_distance > 0 & train$shot_distance <= 8] = "proche panier"
train$shot_distance_type[train$shot_distance > 8 & train$shot_distance <= 16] = "mi distance"
train$shot_distance_type[train$shot_distance > 16 & train$shot_distance <= 22] = "avt 3 pts"
train$shot_distance_type[train$shot_distance > 22  & train$shot_distance <= 25] = "proche ligne 3pts"
train$shot_distance_type[train$shot_distance > 25 & train$shot_distance <= 43] = "entre ligne 3pts et ligne mi terrain"
train$shot_distance_type[train$shot_distance > 43] = "trop loin!"
train$shot_distance_type = as.factor(train$shot_distance_type)
summary(train)

courtplot <- function(feat) {
  feat <- substitute(feat)
  train %>% 
    ggplot(aes(x = loc_x, y = loc_y)) +
    geom_point(aes_q(color = feat), alpha = 0.7, size = 3) +
    theme_void() +
    ggtitle(paste(feat))
}
courtplot(shot_distance_type)
