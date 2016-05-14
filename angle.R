library(lattice)
library(ggplot2)
library(dplyr)
library('png')

shots = read.csv("data/data.csv", stringsAsFactors = T)

train = shots[!is.na(shots$shot_made_flag),]
test = shots[is.na(shots$shot_made_flag),]

train$angle <- NA
train$angle = apply(train[,c('loc_x', 'loc_y')], 1, function(vector) {
  atan2(abs(vector[2]), abs(vector[1])) * 180 / pi
})

train$angle[train$loc_x < 0 & train$loc_y < 0] = 360 - train$angle[train$loc_x < 0 & train$loc_y < 0]
train$angle[train$loc_x > 0 & train$loc_y > 0] = 180 - train$angle[train$loc_x > 0 & train$loc_y > 0]
train$angle[train$loc_x > 0 & train$loc_y < 0] = 180 + train$angle[train$loc_x > 0 & train$loc_y < 0]


boxplot_angle = boxplot(train$angle~train$shot_made_flag, varwidth = T, col = "bisque", staplewex = 1)

par(mfrow=c(2,1))
hist(train[train$shot_made_flag == 0,]$angle, col = "red", labels = TRUE, ylim = c(0, 3600))
hist(train[train$shot_made_flag == 1,]$angle, col = "green", labels = TRUE, ylim = c(0, 3600))

######################"

train$absolute_angle <- NA
train$absolute_angle = apply(train[,c('loc_x', 'loc_y')], 1, function(vector) {
  atan2(abs(vector[2]), abs(vector[1])) * 180 / pi
})
par(mfrow=c(2,1))
hist(train[train$shot_made_flag == 0,]$absolute_angle, col = "red", labels = TRUE, ylim = c(0, 3600))
hist(train[train$shot_made_flag == 1,]$absolute_angle, col = "green", labels = TRUE, ylim = c(0, 3600))
