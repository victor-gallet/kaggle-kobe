library(ggplot2)
library(dplyr)
library('png')

shots = read.csv("data/data.csv", stringsAsFactors = T)

train = shots[!is.na(shots$shot_made_flag),]
test = shots[is.na(shots$shot_made_flag),]

courtplot <- function(feat) {
  feat <- substitute(feat)
  train %>% 
    ggplot(aes(x = loc_x, y = loc_y)) +
    geom_point(aes_q(color = feat), alpha = 0.7, size = 3) +
    theme_void() +
    ggtitle(paste(feat))
}

train$shot_zone_detailed <- NA
train$shot_zone_detailed[train$loc_x <= -220 & train$loc_y <= 100 & train$shot_type == "3PT Field Goal"] = "1"
train$shot_zone_detailed[train$loc_x >= -220 & train$loc_x <= -150 & train$loc_y <= 100 & train$shot_type == "2PT Field Goal"] = "2"
train$shot_zone_detailed[train$loc_x < -90 & train$shot_type == "3PT Field Goal" & train$loc_y > 100] = "3"
train$shot_zone_detailed[train$loc_x < 90 & train$loc_x > -90 & train$shot_type == "3PT Field Goal"] = "4"

train$shot_zone_detailed[train$loc_x < 70 & train$loc_x > -70 & train$loc_y > 150 & train$shot_type == "2PT Field Goal"] = "6"
train$shot_zone_detailed[train$loc_x > 70 & train$shot_type == "3PT Field Goal"] = "7"

train$shot_zone_detailed[train$loc_x < 90 & train$loc_x > -90 & train$loc_y > 90 & train$loc_y < 150 & train$shot_type == "2PT Field Goal"] = "10"

train$shot_zone_detailed[sqrt(train$loc_x^2 + train$loc_y^2) < 90] = "12"
train$shot_zone_detailed[train$loc_x < 220 & train$loc_x > 150 & train$loc_y <= 100] = "13"
train$shot_zone_detailed[train$loc_x > 220 & train$loc_y < 100 & train$shot_type == "3PT Field Goal"] = "14"

train$shot_zone_detailed[is.na(train$shot_zone_detailed) & train$loc_y > 100 & train$loc_x > -210 & train$loc_x < 70] = "5"
train$shot_zone_detailed[is.na(train$shot_zone_detailed) & train$loc_y > 100 & train$loc_x >= 70 & train$loc_x < 210] = "8"

train$shot_zone_detailed[is.na(train$shot_zone_detailed) & train$loc_y <= 100 & train$loc_x < 0] = "9"
train$shot_zone_detailed[is.na(train$shot_zone_detailed) & train$loc_y <= 100 & train$loc_x > 0] = "11"


train$shot_zone_detailed = as.factor(train$shot_zone_detailed)
courtplot(shot_zone_detailed)
summary(train$shot_zone_detailed)

## Construction 
mean_x = aggregate(train$loc_x, list(train$shot_zone_detailed), na.rm = TRUE, mean)
mean_y = aggregate(train$loc_y, list(train$shot_zone_detailed), na.rm = TRUE, mean)
mean_xy = data.frame(mean_x$Group.1, mean_x$x, mean_y$x)

pourcentage_shot = as.data.frame(prop.table(table(train$shot_made_flag, train$shot_zone_detailed), 2))
shot_made_by_zone = as.data.frame(table(train$shot_made_flag, train$shot_zone_detailed))

plot(1, type="n", xlab="", ylab="", xlim=c(-235, 235), ylim=c(-30, 400))
lim <- par()
rasterImage(courtimg, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
grid()
for (zone in 1:14) {
  x = mean_xy[mean_xy$mean_x.Group.1 == zone,]$mean_x.x
  y = mean_xy[mean_xy$mean_x.Group.1 == zone,]$mean_y.x
  text_pourcentage = pourcentage_shot[pourcentage_shot$Var1 == 1 & pourcentage_shot$Var2 == zone,]$Freq
  success_shot = shot_made_by_zone[shot_made_by_zone$Var2 == zone & shot_made_by_zone$Var1 == 1,]$Freq
  missed_shot = shot_made_by_zone[shot_made_by_zone$Var2 == zone & shot_made_by_zone$Var1 == 0,]$Freq
  text(x, y, sprintf("%d%% \n %d / %d", round(text_pourcentage * 100), success_shot, success_shot + missed_shot))
}