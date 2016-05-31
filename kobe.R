library(ggplot2)
library(dplyr)
library('png')

shots = read.csv("data/data.csv", stringsAsFactors = T)

shots$shot_made_flag <- as.factor(shots$shot_made_flag)

courtimg = readPNG('court.png')

train = shots[!is.na(shots$shot_made_flag),]
test = shots[is.na(shots$shot_made_flag),]

season <- aggregate(train$shot_made_flag, list(train$season), na.rm = TRUE, mean)

courtplot <- function(feat) {
  feat <- substitute(feat)
  train %>% 
    ggplot(aes(x = loc_x, y = loc_y)) +
    geom_point(aes_q(color = feat), alpha = 0.7, size = 3) +
    theme_void() +
    ggtitle(paste(feat))
}


#Set up the plot area
plot(1, type="n", xlab="", ylab="", xlim=c(-235, 235), ylim=c(-30, 400))

#Get the plot information so the image will fill the plot box, and draw it
lim <- par()
rasterImage(courtimg, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
grid()
lines(train$loc_x, train$loc_y, type="p", col="")


plot_kobe <- function(data, cat, alpha = 1){
  p <- ggplot(data, aes(data[, loc_x], data[, loc_y], color = data[[cat]]))+
    annotation_custom(courtimg, -300, 300, -115, 900)+
    geom_point(alpha = alpha)+
    theme_bw()+
    ylim(-80, 400)+
    xlim(-280, 280)+
    xlab('X')+
    ylab('Y')+
    scale_color_manual('Shots', values = c('#e61919', '#009999'))
  return(p)
}


waggplot(season, aes(x=Group.1, y = x)) + geom_bar(stat="identity")  + ylim(0,.5) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Season") + ylab("Shooting Percentage") + geom_hline(yintercept = mean(season$x))



train$shot_zone_detailed <- NA
train$shot_zone_detailed[train$loc_x <= -220 & train$loc_y <= 100 & train$shot_type == "3PT Field Goal"] = "A"
train$shot_zone_detailed[train$loc_x >= -220 & train$loc_x <= -150 & train$loc_y <= 100 & train$shot_type == "2PT Field Goal"] = "B"
train$shot_zone_detailed[train$loc_x < -90 & train$shot_type == "3PT Field Goal" & train$loc_y > 100] = "C"
train$shot_zone_detailed[train$loc_x < 90 & train$loc_x > -90 & train$shot_type == "3PT Field Goal"] = "D"

train$shot_zone_detailed[train$loc_x < 70 & train$loc_x > -70 & train$loc_y > 150 & train$shot_type == "2PT Field Goal"] = "F"
train$shot_zone_detailed[train$loc_x > 70 & train$shot_type == "3PT Field Goal"] = "G"

train$shot_zone_detailed[train$loc_x < 90 & train$loc_x > -90 & train$loc_y > 90 & train$loc_y < 150 & train$shot_type == "2PT Field Goal"] = "J"

train$shot_zone_detailed[sqrt(train$loc_x^2 + train$loc_y^2) < 90] = "L"
train$shot_zone_detailed[train$loc_x < 220 & train$loc_x > 150 & train$loc_y <= 100] = "M"
train$shot_zone_detailed[train$loc_x > 220 & train$loc_y < 100 & train$shot_type == "3PT Field Goal"] = "N"

train$shot_zone_detailed[is.na(train$shot_zone_detailed) & train$loc_y > 100 & train$loc_x > -210 & train$loc_x < 70] = "E"
train$shot_zone_detailed[is.na(train$shot_zone_detailed) & train$loc_y > 100 & train$loc_x >= 70 & train$loc_x < 210] = "H"

train$shot_zone_detailed[is.na(train$shot_zone_detailed) & train$loc_y <= 100 & train$loc_x < 0] = "I"
train$shot_zone_detailed[is.na(train$shot_zone_detailed) & train$loc_y <= 100 & train$loc_x > 0] = "K"


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
for (zone in c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N")) {
  x = mean_xy[mean_xy$mean_x.Group.1 == zone,]$mean_x.x
  y = mean_xy[mean_xy$mean_x.Group.1 == zone,]$mean_y.x
  text_pourcentage = pourcentage_shot[pourcentage_shot$Var1 == 1 & pourcentage_shot$Var2 == zone,]$Freq
  success_shot = shot_made_by_zone[shot_made_by_zone$Var2 == zone & shot_made_by_zone$Var1 == 1,]$Freq
  missed_shot = shot_made_by_zone[shot_made_by_zone$Var2 == zone & shot_made_by_zone$Var1 == 0,]$Freq
  text(x, y, sprintf("%d%% \n %d / %d", round(text_pourcentage * 100), success_shot, success_shot + missed_shot))
}




