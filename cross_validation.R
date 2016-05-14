library(ggplot2)
library(dplyr)
library('png')
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm

library("rattle")
library("rpart.plot")
library("RColorBrewer")
library('rpart')

library('party')
library('mice')


shots = read.csv("data/data.csv", stringsAsFactors = F)

shots$shot_made_flag <- as.factor(shots$shot_made_flag)

## Zone detailed
shots$shot_zone_detailed <- NA
shots$shot_zone_detailed[shots$loc_x <= -220 & shots$loc_y <= 100 & shots$shot_type == "3PT Field Goal"] = "1"
shots$shot_zone_detailed[shots$loc_x >= -220 & shots$loc_x <= -150 & shots$loc_y <= 100 & shots$shot_type == "2PT Field Goal"] = "2"
shots$shot_zone_detailed[shots$loc_x < -90 & shots$shot_type == "3PT Field Goal" & shots$loc_y > 100] = "3"
shots$shot_zone_detailed[shots$loc_x < 90 & shots$loc_x > -90 & shots$shot_type == "3PT Field Goal"] = "4"

shots$shot_zone_detailed[shots$loc_x < 70 & shots$loc_x > -70 & shots$loc_y > 150 & shots$shot_type == "2PT Field Goal"] = "6"
shots$shot_zone_detailed[shots$loc_x > 70 & shots$shot_type == "3PT Field Goal"] = "7"

shots$shot_zone_detailed[shots$loc_x < 90 & shots$loc_x > -90 & shots$loc_y > 90 & shots$loc_y < 150 & shots$shot_type == "2PT Field Goal"] = "10"

shots$shot_zone_detailed[sqrt(shots$loc_x^2 + shots$loc_y^2) < 90] = "12"
shots$shot_zone_detailed[shots$loc_x < 220 & shots$loc_x > 150 & shots$loc_y <= 100] = "13"
shots$shot_zone_detailed[shots$loc_x > 220 & shots$loc_y < 100 & shots$shot_type == "3PT Field Goal"] = "14"

shots$shot_zone_detailed[is.na(shots$shot_zone_detailed) & shots$loc_y > 100 & shots$loc_x > -210 & shots$loc_x < 70] = "5"
shots$shot_zone_detailed[is.na(shots$shot_zone_detailed) & shots$loc_y > 100 & shots$loc_x >= 70 & shots$loc_x < 210] = "8"

shots$shot_zone_detailed[is.na(shots$shot_zone_detailed) & shots$loc_y <= 100 & shots$loc_x < 0] = "9"
shots$shot_zone_detailed[is.na(shots$shot_zone_detailed) & shots$loc_y <= 100 & shots$loc_x > 0] = "11"
shots$shot_zone_detailed = as.factor(shots$shot_zone_detailed)


## Distance
shots$shot_distance_type <- NA
shots$shot_distance_type[shots$shot_distance == 0] = "distance nulle"
shots$shot_distance_type[shots$shot_distance > 0 & shots$shot_distance <= 8] = "proche panier"
shots$shot_distance_type[shots$shot_distance > 8 & shots$shot_distance <= 16] = "mi distance"
shots$shot_distance_type[shots$shot_distance > 16 & shots$shot_distance <= 22] = "avt 3 pts"
shots$shot_distance_type[shots$shot_distance > 22  & shots$shot_distance <= 25] = "proche ligne 3pts"
shots$shot_distance_type[shots$shot_distance > 25 & shots$shot_distance <= 43] = "entre ligne 3pts et ligne mi terrain"
shots$shot_distance_type[shots$shot_distance > 43] = "trop loin!"
shots$shot_distance_type = as.factor(shots$shot_distance_type)

## Angle
shots$angle <- NA
shots$angle = apply(shots[,c('loc_x', 'loc_y')], 1, function(vector) {
  atan2(abs(vector[2]), abs(vector[1])) * 180 / pi
})

shots$angle[shots$loc_x < 0 & shots$loc_y < 0] = 360 - shots$angle[shots$loc_x < 0 & shots$loc_y < 0]
shots$angle[shots$loc_x > 0 & shots$loc_y > 0] = 180 - shots$angle[shots$loc_x > 0 & shots$loc_y > 0]
shots$angle[shots$loc_x > 0 & shots$loc_y < 0] = 180 + shots$angle[shots$loc_x > 0 & shots$loc_y < 0]

shots$absolute_angle <- NA
shots$absolute_angle = apply(shots[,c('loc_x', 'loc_y')], 1, function(vector) {
  atan2(abs(vector[2]), abs(vector[1])) * 180 / pi
})


## Action type
action_table = table(shots$action_type)
df = as.data.frame(action_table)
rare_action_title = df[df$Freq < 10,]$Var1

shots$shot_action_type_simplified = shots$action_type
shots$shot_action_type_simplified[shots$shot_action_type_simplified %in% rare_action_title]  <- 'Rare action'



############################

## Crosss Validation

train = shots[!is.na(shots$shot_made_flag),]
test = shots[is.na(shots$shot_made_flag),]

### Shuffled the data 
train_new = train
n <- nrow(train_new)
shuffled <- train_new[sample(n), ]

# Set random seed. Don't remove this line.
set.seed(1)

# Initialize the accs vector
accs = array(1:6, 0)
accs

for (i in 1:6) {
  # These indices indicate the interval of the test set
  indices <- (((i-1) * round((1/6)*nrow(shuffled))) + 1):((i*round((1/6) * nrow(shuffled))))
  
  # Exclude them from the train set
  train_train <- shuffled[-indices,]
  
  # Include them in the test set
  train_test <- shuffled[indices,]
  
  # A model is learned using each training set
  rpart_tree = rpart(shot_made_flag ~ shot_distance_type + shot_zone_detailed + action_type + combined_shot_type
     + period + angle + absolute_angle + shot_type + shot_zone_area + shot_zone_basic + shot_zone_range + opponent,
                     data = train_train, method = "class", control = rpart.control(cp = 0, minsplit = 5))
  
  # Make a prediction on the test set using tree
  prediction = predict(rpart_tree, train_test, type = "class")
  
  # Assign the confusion matrix to conf
  conf = table(train_test$shot_made_flag, prediction)
  
  # Assign the accuracy of this model to the ith index in accs
  accs[i] = sum(diag(conf)) / sum(conf)
}

# Print out the mean of accs
mean(accs)

## RandomForest
train_new = train
n <- nrow(train_new)
shuffled <- train_new[sample(n), ]

# Set random seed. Don't remove this line.
set.seed(1)

# Initialize the accs vector
accs = array(1:6, 0)
accs

for (i in 1:6) {
  # These indices indicate the interval of the test set
  indices <- (((i-1) * round((1/6)*nrow(shuffled))) + 1):((i*round((1/6) * nrow(shuffled))))
  
  # Exclude them from the train set
  train_train <- shuffled[-indices,]
  
  # Include them in the test set
  train_test <- shuffled[indices,]
  
  # A model is learned using each training set
  rpart_tree = randomForest(shot_made_flag ~ shot_distance_type + opponent + angle + absolute_angle + shot_zone_range + shot_zone_basic 
  + period + shot_type + shot_zone_area + shot_zone_detailed + combined_shot_type,
                     train_train,  importance = TRUE)
  
  # Make a prediction on the test set using tree
  prediction = predict(rpart_tree, train_test)
  
  # Assign the confusion matrix to conf
  conf = table(train_test$shot_made_flag, prediction)
  
  # Assign the accuracy of this model to the ith index in accs
  accs[i] = sum(diag(conf)) / sum(conf)
}


## Cforest
train_new = train
n <- nrow(train_new)
shuffled <- train_new[sample(n), ]

# Set random seed. Don't remove this line.
set.seed(1)

# Initialize the accs vector
accs = array(1:6, 0)
accs

for (i in 1:1) {
  # These indices indicate the interval of the test set
  indices <- (((i-1) * round((1/6)*nrow(shuffled))) + 1):((i*round((1/6) * nrow(shuffled))))
  
  # Exclude them from the train set
  train_train <- shuffled[-indices,]
  
  # Include them in the test set
  train_test <- shuffled[indices,]
  
  # A model is learned using each training set
  rpart_tree = cforest(shot_made_flag ~ shot_distance_type + opponent + angle + absolute_angle + shot_zone_range + shot_zone_basic 
                            + period + shot_type + shot_zone_area + shot_zone_detailed + combined_shot_type,
                            train_train)
  
  # Make a prediction on the test set using tree
  prediction = predict(rpart_tree, train_test, OOB=TRUE, type = "response")
  
  # Assign the confusion matrix to conf
  conf = table(train_test$shot_made_flag, prediction)
  
  # Assign the accuracy of this model to the ith index in accs
  accs[i] = sum(diag(conf)) / sum(conf)
}
