require(xgboost)
## Loading required package: xgboost
require(methods)
## Loading required package: methods
require(data.table)
## Loading required package: data.table
require(magrittr)

library(Ckmeans.1d.dp)

shots = read.csv("data/data.csv", stringsAsFactors = T)
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

### Time
shots$time <- NA
shots$time = apply(shots[,c('period', 'minutes_remaining', 'seconds_remaining')], 1, function(vector) {
  (vector[1] * vector[2]) * 60 + vector[3]
})


shots_numeric = shots


shots_numeric$action_type = as.numeric(shots_numeric$action_type)
shots_numeric$combined_shot_type = as.numeric(shots_numeric$combined_shot_type)

shots_numeric$season = as.numeric(shots_numeric$season)
shots_numeric$shot_type = as.numeric(shots_numeric$shot_type)
shots_numeric$shot_zone_area = as.numeric(shots_numeric$shot_zone_area)
shots_numeric$shot_zone_range = as.numeric(shots_numeric$shot_zone_range)
shots_numeric$team_name = as.numeric(shots_numeric$team_name)
shots_numeric$game_date = as.numeric(shots_numeric$game_date)
shots_numeric$matchup = as.numeric(shots_numeric$matchup)
shots_numeric$opponent = as.numeric(shots_numeric$opponent)
shots_numeric$shot_distance_type = as.numeric(shots_numeric$shot_distance_type)



train = shots[!is.na(shots$shot_made_flag),]
test = shots[is.na(shots$shot_made_flag),]

train_matrix = data.matrix(train[,c(1:14, 16:30)])
test_matrix =  data.matrix(test[,c(1:14, 16:30)])

param <- list("objective" = "multi:softprob", "eval_metric" = "mlogloss", "num_class" = 2)

cv.nround <- 5
cv.nfold <- 3

bst.cv = xgb.cv(param=param, data = train_matrix, label = train$shot_made_flag, nfold = cv.nfold, nrounds = cv.nround)


########################

nround = 50
bst = xgboost(param=param, data = train_matrix, label = train$shot_made_flag, nrounds=nround, nfold = 300)

bst_importance = xgb.importance(names(train[,c(1:14, 16:30)]), model = bst)
xgb.plot.importance(bst_importance)




prediction = predict(bst, test_matrix)

prediction = matrix(predict(bst, test_matrix), ncol = 2, byrow = T)

my_solution <- data.frame(shot_id = test$shot_id, shot_made_flag = prediction[,2])

# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution, file = "my_solution.csv" , row.names = FALSE)












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
  train_matrix = data.matrix(train_train[,c(1:14, 16:29)])
  test_matrix =  data.matrix(train_test[,c(1:14, 16:29)])
  
  nround = 50
  bst = xgboost(param=param, data = train_matrix, label = train_train$shot_made_flag, nrounds=nround)
  
  # Make a prediction on the test set using tree
  prediction = predict(bst, test_matrix)
  
  # Assign the confusion matrix to conf
  conf = table(train_test$shot_made_flag, prediction)
  
  # Assign the accuracy of this model to the ith index in accs
  accs[i] = sum(diag(conf)) / sum(conf)
}


bst = xgboost(param=param, data = train_matrix, label = train_train$shot_made_flag, nrounds=nround)
