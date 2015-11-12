rm(list=ls())

library(mlbench)
data(BostonHousing)

require(caret)
set.seed(1)
mdl <-  train(medv ~ ., data = BostonHousing, method = "rf", trControl = trainControl(method = "oob"))
ggplot(mdl)

sessionInfo()
