rm(list = ls())
library(mlbench)
data(BostonHousing)

library(doMC)
registerDoMC(2)

## NOTE: don't run models from RWeka when using
### multicore. The session will crash.

## The code for train() does not change:
require(caret)
set.seed(1)
usingMC <-  train(medv ~ ., data = BostonHousing, "bayesglm")

sessionInfo()
