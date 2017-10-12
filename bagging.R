library(ipred)
library(rpart)
library(randomForest)

pred.rpart <- function(object, newdata) {
  predict(object, newdata = newdata, type = "class")
}

set.seed(1)
c(Tree = errorest(status ~ ., data=data, model = rpart, predict=pred.rpart)$error,
  Bagging = errorest(status ~ ., data=data, model = bagging)$error,
  Forest = errorest(status ~ ., data=data, model = randomForest)$error)

rf1 <- randomForest(status~., data=data, importance=TRUE)
importance(rf1)
barplot(rf1$importance[,3], main="Importance (Dec Accuracy)")
barplot(rf1$importance[,4], main="Importance (Gini Index)")