library("ISLR")

prepost[prepost=="NULL"] <- NA
prepost[prepost==""] <- NA

library(Amelia)
missmap(prepost, main = "Missing values vs observed")
#license has too many missing values, we will not use it
#dropping account_id, username, longitude, latitude, posting_longitude, posting_latitude, citystate

#merging data from poster, carrier, location and price tables
data <- subset(prepost, select=c(1,2,4,6,8,9,10,11,13,14))
data <- merge(data, locationTable, by.x="account_id", by.y="id")
data <- merge(data, priceTable, by.x="account_id", by.y="id")
data[c('posterPercent', 'carrierPercent', 'locationPercent', 'pricePercent')] <- ""
data$posterPercent[data$poster=="agent"]=27
data$posterPercent[data$poster=="landlord"]=31
data$posterPercent[data$poster=="other"]=26
data$posterPercent[data$poster=="renter"]=7
data$carrierPercent[data$carrier=="landline"]=35
data$carrierPercent[data$carrier=="mobile"]=14
data$carrierPercent[is.na(data$carrier)]=69
data$carrierPercent[data$carrier=="voip"]=67
data$locationPercent[data$bucket==1]=10
data$locationPercent[data$bucket==""]=10
data$locationPercent[data$bucket==2]=15
data$locationPercent[data$bucket==3]=23
data$locationPercent[data$bucket==4]=41
data$pricePercent[data$priceBucket==1]=36
data$pricePercent[data$priceBucket==""]=36
data$pricePercent[data$priceBucket==2]=23
data$pricePercent[data$priceBucket==3]=17
data$pricePercent[data$priceBucket==4]=12

#selecting only the subset of data required
data <- subset(data, select=c(18,21,22,23,24))

#convert variables to numeric
data$posterPercent = as.numeric(data$posterPercent)
data$carrierPercent = as.numeric(data$carrierPercent)
data$locationPercent = as.numeric(data$locationPercent)
data$pricePercent = as.numeric(data$pricePercent)

#splittling training and testing data
training = data[1:432,]
testing = data[433:618,]

#perform logistic regression
model <- glm(status ~ .,family=binomial(link='logit'),training)
summary(model)
anova(model, test="Chisq")

#calculate accuracy of prediction
fitted.results <- predict(model, newdata=testing, type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,2)
fitted.results
misClasificError <- mean(fitted.results != as.numeric(testing$status))
print(paste('Accuracy', 1-misClasificError))

#generate confusion matrix
library(RTextTools)
confusion_matrix = table(fitted.results, as.numeric(testing$status))
confusion_matrix
#fitted.results   1   2
#             1   7  25
#             2 141  13

#obtain a ROCR curve plot and AUC value
library(ROCR)
p <- predict(model,newdata=testing, type="response")
pr <- prediction(p, as.numeric(testing$status))
prf <- performance(pr, measure="tpr", x.measure="fpr")
plot(prf)
auc <- performance(pr, measure="auc")
auc <- auc@y.values[[1]]
auc

#k-fold cross validation
library(caret)
library(rpart)
train_control <- trainControl(method="cv", number=10, savePredictions = TRUE)
model <- train(status~., data=data, trControl=train_control, method="rpart")
print(model)

predictions <- predict(model, data)
data <- cbind(data, predictions)
confusionMatrix <- confusionMatrix(data$predictions,data$status)
confusionMatrix
model$pred