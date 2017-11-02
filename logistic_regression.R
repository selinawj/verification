library(ISLR)
library(caret)

prepost[prepost=="NULL"] <- NA
prepost[prepost==""] <- NA

library(Amelia)
missmap(prepost, main = "Missing values vs observed")
#license has too many missing values, we will not use it
#dropping account_id, username, longitude, latitude, posting_longitude, posting_latitude, citystate

#merge data from poster, carrier, location and price tables
data <- subset(prepost, select=c(1,2,4,6,8,9,10,11,13,14))
data <- merge(data, locationTable, by.x="account_id", by.y="account_id")
data <- merge(data, priceTable, by.x="account_id", by.y="account_id")
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

#select only the subset of data required
data <- subset(data, select=c(18,21,22,23,24))

#convert variables to numeric
data$posterPercent = as.numeric(data$posterPercent)
data$carrierPercent = as.numeric(data$carrierPercent)
data$locationPercent = as.numeric(data$locationPercent)
data$pricePercent = as.numeric(data$pricePercent)

#obtain correlation table
cor(data)

#split training and testing data
training = data[1:432,]
testing = data[433:618,]

training$status = as.numeric(training$status) - 1
testing$status = as.numeric(testing$status) - 1

#perform logistic regression
model <- glm(status ~ .,family=binomial(link='logit'),training)
summary(model)
anova(model, test="Chisq")

#calculate accuracy of prediction
fitted.results <- predict(model, newdata=testing, type='response')
fitted.results <- round(fitted.results)

#setting threshold
#fitted.results <- ifelse(fitted.results > 0.3,1,0)

#generate confusion matrix
confusionMatrix <- confusionMatrix(as.numeric(testing$status), fitted.results)
confusionMatrix
#           Reference
#Prediction   0   1
#         0 127  21
#         1  12  26

#plot logistic regression in R (posterPercent)
training = data[1:432,]
testing = data[433:618,]
plot(x=training$posterPercent, y=as.numeric(training$status)-1, col=c('blue','orange')[as.numeric(training$status)], xlab="Poster Percent", ylab="Probability of Scammer status", xlim=c(0,40))
model <- glm(status~posterPercent, data=training, family=binomial)
curve(predict(model,data.frame(posterPercent=x),type="resp"), add=TRUE)
points(training$posterPercent,fitted(model),pch=20,col=c("blue","orange")[as.numeric(training$status)])
summary(model)
fitted.results <- predict(model, newdata=testing, type='response')
fitted.results <- ifelse(fitted.results > 0.3,1,0)
testing$status = as.numeric(testing$status) - 1
confusionMatrix <- confusionMatrix(testing$status, fitted.results)
confusionMatrix

#plot logistic regression in R (carrierPercent)
training = data[1:432,]
testing = data[433:618,]
plot(x=training$carrierPercent, y=as.numeric(training$status)-1, col=c('blue','orange')[as.numeric(training$status)], xlab="Carrier Percent", ylab="Probability of Scammer status", xlim=c(0,70))
model <- glm(status~carrierPercent, data=training, family=binomial)
curve(predict(model,data.frame(carrierPercent=x),type="resp"), add=TRUE)
points(training$carrierPercent,fitted(model),pch=20,col=c("blue","orange")[as.numeric(training$status)])
summary(model)
fitted.results <- predict(model, newdata=testing, type='response')
fitted.results <- ifelse(fitted.results > 0.3,1,0)
testing$status = as.numeric(testing$status) - 1
confusionMatrix <- confusionMatrix(testing$status, fitted.results)
confusionMatrix

#plot logistic regression in R (locationPercent)
training = data[1:432,]
testing = data[433:618,]
plot(x=training$locationPercent, y=as.numeric(training$status)-1, col=c('blue','orange')[as.numeric(training$status)], xlab="Location Percent", ylab="Probability of Scammer status", xlim=c(0,50))
model <- glm(status~locationPercent, data=training, family=binomial)
curve(predict(model,data.frame(locationPercent=x),type="resp"), add=TRUE)
points(training$locationPercent,fitted(model),pch=20,col=c("blue","orange")[as.numeric(training$status)])
summary(model)
fitted.results <- predict(model, newdata=testing, type='response')
fitted.results <- ifelse(fitted.results > 0.3,1,0)
testing$status = as.numeric(testing$status) - 1
confusionMatrix <- confusionMatrix(testing$status, fitted.results)
confusionMatrix

#plot logistic regression in R (pricePercent)
training = data[1:432,]
testing = data[433:618,]
plot(x=training$pricePercent, y=as.numeric(training$status)-1, col=c('blue','orange')[as.numeric(training$status)], xlab="Price Percent", ylab="Probability of Scammer status", xlim=c(0,50))
model <- glm(status~pricePercent, data=training, family=binomial)
curve(predict(model,data.frame(pricePercent=x),type="resp"), add=TRUE)
points(training$pricePercent,fitted(model),pch=20,col=c("blue","orange")[as.numeric(training$status)])
summary(model)
fitted.results <- predict(model, newdata=testing, type='response')
fitted.results <- ifelse(fitted.results > 0.3,1,0)
testing$status = as.numeric(testing$status) - 1
confusionMatrix <- confusionMatrix(testing$status, fitted.results)
confusionMatrix

#box-plots for posterPercent, carrierPercent, locationPercent, pricePercent
training = data[1:432,]
testing = data[433:618,]
plot(training$status, training$posterPercent, xlab = "Status", ylab = "posterPercent", main="Poster Data", col="pink", ylim=c(0,40))
plot(training$status, training$carrierPercent, xlab = "Status", ylab = "carrierPercent", main="Carrier Data", col="pink")
plot(training$status, training$locationPercent, xlab = "Status", ylab = "locationPercent", main="Location Data", col="pink")
plot(training$status, training$pricePercent, xlab = "Status", ylab = "pricePercent", main="Price Data", col="pink")

#k-fold cross validation with all variables
library(rpart)
train_control <- trainControl(method="cv", number=20, savePredictions = TRUE)
model <- train(status~., data=training, method="glm", family="binomial", trControl=train_control)

prediction <- predict(model, testing)
confusionMatrix <- confusionMatrix(testing$status, prediction)
confusionMatrix
