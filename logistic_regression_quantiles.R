library(ISLR)
library(caret)

prepost[prepost=="NULL"] <- NA
prepost[prepost==""] <- NA

#merge data from poster, carrier, location and price tables
data <- subset(prepost, select=c(1,2,4,6,8,9,10,11,13,14))
data <- merge(data, locationTable, by.x="account_id", by.y="account_id")
data <- merge(data, priceTable, by.x="account_id", by.y="account_id")
data[c('posterPercent', 'carrierPercent')] <- ""
data$posterPercent[data$poster=="agent"]=27
data$posterPercent[data$poster=="landlord"]=31
data$posterPercent[data$poster=="other"]=26
data$posterPercent[data$poster=="renter"]=7
data$carrierPercent[data$carrier=="landline"]=35
data$carrierPercent[data$carrier=="mobile"]=14
data$carrierPercent[is.na(data$carrier)]=69
data$carrierPercent[data$carrier=="voip"]=67

#select only the subset of data required
data <- subset(data, select=c(17,18,20,21,22))
colnames(data) = c("locationBucket","status","priceBucket","posterPercent","carrierPercent")
data = data[,c("status", "posterPercent", "carrierPercent", "locationBucket", "priceBucket")]
#remove duplicated values

#convert variables to numeric
data$posterPercent = as.numeric(data$posterPercent)
data$carrierPercent = as.numeric(data$carrierPercent)
data$locationBucket = as.numeric(data$locationBucket)
data$priceBucket = as.numeric(data$priceBucket)

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

confusionMatrix <- confusionMatrix(as.numeric(testing$status), fitted.results)
confusionMatrix
