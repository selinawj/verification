library(tree)
library(rpart)
library(rpart.plot)
library(caret)

prepost[prepost=="NULL"] <- NA
prepost[prepost==""] <- NA

#merge data from poster, carrier, location and price tables
data <- subset(prepost, select=c(1,4,6))
data <- merge(data, locationTable, by.x="account_id", by.y="account_id")
data <- merge(data, priceTable, by.x="account_id", by.y="account_id")

#select only the subset of data required
data <- subset(data, select=c(2,3,10,13))
colnames(data) <- c('status','carrier','locationBucket','priceBucket')

bad <- is.na(data$price)
data <- data[!bad,]
data$locationBucket <- as.numeric(data$locationBucket)
data$priceBucket <- as.numeric(data$priceBucket)

tot_count <- function(x, labs, digits, varlen)
{
  paste(labs, "\n\nn =", x$frame$n)
}

set.seed(123)

#separate training & testing data
training = data[1:432,]
testing = data[433:618,]

#construct decision tree
training$priceBucket = as.numeric(training$priceBucket)
training$locationBucket = as.numeric(training$locationBucket)
testing$priceBucket = as.numeric(testing$priceBucket)
testing$locationBucket = as.numeric(testing$locationBucket)
bad <- is.na(training$priceBucket)
training <- training[!bad,]
model <- tree(status~locationBucket+priceBucket, training, method="class")
plot(model)
text(model)
predicted=predict(model, testing, type="class")
confusionMatrix(testing$status, predicted)
#Prediction    non-scammer scammer
#non-scammer         141       7
#scammer              30       8

#plot
plot(data$price, data$dist, pch=19, col=as.numeric(data$status))
partition.tree(model, label="Status", add=TRUE)
legend("topright", legend=unique(data$status), col=unique(as.numeric(data$status)), pch=19)
