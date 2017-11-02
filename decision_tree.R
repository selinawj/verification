library(tree)
library(rpart)
library(rpart.plot)
library(caret)

prepost[prepost=="NULL"] <- NA
prepost[prepost==""] <- NA

#merge data from poster, carrier, location and price tables
data <- subset(prepost, select=c(1, 4, 6, 13))
data <- merge(data, locationTable, by.x="account_id", by.y="account_id")

#select only the subset of data required
data <- subset(data, select=c(2,3,4,9,10))
data = data[,c("status", "poster", "carrier", "price", "dist")]
bad <- is.na(data$price)
data <- data[!bad,]
data$price <- as.numeric(data$price)

tot_count <- function(x, labs, digits, varlen)
{
  paste(labs, "\n\nn =", x$frame$n)
}

set.seed(123)
tree <- rpart(status~poster+carrier+price+dist, data = data, control = rpart.control(cp = 0.0001))
printcp(tree)
plot(tree)
text(tree, xpd=TRUE)
prp(tree, faclen = 0, cex = 0.8, node.fun=tot_count)
confusionMatrix(data$status, predict(tree, type="class"))

bestcp <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
tree.pruned <- prune(tree, cp=bestcp)
confusionMatrix(data$status, predict(tree.pruned, type="class"))
plot(tree.pruned)
text(tree.pruned, xpd=TRUE)
prp(tree.pruned, faclen = 0, cex = 0.8, node.fun=tot_count)
