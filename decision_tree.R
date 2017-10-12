library(tree)

prepost[prepost=="NULL"] <- NA
prepost[prepost==""] <- NA

#merge data from poster, carrier, location and price tables
data <- subset(prepost, select=c(1,13))
data <- merge(data, locationTable, by.x="account_id", by.y="id")

#select only the subset of data required
data <- subset(data, select=c(2,7,8))

#construct decision tree
data$price = as.numeric(data$price)
bad <- is.na(data$price)
data <- data[!bad,]
model <- tree(status~as.numeric(price)+dist, data)
plot(model)
text(model)

#tree pruning
set.seed(1)
cv <- cv.tree(model, FUN=prune.misclass)
cv
plot(cv$size, cv$dev, type="b")
model1 <- prune.misclass(model, best=6)
plot(model1)
text(model1)
summary(model)
summary(model1)
#error classification rate higher for model1

plot(data$price, data$dist, pch=19, col=as.numeric(data$status))
partition.tree(model, label="Status", add=TRUE)
legend("topright", legend=unique(data$status), col=unique(as.numeric(data$status)), pch=19)
