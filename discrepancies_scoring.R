#create new dataframe to store similarity in prices
priceTable = data.frame(prepost$account_id, prepost$status, prepost$price)
priceTable[c("bucket")] = ""
colnames(priceTable) = c("id", "status", "price", "bucket")

priceTable$price = as.numeric(as.character(priceTable$price))

#plot
hist(priceTable$price, las = 1, breaks = 7, xlab = "Price ($)", ylab = "Freq", main="Histogram of Prices of Listings", lwd = 2, col="pink")
plot(priceTable$status, priceTable$price, xlab = "Status", ylab = "Price ($)", main="Price Data", col="turquoise")

#min
min(priceTable[,3], na.rm=T)
#max
max(priceTable[,3], na.rm=T)
#quantiles
summary(priceTable[,3])
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0    1500    2200    2349    2950   18000       2 

#create buckets
quantOne = 1500
#1500 to 2200
quantTwo = 2200
#2200 to 2950
quantThree = 2950
#2950 to 18000

#allocate buckets according to quantiles
row = 1
while (row <= nrow(priceTable)){
  if (!is.na(priceTable[row,3])){
    if (priceTable[row,3] <= quantOne){
      priceTable[row,4] = 1
    } else if (priceTable[row,3] > quantOne & priceTable[row,3] <= quantTwo){
      priceTable[row,4] = 2
    } else if (priceTable[row,3] > quantTwo & priceTable[row,3] <= quantThree){
      priceTable[row,4] = 3
    } else if (priceTable[row,3] > quantThree){
      priceTable[row,4] = 4
    }
  }
  row = row + 1
}

##GG PLOTS
library(ggplot2)

#data frame for plotting quantile
priceScammersTable = data.frame(table(priceTable$bucket, priceTable$status))
colnames(priceScammersTable) = c('quantile', 'status', 'count')

priceScammersTable[priceScammersTable==""] <- NA
priceScammersTable <- na.omit(priceScammersTable)

#ggplot of price scammers by quantiles
priceScammersPlot = ggplot(priceScammersTable, aes(x = quantile, y = count, fill = status)) + geom_bar(stat = "identity") + geom_text(aes(x = reorder(quantile, -count), y = count, label = count, group = status), position = position_dodge(width = 1), vjust = -0.5)
priceScammersPlot + labs(title = "Scammers based on Price Quantile", y = "Count", x = "Quantile", fill = "Status")

#quantile count dataframe
priceQuant = data.frame(table(priceTable$bucket, locationTable$status))

library(reshape2)

#final table
priceQuant = dcast(priceQuant, priceTable$bucket~priceTable$status)
colnames(priceQuant) = c("quantile", "nonscammers", "scammers")
priceQuant[priceQuant==""] <- NA
priceQuant <- na.omit(priceQuant)
priceQuant[c("total", "percentage")] = NA
priceQuant$total = with(priceQuant, scammers+nonscammers)
priceQuant$percentage = with(priceQuant, scammers/total*100)