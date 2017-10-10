#create new dataframe to store similarity in location
locationTable = data.frame(prepost$account_id, prepost$latitude, prepost$longitude, prepost$postingLatitude, prepost$postingLongitude, prepost$status)
locationTable[c("dist", "bucket")] = ""
colnames(locationTable) = c("id", "listLat", "listLong", "postLat", "postLong", "status", "dist", "bucket")

#calculate dist(meters) from list to post
locationTable$listLat = as.numeric(as.character(locationTable$listLat))
locationTable$listLong = as.numeric(as.character(locationTable$listLong))
locationTable$postLat = as.numeric(locationTable$postLat)
locationTable$postLong = as.numeric(locationTable$postLong)

#calculate distance
library(geosphere)

row = 1
listLatCol = 2
listLongCol = 3
postLatCol = 4
postLongCol = 5
distCol = 7
while (row <= nrow(locationTable)){
  if ((!is.na(locationTable[row,listLongCol])) & (!is.na(locationTable[row,listLatCol]))) {
    if ((!is.na(locationTable[row,postLatCol])) & (!is.na(locationTable[row,postLongCol]))){
      locationTable[row, distCol] = distm(c(locationTable[row,listLongCol], locationTable[row,listLatCol]), 
               c(locationTable[row,postLongCol], locationTable[row,postLatCol]), fun = distHaversine)
    } else if ((is.na(locationTable[row,listLongCol])) & (is.na(locationTable[row,listLatCol]))) {
      if ((is.na(locationTable[row,postLatCol])) & (is.na(locationTable[row,postLongCol]))) {
        locationTable[row, distCol] = NA
      }
    }
  }
  row = row + 1
}

#convert dist col to numeric
locationTable = transform(locationTable, dist = as.numeric(dist))

#plot
hist(locationTable$dist, las = 1, breaks = 7, xlab = "Distance (m)", ylab = "Freq", main="Histogram of Posting vs Listing distances", lwd = 2, col="turquoise")
plot(locationTable$status, locationTable$dist, xlab = "Status", ylab = "Distance (m)", main="Location Data", col="pink")

#min
min(locationTable[,7], na.rm=T)
#max
max(locationTable[,7], na.rm=T)
#quantiles
summary(locationTable[,7])
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#218     2729    10470   759000   348200 12650000        2 

#create buckets
quantOne = 2729
#2729 to 10470
quantTwo = 10470
#10470 to 348200
quantThree = 348200
#348200 to 12650000

#allocate buckets according to quantiles
row = 1
while (row <= nrow(locationTable)){
  if (!is.na(locationTable[row,7])){
    if (locationTable[row,7] <= quantOne){
      locationTable[row,8] = 1
    } else if (locationTable[row,7] > quantOne & locationTable[row,7] <= quantTwo){
      locationTable[row,8] = 2
    } else if (locationTable[row,7] > quantTwo & locationTable[row,7] <= quantThree){
      locationTable[row,8] = 3
    } else if (locationTable[row,7] > quantThree){
      locationTable[row,8] = 4
    }
  }
  row = row + 1
}

##GG PLOTS
library(ggplot2)

#data frame for plotting quantile
locationScammersTable = data.frame(table(locationTable$bucket, locationTable$status))
colnames(locationScammersTable) = c('quantile', 'status', 'count')

locationScammersTable[locationScammersTable==""] <- NA
locationScammersTable <- na.omit(locationScammersTable)

#ggplot of location scammers by quantiles
locationScammersPlot = ggplot(locationScammersTable, aes(x = quantile, y = count, fill = status)) + geom_bar(stat = "identity") + geom_text(aes(x = reorder(quantile, -count), y = count, label = count, group = status), position = position_dodge(width = 1), vjust = -0.5)
locationScammersPlot + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Scammers based on Location Quantile", y = "Count", x = "Quantile", fill = "Status")

#quantile count dataframe
locationQuant = data.frame(table(locationTable$bucket, locationTable$status))

library(reshape2)

#final table
locationQuant = dcast(locationQuant, locationTable$bucket~locationTable$status)
colnames(locationQuant) = c("quantile", "nonscammers", "scammers")
locationQuant[locationQuant==""] <- NA
locationQuant <- na.omit(locationQuant)
locationQuant[c("total", "percentage")] = NA
locationQuant$total = with(locationQuant, scammers+nonscammers)
locationQuant$percentage = with(locationQuant, scammers/total*100)
locationQuant$percentage = with(locationQuant, scammers/total*100)