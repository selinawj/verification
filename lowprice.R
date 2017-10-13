lowPrice = data.frame(matrix(nrow= 4, ncol=3))
colnames(lowPrice) = c("price", "status", "count")
#low prices constitutes 0 or 1 prices
lowPrice[c(1,3),1] = "low"
lowPrice[c(2,4),1] = "normal"
lowPrice[c(1,2),2] = "scammer"
lowPrice[c(3,4),2] = "nonscammer"
lowPrice[1,3] = sum(prepost$price=="0" & prepost$status=="scammer") + sum(prepost$price=="1" & prepost$status=="scammer")
lowPrice[2,3] = sum(prepost$price!="0" & prepost$price!="1" & prepost$status=="scammer")
lowPrice[3,3] = sum(prepost$price=="0" & prepost$status=="non-scammer") + sum(prepost$price=="1" & prepost$status=="non-scammer")
lowPrice[4,3] = sum(prepost$price!="0" & prepost$price!="1" & prepost$status=="non-scammer")

#plot of how many scammers in each price type
lowPriceScammersPlot = ggplot(lowPrice) + geom_bar(aes(x = reorder(price, -count), y = count, fill = status, group = status), stat = "identity", position = 'dodge') + geom_text(aes(x = reorder(price, -count), y = count, label = count, group = status), position = position_dodge(width = 1), vjust = -0.5)
lowPriceScammersPlot + labs(title = "Scammers based on Price Type", y = "Count", x = "Price Type", fill = "Status")

#plot of how many of price type in each status
scammersPlot = ggplot(lowPrice, aes(x = reorder(status, -count), y = count, fill = price)) + geom_bar(stat = "identity", position = 'dodge') + geom_text(aes(label = count), position = position_dodge(width = 1), vjust = -0.5)
scammersPlot + labs(title = "Num of price type of each scammer status", y = "Count", x = "Status", fill = "Price")

#final table
finalLowPriceTable = data.frame(matrix(nrow=2, ncol=5))
colnames(finalLowPriceTable) = c("price", "scammers", "nonscammers", "total", "scammersPercentage")
finalLowPriceTable[1,1] = "low"
finalLowPriceTable[2,1] = "normal"
finalLowPriceTable[1,2] = lowPrice[1,3]
finalLowPriceTable[1,3] = lowPrice[3,3]
finalLowPriceTable[2,2] = lowPrice[2,3]
finalLowPriceTable[2,3] = lowPrice[4,3]
finalLowPriceTable$total = with(finalLowPriceTable, scammers+nonscammers)
finalLowPriceTable$scammersPercentage = with(finalLowPriceTable, scammers/total*100)