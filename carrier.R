##GG PLOTS
library(ggplot2)

#uniformize from factors to characters format
i <- sapply(prepost, is.factor)
prepost[i] <- lapply(prepost[i], as.character)

#dataframe of how many each phone numbers are present
carrierTable = data.frame(table(prepost$carrier))
colnames(carrierTable) = c('type','count')

#uniformize from factors to characters format
i <- sapply(carrierTable, is.factor)
carrierTable[i] <- lapply(carrierTable[i], as.character)
carrierTable[1,1] = "NA"

#ggplot of total number of each carrier type
carrierPlot = ggplot(carrierTable, aes(x = reorder(type, -count), y = count)) + geom_bar(stat = "identity") + geom_text(aes(label = count), position = position_dodge(width = 1), vjust = -0.5)
carrierPlot + labs(title = "Number of each type of carrier", y = "Count", x = "Carrier")

#count how many scammers is present in each carrier
carrierScammers = data.frame(table(prepost$carrier, prepost$status=='scammer'))
colnames(carrierScammers) = c('carrier', 'status', 'count')

#uniformize from factors to characters format
i <- sapply(carrierScammers, is.factor)
carrierScammers[i] <- lapply(carrierScammers[i], as.character)
carrierScammers[1,1] = "NA"
carrierScammers[5,1] = "NA"

carrierScammers$status[carrierScammers$status == "FALSE"] <- "non-scammers"
carrierScammers$status[carrierScammers$status == "TRUE"] <- "scammers"

#plot of how many scammers in each phone category
carrierScammersPlot = ggplot(carrierScammers) + geom_bar(aes(x = reorder(carrier, -count), y = count, fill = status, group = status), stat = "identity", position = 'dodge') + geom_text(aes(x = reorder(carrier, -count), y = count, label = count, group = status), position = position_dodge(width = 1), vjust = -0.5)
carrierScammersPlot + labs(title = "Scammers based on Carrier Type", y = "Count", x = "Carrier Type", fill = "Status")

#plot of how many posters in each status
scammersPlot = ggplot(carrierScammers, aes(x = reorder(status, -count), y = count, fill = carrier)) + geom_bar(stat = "identity", position = 'dodge') + geom_text(aes(label = count), position = position_dodge(width = 1), vjust = -0.5)
scammersPlot + labs(title = "Num of Carrier Type within each scammer status", y = "Count", x = "Status", fill = "Carrier")

#merge table
carrierScammersTrue = carrierScammers[carrierScammers$status!="non-scammers",]
carrierScammersFalse = carrierScammers[carrierScammers$status!="scammers",]
finalCarrierTable = merge(carrierScammersTrue, carrierScammersFalse, by.x="carrier", by.y="carrier")
finalCarrierTable =finalCarrierTable[,-c(2,4)]
finalCarrierTable[c("total", "percentage")] = NA
colnames(finalCarrierTable) = c("carrier", "scammers", "nonscammers", "total", "scammersPercentage")
finalCarrierTable$total = with(finalCarrierTable, scammers+nonscammers)
finalCarrierTable$scammersPercentage = with(finalCarrierTable, scammers/total*100)