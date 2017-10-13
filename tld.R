##GG PLOTS
library(ggplot2)

#dataframe of how many each tld is present (2 of them NAs)
tldTable = data.frame(table(prepost$tld))
colnames(tldTable) = c('tld', 'count')

#ggplot of total number of each tld type by descending order
tldPlot = ggplot(tldTable, aes(x = reorder(tld, -count), y = count)) + geom_bar(stat = "identity") + geom_text(aes(x = tld, y = count, label = count), position = position_dodge(width = 1), vjust = -0.5)
tldPlot

#dataframe of how many scammers is present in each domain
tldScammersTable = data.frame(table(prepost$tld, prepost$status))
colnames(tldScammersTable) = c('tld', 'status', 'count')

#ggplot of TLD scammers
tldScammersPlot = ggplot(tldScammersTable, aes(x = reorder(tld, -count), y = count, fill = status)) + geom_bar(stat = "identity") + geom_text(aes(x = tld, y = count, label = count, group = status), position = position_dodge(width = 1), vjust = -0.5)
tldScammersPlot + labs(title = "Scammers based on TLD", y = "Count", x = "TLD", fill = "Status")

#count how many scammers is present in each tld
tldScammers = data.frame(table(prepost$tld, prepost$status=='scammer'))
colnames(tldScammers) = c('tld', 'scammer_status', 'count')

#merge table
tldScammersTrue = tldScammers[tldScammers$scammer_status!="FALSE",]
tldScammersFalse = tldScammers[tldScammers$scammer_status!="TRUE",]
finalTldTable = merge(tldScammersTrue, tldScammersFalse, by.x="tld", by.y="tld")
finalTldTable = finalTldTable[,-c(2,4)]
finalTldTable[c("total", "percentage")] = NA
colnames(finalTldTable) = c("tld", "scammers", "nonscammers", "total", "scammersPercentage")
finalTldTable$total = with(finalTldTable, scammers+nonscammers)
finalTldTable$scammersPercentage = with(finalTldTable, scammers/total*100)